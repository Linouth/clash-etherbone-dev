{-# OPTIONS -fplugin=Protocols.Plugin #-}

module TestPacketStream where

import Debug.Trace
import Data.Bifunctor as B
import Data.Maybe

import Clash.Prelude

import Protocols
import Protocols.PacketStream
import Protocols.PacketStream.Base
import Protocols.Idle
-- import Protocols.Internal
-- import Protocols.Wishbone



printerC :: (KnownDomain dom, ShowX meta) => Circuit (PacketStream dom dataWidth meta) ()
printerC = fromSignals go
  where
    -- go (s,_) = {- trace (showX $ sample_lazy s) -} (pure (PacketStreamS2M True), ())
    go (s,_) = seq (printX $ sample_lazy s) (pure (PacketStreamS2M True), ())
    -- go (s,_) = seq (traceSignal1 "test" $ _data <$> (trac <$> s)) (pure (PacketStreamS2M True), ())
    --
    -- trac Nothing  = ()
    -- trac (Just s) = s




foo :: Circuit a () -> Fwd a -> Bwd a
foo c = go
  where
    go x = fst $ toSignals c (x, ())


tmp :: [Maybe (PacketStreamM2S 4 ())]
tmp = [Nothing, Just $ PacketStreamM2S (0:>0:>0:>0:>Nil) (Nothing) () False, Just $ PacketStreamM2S (1:>1:>1:>1:>Nil) (Just 3) () False]


config :: SimulationConfig
config = SimulationConfig 1 maxBound False

data EBHeader = EBHeader
  { _magic    :: Unsigned 16
  , _version  :: Unsigned 4
  , _res      :: Bit
  , _nr       :: Bit
  , _pr       :: Bit
  , _pf       :: Bit
  , _addrSize :: BitVector 4
  , _portSize :: BitVector 4
  } deriving (Generic, BitPack, NFDataX, Show, ShowX)

data RecordHeader = RecordHeader
  { _bca    :: Bit
  , _rca    :: Bit
  , _rff    :: Bit
  , _res0   :: Bit
  , _cyc    :: Bit
  , _wca    :: Bit
  , _wff    :: Bit
  , _res1   :: Bit
  , _byteEn :: BitVector 8
  , _wCount :: Unsigned 8
  , _rCount :: Unsigned 8
  } deriving (Generic, BitPack, NFDataX, Show, ShowX)


etherboneDepacketizerC
 :: (HiddenClockResetEnable dom, KnownNat dataWidth, 1 <= dataWidth)
 => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth EBHeader)
etherboneDepacketizerC = depacketizerC @_ metaMap
  where metaMap hdr _ = hdr


-- This adds a cycle latency!
etherbonePacketizerC
  :: (HiddenClockResetEnable dom, KnownNat dataWidth, 1 <= dataWidth)
  => Circuit (PacketStream dom dataWidth EBHeader) (PacketStream dom dataWidth ())
etherbonePacketizerC = packetizerC (const ()) id


-- We can drop EBHeader. All info in here is static and known
recordDepacketizerC
  :: (HiddenClockResetEnable dom, KnownNat dataWidth, 1 <= dataWidth)
 => Circuit (PacketStream dom dataWidth EBHeader) (PacketStream dom dataWidth RecordHeader)
recordDepacketizerC = depacketizerC metaMap
  where metaMap hdr _ = hdr

recordPacketizerC
  :: (HiddenClockResetEnable dom, KnownNat dataWidth, 1 <= dataWidth)
 => Circuit (PacketStream dom dataWidth RecordHeader) (PacketStream dom dataWidth EBHeader)
recordPacketizerC = packetizerC (const ebHeader) metaHeaderMap
  where
    ebHeader = EBHeader { _magic    = 0x4e6f
                        , _version  = 1
                        , _res      = undefined
                        , _nr       = 1
                        , _pr       = 0
                        , _pf       = 0
                        , _addrSize = 0b0100
                        , _portSize = 0b0100
                        }

    metaHeaderMap m = m { _bca = 0
                        , _rca = 0
                        , _rff = 0
                        , _wca = _bca m
                        , _wff = _rff m
                        , _rCount = 0
                        , _wCount = _rCount m
                        }


data Size = B8 | B16 | B32 | B64 deriving (Generic, Eq, BitPack)


receiverC
  :: (HiddenClockResetEnable dom)
  => Circuit (PacketStream dom dataWidth EBHeader)
             (Vec 2 (PacketStream dom dataWidth EBHeader))
receiverC = packetDispatcherC (probePred :> recordPred :> Nil)
  where
    isValid = and . sequenceA [validMagic, validVersion, validAddr, validPort]

    validMagic    = (== 0x4e6f) . _magic
    validVersion  = (== 1) . _version
    validAddr     = (/= 0) . (.&. 0b0100) . _addrSize
    validPort     = (/= 0) . (.&. 0b0100) . _portSize

    isProbe       = bitToBool . _pf

    probePred m = isValid m && isProbe m
    recordPred = isValid

    -- dispatcherC drops packets that are not matched.
    -- invalidPred = 


-- Probe packet _always_ padded to max alignment. Only header and no records
probeHandlerC :: Circuit
  (PacketStream dom dataWidth EBHeader)
  (PacketStream dom dataWidth EBHeader)
probeHandlerC = mapMeta metaMap
  where
    metaMap m = m { _pf = low, _pr = high }


-- TODO: Write custom arbiter that selects the bus with Justs instead of Maybes
-- (Essentiall foldMaybe?)
packetMuxC :: forall n dom dataWidth meta. (KnownNat n, 1 <= n) =>
  Circuit (Vec n (PacketStream dom dataWidth meta)) (PacketStream dom dataWidth meta)
-- packetMuxC = Circuit (B.first unbundle . go . B.first bundle)
packetMuxC = Circuit (B.first unbundle . go . B.first bundle)
  where
-- _ :: (Signal
--    dom (Vec (Any + 1) (Maybe (PacketStreamM2S dataWidth meta))),
--  Signal dom PacketStreamS2M)
-- -> (Signal dom (Vec n PacketStreamS2M),
    -- Signal dom (Maybe (PacketStreamM2S dataWidth meta)))
    go :: (Signal dom (Vec n (Maybe (PacketStreamM2S dataWidth meta))),
           Signal dom PacketStreamS2M)
          -> (Signal dom (Vec n PacketStreamS2M),
              Signal dom (Maybe (PacketStreamM2S dataWidth meta)))
    go (fwds, bwd) = (bwds, fwd)
      where
        bwds = repeat <$> bwd
        fwd  = fold @(n-1) (<|>) <$> fwds


traceC :: forall dom dataWidth meta .
  (HiddenClockResetEnable dom, Show meta)
  => String
  -> Circuit (PacketStream dom dataWidth meta)
             (PacketStream dom dataWidth meta)
traceC name = Circuit go
  where
    go (fwd, bwd) = (printf "<<" <$> bwd <*> counter, printf ">>" <$> fwd <*> counter)

    counter = register @dom (0 :: Integer) (counter + 1)

    printf :: (Show a, Show b) => String -> a -> b -> a
    printf dir f c = trace (dir <> " " <> show c <> " " <> name <> " " <> ": " <> show f) f


testCircuit
  :: (HiddenClockResetEnable dom, KnownNat dataWidth, 1 <= dataWidth)
  => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
testCircuit = circuit $ \pm -> do
  ebpkt <- etherboneDepacketizerC -< pm

  -- Filters out invalid, probe and record packets
  [probe, record] <- receiverC -< ebpkt

  -- The `record` branch is consumed and thrown out
  -- consume <| traceC "Record" -< record
  
  rdpkt <- traceC "RecordOut" <| recordDepacketizerC <| traceC "RecordIn" -< record
  rpkt <- recordPacketizerC -< rdpkt

  probeOut <- probeHandlerC -< probe

  resp <- packetMuxC -< [probeOut, rpkt]

  etherbonePacketizerC -< resp
  -- etherbonePacketizerC -< ebpkt


packet :: Bool -> Maybe (BitVector 32) -> Maybe (PacketStreamM2S 4 ())
packet _ Nothing  = Nothing
packet l (Just x) = Just $ PacketStreamM2S (bitCoerce x) lastIndex () False
  where lastIndex = if l then Just 3 else Nothing


streamInput :: [Maybe (PacketStreamM2S 4 ())]
streamInput = 
  [ packet False $ Just 0x4e6f11ff
  , packet True  $ Just 0x00000086
  , Nothing
  , packet False $ Just 0x4e6f1044
  , packet False $ Just 0xe80f0000
  , packet False $ Just 0xcc
  , packet False $ Just 0xdd
  , Nothing
  , packet True $ Just 0x00
  , Nothing
  , packet False $ Just 0x4e6f1144
  , packet False $ Just 0xbb
  , packet False $ Just 0xcc
  , packet True  $ Just 0xdd
  ]



myCircuit :: forall dom. (HiddenClockResetEnable dom) => Circuit (PacketStream dom 4 ()) ()
myCircuit = (etherboneDepacketizerC @dom) |> etherbonePacketizerC |> idleSink


topEntity
  :: "clk"  ::: Clock System
  -> "rstn" ::: Reset System
  -> "en"   ::: Enable System
  -> "inp" ::: Signal System (Maybe (PacketStreamM2S 4 ()))
  -> "out" ::: Signal System PacketStreamS2M
topEntity clk rst en = fn
  where
    fn x = fst $ toSignals circ (x, ())
    circ = exposeClockResetEnable myCircuit clk rst en
