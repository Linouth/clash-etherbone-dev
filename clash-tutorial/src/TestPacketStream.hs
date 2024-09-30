{-# OPTIONS -fplugin=Protocols.Plugin #-}
{-# LANGUAGE RecordWildCards #-}

module TestPacketStream where

import Debug.Trace
import Data.Bifunctor as B
import Data.Maybe

import Clash.Prelude
import Clash.Annotations.TH

import Protocols
import Protocols.PacketStream
import Protocols.PacketStream.Base
import Protocols.Wishbone
import Protocols.Idle
-- import Protocols.Internal
-- import Protocols.Wishbone


config :: SimulationConfig
config = SimulationConfig 1 maxBound False

data EBHeader = EBHeader
  { _magic    :: Unsigned 16
  , _version  :: Unsigned 4
  , _res      :: Bit
  , _nr       :: Bool
  , _pr       :: Bool
  , _pf       :: Bool
  , _addrSize :: BitVector 4
  , _portSize :: BitVector 4
  } deriving (Generic, BitPack, NFDataX, Show, ShowX)

data RecordHeader = RecordHeader
  { _bca    :: Bool
  , _rca    :: Bool
  , _rff    :: Bool
  , _res0   :: Bit
  , _cyc    :: Bool
  , _wca    :: Bool
  , _wff    :: Bool
  , _res1   :: Bit
  , _byteEn :: BitVector 8
  , _wCount :: Unsigned 8
  , _rCount :: Unsigned 8
  } deriving (Generic, BitPack, NFDataX, Show, ShowX)


-- Extract EBHeader data from stream into metadata
etherboneDepacketizerC
 :: (HiddenClockResetEnable dom, KnownNat dataWidth, 1 <= dataWidth)
 => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth EBHeader)
etherboneDepacketizerC = depacketizerC @_ metaMap
  where metaMap hdr _ = hdr


-- Concat EBHeader metadata into data stream
-- This adds a cycle latency!
etherbonePacketizerC
  :: (HiddenClockResetEnable dom, KnownNat dataWidth, 1 <= dataWidth)
  => Circuit (PacketStream dom dataWidth EBHeader) (PacketStream dom dataWidth ())
etherbonePacketizerC = packetizerC (const ()) id


-- Extract RecordHeader data from stream into metadata
-- We can drop EBHeader. All info in here is static and known
recordDepacketizerC
  :: (HiddenClockResetEnable dom, KnownNat dataWidth, 1 <= dataWidth)
 => Circuit (PacketStream dom dataWidth EBHeader) (PacketStream dom dataWidth RecordHeader)
recordDepacketizerC = depacketizerC metaMap
  where metaMap hdr _ = hdr

-- Concat RecordHeader metadata back into data stream
-- Header is moddified to map to a response record (e.g. WCount = RCount)
recordPacketizerC
  :: (HiddenClockResetEnable dom, KnownNat dataWidth, 1 <= dataWidth)
 => Circuit (PacketStream dom dataWidth RecordHeader) (PacketStream dom dataWidth EBHeader)
recordPacketizerC = packetizerC (const ebHeader) metaHeaderMap
  where
    ebHeader = EBHeader { _magic    = 0x4e6f
                        , _version  = 1
                        , _res      = undefined
                        , _nr       = True
                        , _pr       = False
                        , _pf       = False
                        , _addrSize = 0b0100
                        , _portSize = 0b0100
                        }

    metaHeaderMap m = m { _bca = False
                        , _rca = False
                        , _rff = False
                        , _wca = _bca m
                        , _wff = _rff m
                        , _rCount = 0
                        , _wCount = _rCount m
                        }


-- TODO: Use Enum (with one-hot encoding) for addrWidth and portWidth
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

    isProbe       = _pf

    probePred m = isValid m && isProbe m
    recordPred = isValid

    -- dispatcherC drops packets that are not matched.
    -- invalidPred = 


-- Handle probe records. Header flags set correctly and probe-id is passed
-- through automatically.
-- Probe is packet _always_ padded to max alignment. Only header and no records
probeHandlerC :: Circuit
  (PacketStream dom dataWidth EBHeader)
  (PacketStream dom dataWidth EBHeader)
probeHandlerC = mapMeta metaMap
  where
    metaMap m = m { _pf = False, _pr = True }


-- 'Cheap' arbiter, making use of the fact that there is always only one input
-- stream with data.
-- TODO: Check if this is working correctly. I expect it to fail, as the record
-- path has more latency than the probe path. If there is a probe and record
-- packet directly after each-other this might cause issues in the Bwd channel.
packetMuxC :: forall n dom dataWidth meta. (KnownNat n, 1 <= n) =>
  Circuit (Vec n (PacketStream dom dataWidth meta)) (PacketStream dom dataWidth meta)
packetMuxC = Circuit (B.first unbundle . go . B.first bundle)
  where
    go :: (Signal dom (Vec n (Maybe (PacketStreamM2S dataWidth meta))),
           Signal dom PacketStreamS2M)
          -> (Signal dom (Vec n PacketStreamS2M),
              Signal dom (Maybe (PacketStreamM2S dataWidth meta)))
    go (fwds, bwd) = (bwds, fwd)
      where
        bwds = repeat <$> bwd
        fwd  = fold @(n-1) (<|>) <$> fwds


cycExportC :: Circuit (PacketStream dom dataWidth RecordHeader)
                      (PacketStream dom dataWidth RecordHeader
                      , CSignal dom (Maybe Bool))
cycExportC = Circuit go
  where 
    go (fwd, (bwd, _)) = (bwd, (fwd, cycBit <$> fwd))

    cycBit :: Maybe (PacketStreamM2S dataWidth RecordHeader) -> Maybe Bool
    cycBit Nothing  = Nothing
    cycBit (Just x)
      -- | _wCount hdr == 0 && _rCount hdr == 0 = Nothing
      | (_wCount hdr .|. _rCount hdr) == 0 = Nothing
      | otherwise = Just $ _cyc hdr
        where hdr = _meta x



-- This circuit prints all data that goes through it. Useful for debugging.
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


-- slave
--   :: Circuit ()
--              (Wishbone dom Standard 32 (BitVector dataWidth))
--   -> Circuit (PacketStream dom dataWidth RecordHeader)
--              (PacketStream dom dataWidth RecordHeader)
-- -- slave = id

-- data WishboneMasterState = WaitForRecord | WriteAddr | Write | ReadAddr | Read
data WishboneMasterState (addrWidth :: Nat)
  = WaitForRecord
  | Write { _addr :: Maybe (BitVector addrWidth) }
  | Read  { _addr :: Maybe (BitVector addrWidth) }
  deriving (Generic, Show, ShowX)

-- TODO: Write WBM block
-- wishboneMasterT
--   :: WishboneMasterState addrWidth
--   -> (Maybe (PacketStreamM2S dataWidth EBHeader), PacketStreamS2M)
--   -> ( WishboneMasterState addrWidth
--      , (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth RecordHeader))
--      )
--
-- wishboneMaster
--   :: Circuit (PacketStream dom dataWidth meta)
--              (PacketStream dom dataWidth meta
--              , Wishbone dom Standard 32 (BitVector dataWidth))
-- wishboneMaster = id

data RecordInserterState (dataWidth :: Nat) = RecordInserterState
  { _inserted   :: Bool
  , _prevStream :: Maybe (PacketStreamM2S dataWidth EBHeader)
  } deriving (Generic, NFDataX)


-- For now, only 32 and 64 bit.
recordInserterC
  :: forall dom dataWidth .
  (HiddenClockResetEnable dom, KnownNat dataWidth, 4 <= dataWidth, dataWidth~4)
  => Circuit (PacketStream dom dataWidth RecordHeader)
             (PacketStream dom dataWidth EBHeader)
recordInserterC = Circuit go
  where
    go (fwd, bwd) = (bwd, mealyB insertT False (fwd, bwd))

    insertT
      :: Bool
      -> (Maybe (PacketStreamM2S dataWidth RecordHeader), PacketStreamS2M)
      -> (Bool, Maybe (PacketStreamM2S dataWidth EBHeader))
    insertT s     (Nothing, _)    = (s, Nothing)
    insertT False (Just f,  b)
      | _wCount (_meta f) == 0    = case _last f of
        Nothing                  -> (_ready b, Just $ insert f)
        Just _                   -> (False,    Just $ insert f)
      | otherwise                 = (False,    Just $ ebMeta f)
    insertT True  (Just f,  b)
      | isJust (_last f)          = (not $ _ready b, Just $ ebMeta f)
      | otherwise                 = (True,           Just $ ebMeta f)

    ebMeta  f = f { _meta = ebHeader }
    -- insert  f = ebMeta f { _data = bitCoerce (0 :: BitVector 32) }
    insert  f = ebMeta f { _data = bitCoerce (metaBits ++# zeroPadding) }
      where
        zeroPadding :: BitVector (dataWidth * 8 - BitSize RecordHeader)
        zeroPadding = zeroBits
        metaBits = pack $ recordMetaMap $ _meta f
        -- dataVec :: Vec dataWidth (BitVector 8)
        -- dataVec = bitCoerce metaBits ++ repeat 0

    ebHeader = EBHeader { _magic     = 0x4e6f
                        , _version   = 1
                        , _res       = 0
                        , _nr        = True
                        , _pr        = False
                        , _pf        = False
                        , _addrSize  = width
                        , _portSize  = width
                        }
      where width = snatToNum (SNat @dataWidth)

    recordMetaMap m = m { _bca = False
                        , _rca = False
                        , _rff = False
                        , _wca = _bca m
                        , _wff = _rff m
                        , _rCount = 0
                        , _wCount = _rCount m
                        }


recordHandlerC = mapMeta fn
  where fn _ = EBHeader { _magic     = 0x4e6f
                        , _version   = 0b1111
                        , _res       = 0
                        , _nr        = True
                        , _pr        = False
                        , _pf        = False
                        , _addrSize  = 0b0100
                        , _portSize  = 0b0100
                        }

-- Final circuit
myCircuit
  :: (HiddenClockResetEnable dom, KnownNat dataWidth, 4 <= dataWidth, dataWidth~4)
  => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
myCircuit = circuit $ \pm -> do
  ebpkt <- etherboneDepacketizerC -< pm

  -- Filters out invalid, probe and record packets
  [probe, record] <- receiverC -< ebpkt

  -- The `record` branch is consumed and thrown out
  -- consume <| traceC "Record" -< record
  
  rdpkt <- traceC "RecordOut" <| recordDepacketizerC <| traceC "RecordIn" -< record
  rpkt <- traceC "InserterOut" <| recordInserterC -< rdpkt
  -- rpkt <- traceC "InserterOut" <| recordPacketizerC -< rdpkt

  probeOut <- probeHandlerC -< probe

  resp <- packetMuxC -< [rpkt, probeOut] -- Bias towards first entry

  etherbonePacketizerC -< resp
  -- etherbonePacketizerC -< ebpkt

{-
  With recordHandlerC
   Number of wires:                351
   Number of wire bits:           7036
   Number of public wires:         351
   Number of public wire bits:    7036
   Number of ports:                  5
   Number of port bits:             77
   Number of memories:               0
   Number of memory bits:            0
   Number of processes:              0
   Number of cells:                225
     L6MUX21                        10
     LUT4                          124
     PFUMX                          21
     TRELLIS_FF                     70

  With recordInserterC
   Number of wires:                712
   Number of wire bits:           8035
   Number of public wires:         712
   Number of public wire bits:    8035
   Number of ports:                  5
   Number of port bits:             77
   Number of memories:               0
   Number of memory bits:            0
   Number of processes:              0
   Number of cells:                630
     L6MUX21                        76
     LUT4                          335
     PFUMX                         119
     TRELLIS_FF                    100

  With recordPacketizerC
   Number of wires:                477
   Number of wire bits:           8708
   Number of public wires:         477
   Number of public wire bits:    8708
   Number of ports:                  5
   Number of port bits:             77
   Number of memories:               0
   Number of memory bits:            0
   Number of processes:              0
   Number of cells:                386
     L6MUX21                         7
     LUT4                          213
     PFUMX                          50
     TRELLIS_FF                    116

-}

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
  , packet False $ Just 0xe80f0100
  , packet False $ Just 0xcc
  , packet False $ Just 0xdd
  , packet True  $ Just 0x00
  , Nothing
  , packet False $ Just 0x4e6f1044
  , packet False $ Just 0xe80f0000
  , packet False $ Just 0xcc
  , packet True  $ Just 0xdd
  , Nothing
  , packet False $ Just 0x4e6f1044
  , packet False $ Just 0xe80f0100
  , packet False $ Just 0xcc
  , packet True  $ Just 0xdd
  , Nothing
  , packet True $ Just 0x00
  , packet True $ Just 0x00
  , packet False $ Just 0x4e6f1144
  , packet False $ Just 0xbb
  , packet False $ Just 0xcc
  , packet True  $ Just 0xdd
  ]


topEntity
  :: "clk"  ::: Clock System
  -> "rstn" ::: Reset System
  -> "en"   ::: Enable System
  -> "inp" ::: Signal System (Maybe (PacketStreamM2S 4 ()))
  -> "out" ::: Signal System (Maybe (PacketStreamM2S 4 ()))
topEntity clk rst en = fn
  where
    fn x = snd $ toSignals circ (x, pure $ PacketStreamS2M True)
    circ = exposeClockResetEnable myCircuit clk rst en


-- Simulate with
-- mapM_ print $ L.take 32 $ simulateC (withClockResetEnable @System clockGen resetGen enableGen (myCircuit)) config streamInput

makeTopEntity 'topEntity
