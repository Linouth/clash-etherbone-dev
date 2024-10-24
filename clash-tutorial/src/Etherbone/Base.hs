{-# OPTIONS -fplugin=Protocols.Plugin #-}

module Etherbone.Base where

import Clash.Prelude
import Protocols
import Protocols.PacketStream
import Debug.Trace


etherboneVersion :: Natural
etherboneVersion = 1

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
etherboneDepacketizerC :: forall dom dataWidth .
 ( HiddenClockResetEnable dom, KnownNat dataWidth, 4 <= dataWidth )
 => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth EBHeader)
etherboneDepacketizerC = depacketizerC @_ metaMap
  where
    metaMap :: (EBHeader, Vec (dataWidth - 4) (BitVector 8)) -> () -> EBHeader
    metaMap hdr _ = fst hdr


-- Concat EBHeader metadata into data stream
-- This adds a cycle latency!
etherbonePacketizerC :: forall dom dataWidth .
  ( HiddenClockResetEnable dom, KnownNat dataWidth, 4 <= dataWidth )
  => Circuit (PacketStream dom dataWidth EBHeader) (PacketStream dom dataWidth ())
etherbonePacketizerC = packetizerC (const ()) metaMap
  where
    metaMap :: EBHeader -> (EBHeader, Vec (dataWidth - 4) (BitVector 8))
    metaMap hdr = (hdr, repeat 0)


-- Extract RecordHeader data from stream into metadata
-- We can drop EBHeader. All info in here is static and known
recordDepacketizerC :: forall dom dataWidth . 
  ( HiddenClockResetEnable dom, KnownNat dataWidth, 4 <= dataWidth )
  => Circuit (PacketStream dom dataWidth EBHeader) (PacketStream dom dataWidth RecordHeader)
recordDepacketizerC = depacketizerC metaMap
  where
    metaMap :: (RecordHeader, Vec (dataWidth - 4) (BitVector 8)) -> EBHeader -> RecordHeader
    metaMap hdr _ = fst hdr


sizeMask :: Integer -> BitVector 4
sizeMask n
  | n <= 2    = fromInteger n
  | n <= 4    = 0b0100
  | n <= 8    = 0b1000
  | otherwise = error "Etherbone v1 only supports up to 64-bit busses."

receiverC :: forall dom dataWidth addrWidth . 
  ( HiddenClockResetEnable dom, KnownNat dataWidth, KnownNat addrWidth )
  => SNat addrWidth
  -> Circuit (PacketStream dom dataWidth EBHeader)
             (Vec 2 (PacketStream dom dataWidth EBHeader))
receiverC SNat = packetDispatcherC (probePred :> recordPred :> Nil)
  where
    isValid = and . sequenceA [validMagic, validVersion, validAddr, validPort]

    validMagic    = (== 0x4e6f) . _magic
    validVersion  = (== fromIntegral etherboneVersion) . _version
    validAddr     = (/= 0) . (.&. addrSizeMask) . _addrSize
    validPort     = (/= 0) . (.&. portSizeMask) . _portSize

    isProbe       = _pf

    probePred m = isValid m && isProbe m
    recordPred = isValid

    portSizeMask = sizeMask $ natToInteger @dataWidth
    addrSizeMask = sizeMask $ natToInteger @(Div addrWidth 8)


-- Handle probe records. Header flags set correctly and probe-id is passed
-- through automatically.
-- Probe is packet _always_ padded to max alignment. Only header and no records
probeHandlerC :: forall dom dataWidth addrWidth .
  ( KnownNat dataWidth, KnownNat addrWidth )
  => SNat addrWidth
  -> Circuit
  (PacketStream dom dataWidth EBHeader)
  (PacketStream dom dataWidth EBHeader)
probeHandlerC SNat = mapMeta metaMap
  where
    metaMap m = m { _pf = False, _pr = True
                  , _addrSize = _addrSize m .&. addrSizeMask
                  , _portSize = _portSize m .&. portSizeMask
                  }

    portSizeMask = sizeMask $ natToInteger @dataWidth
    addrSizeMask = sizeMask $ natToInteger @(Div addrWidth 8)


arbiterC :: (HiddenClockResetEnable dom, KnownNat n, 1 <= n) =>
  Circuit (Vec n (PacketStream dom dataWidth meta)) (PacketStream dom dataWidth meta)
arbiterC = packetArbiterC RoundRobin

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

-- TODO: traceC type function that instead of printing, adds a traceSignal1 to
-- the fwd and bwd channels.
