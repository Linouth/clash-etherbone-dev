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
import qualified Protocols.Df as Df
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
recordDepacketizerC = depacketizerC const

recordDepacketizerToDfC
  :: (HiddenClockResetEnable dom, KnownNat dataWidth, 1 <= dataWidth)
  => Circuit (PacketStream dom dataWidth EBHeader) (Df dom RecordHeader)
recordDepacketizerToDfC = depacketizeToDfC @4 const

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

-- Extract 'side-channel' signal with the RecordHeader for the `recordBuilderC`. Only
-- the first cycle holds an accurate header.
-- TODO: This is extremely hacky. See if this can be implemented without manually
-- extracting data from the raw stream
recordSignalC
  :: forall dom dataWidth . (KnownNat dataWidth, 4 <= dataWidth)
  => Circuit (PacketStream dom dataWidth EBHeader)
             (PacketStream dom dataWidth EBHeader, CSignal dom RecordHeader)
recordSignalC = Circuit (\(fwd, (bwd, _)) -> (bwd, (fwd, header <$> fwd)))
  where
    header :: Maybe (PacketStreamM2S dataWidth EBHeader) -> RecordHeader
    header Nothing  = unpack zeroBits
    header (Just f) = unpack $ pack $ takeI @4 @(dataWidth-4) $ _data f


recordRxToTx :: RecordHeader -> RecordHeader
recordRxToTx hdr = hdr { _bca = False
                       , _rca = False
                       , _rff = False
                       , _wca = _bca hdr
                       , _wff = _rff hdr
                       , _rCount = 0
                       , _wCount = _rCount hdr
                       }

ebTx = EBHeader { _magic    = 0x4e6f
                , _version  = 1
                , _res      = 0
                , _nr       = True
                , _pr       = False
                , _pf       = False
                , _addrSize = 0b0100
                , _portSize = 0b0100
                }


data RecordBuilderState
  -- If wCount>0: Write zeros, jump to Pad
  --              or backpressure and then pad. Then it matches the rest
  -- If rCount>0: *Write header* and jump to Passthrough
  --              or give a cycle backpressure and jump to Header 
  -- No read or write: write header, jump to passthrough
  --              or Write Nothing, jump to Header. 
  = BuilderInit
  -- ^ Sends zeros or a header, depending on the wCount and rCount fields.
  | BuilderPad          { _header :: RecordHeader }
  -- ^ Sends zero padding. Required for wCount > 0 case
  | BuilderHeader       { _header :: RecordHeader }
  -- ^ Sends the header. Required for wCount > 0 case
  | BuilderPassthrough  { _header :: RecordHeader }
  deriving (Generic, NFDataX, Show, ShowX)

-- TODO: Last and abort handling
recordBuilderT
  :: forall dataWidth . (KnownNat dataWidth, 4 <= dataWidth)
  => RecordBuilderState 
  -- -> ( (Maybe (PacketStreamM2S dataWidth RecordHeader), Df.Data RecordHeader)
  --    , PacketStreamS2M
  --    )
  -- -> ( RecordBuilderState
  --    , ( (PacketStreamS2M, Ack)
  --      , Maybe (PacketStreamM2S dataWidth EBHeader) )
  --    )
  -> ( (Maybe (PacketStreamM2S dataWidth RecordHeader)
       , Maybe (PacketStreamM2S dataWidth EBHeader)
       )
     , PacketStreamS2M
     )
  -> ( RecordBuilderState
     , ( (PacketStreamS2M, PacketStreamS2M)
       , Maybe (PacketStreamM2S dataWidth EBHeader) )
     )
recordBuilderT BuilderInit ((_, Nothing), _)
  -- = errorX "BuilderInit state, but no header data given. This should not happen?"
  = (trace "Init" BuilderInit, ( (PacketStreamS2M False, PacketStreamS2M False), Nothing ))
recordBuilderT BuilderInit ((_, Just hdr'), PacketStreamS2M{_ready})
  = (nextState, ( (PacketStreamS2M _ready, PacketStreamS2M _ready), Just out ))
  where
    (nextState', out)
      | _wCount hdr > 0 = (BuilderPad hdr,         outZeros)
      | otherwise       = (BuilderPassthrough hdr, outHeader)

    nextState
      | not _ready          = BuilderInit
      | isJust (_last hdr') = BuilderInit
      | otherwise           = nextState'

    wCount = _wCount hdr
    rCount = _rCount hdr
    isLast = wCount == 0 && rCount == 0

    outLast =
      if isLast
      then Just (natToNum @(dataWidth-1))
      else Nothing

    outZeros = PacketStreamM2S (repeat 0) outLast ebTx False
    outHeader = PacketStreamM2S (dat ++ repeat @(dataWidth-4) 0) outLast ebTx False
    dat = bitCoerce (recordRxToTx hdr)

    -- This essentially replaces depacketizerToDf
    hdr :: RecordHeader
    hdr = bitCoerce $ takeI @_ @(dataWidth-4) (_data hdr')
recordBuilderT BuilderPad{_header} ((_, _), PacketStreamS2M{_ready})
  = (nextState, ( (PacketStreamS2M _ready, PacketStreamS2M True), Just outZeros ))
  where
    nextState
      | not _ready   = BuilderPad _header
      | wCount' == 0 = BuilderHeader header'
      | otherwise    = BuilderPad header'

    wCount  = _wCount _header
    wCount' = wCount - 1
    header' = _header { _wCount = wCount' }

    outZeros = PacketStreamM2S (repeat 0) Nothing ebTx False
recordBuilderT st@BuilderHeader{_header} ((_, _), PacketStreamS2M{_ready})
  = (nextState, ( (PacketStreamS2M _ready, PacketStreamS2M True), Just outHeader ))
  where
    nextState
      | not _ready  = st
      | isLast      = BuilderInit
      | otherwise   = BuilderPassthrough _header

    rCount = _rCount _header
    isLast = rCount == 0

    outHeader =
      PacketStreamM2S
        (dat ++ repeat @(dataWidth-4) 0)
        (if isLast then Just (natToNum @(dataWidth-1)) else Nothing)
        ebTx
        False
    dat = bitCoerce (recordRxToTx _header)
recordBuilderT st@BuilderPassthrough{_header} ((Nothing, _), _)
  = (st, ( (PacketStreamS2M True, PacketStreamS2M True), Nothing ))
recordBuilderT st@BuilderPassthrough{_header} ((Just x, _), PacketStreamS2M{_ready})
  = (nextState, ( (PacketStreamS2M _ready, PacketStreamS2M True), Just out))
  where
    nextState
      | not _ready       = st
      | isJust (_last x) = BuilderInit
      | otherwise        = st

    out = x { _meta = ebTx }

recordBuilderC
  :: forall dom dataWidth .
  ( HiddenClockResetEnable dom, KnownNat dataWidth, 4 <= dataWidth)
  -- => Circuit (PacketStream dom dataWidth RecordHeader, Df dom RecordHeader)
  => Circuit (PacketStream dom dataWidth RecordHeader, PacketStream dom dataWidth EBHeader)
             (PacketStream dom dataWidth EBHeader)
recordBuilderC = Circuit (B.first unbundle . go . B.first bundle)
  where
    -- go = mealyB recordBuilderT BuilderInit
    go = mealyB fn BuilderInit
      where
        fn s inp = trace ("State: " <> show s <> " Inp: " <> show (fst inp)) recordBuilderT s inp



data RecordInserterState (dataWidth :: Nat) = RecordInserterState
  { _inserted   :: Bool
  , _prevStream :: Maybe (PacketStreamM2S dataWidth EBHeader)
  } deriving (Generic, NFDataX)

-- For now, only 32 and 64 bit.
-- Bool indicates wether header has been inserted already
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
    insert  f = ebMeta f { _data = bitCoerce metaBits ++ repeat 0 }
      where
        metaBits = pack $ recordMetaMap $ _meta f

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
    metaMap m = m { _pf = False, _pr = True
                  , _addrSize = _addrSize m .&. 0b0100
                  , _portSize = _portSize m .&. 0b0100
                  }


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


data WishboneMasterState (addrWidth :: Nat)
  -- TODO: Aborted
  --       Aborts will be handled by a component higher in the chain. No need to
  --       do this in the state machine
  = WriteOrReadAddr

  | Write { _writesLeft :: Unsigned 8
          , _addr :: BitVector addrWidth
          }

  | ReadAddr
  | Read  { _readsLeft :: Unsigned 8 }

  | Wait
  deriving (Generic, Show, ShowX, NFDataX)

-- TODO: Write WBM block
-- NOTE: With a moore, the bwd=False situation can be simplified greatly, since
-- the output would be stored in the state. 
--
-- This currently fails if there is is more data in the packet than the FSM
-- expects. Failure is alright, but it dead-locks. Thats not okay. Maybe watch
-- for a new packet and reset the fsm.
wishboneMasterT
  :: forall dataWidth addrWidth .
  ( KnownNat dataWidth
  , KnownNat addrWidth
  , 4 <= dataWidth
  )
  => WishboneMasterState addrWidth
  -> (Maybe (PacketStreamM2S dataWidth RecordHeader), PacketStreamS2M)
  -> ( WishboneMasterState addrWidth
     , (PacketStreamS2M, Maybe (PacketStreamM2S dataWidth RecordHeader))
     )
wishboneMasterT state (Nothing, bwd) = (state, (bwd, Nothing))
wishboneMasterT WriteOrReadAddr  (Just x, PacketStreamS2M{_ready})
  = (nextState, (PacketStreamS2M _ready, out))
  where
    addr = resize $ pack $ _data x

    wCount = _wCount (_meta x)
    rCount = _rCount (_meta x)

    -- The state if no backpressure is given
    idealNextState
      | wCount > 0 = Write wCount addr
      | rCount > 0 = Read rCount
      | otherwise  = Wait

    nextState
      | not _ready = WriteOrReadAddr
      | otherwise  = idealNextState
    
    out = case idealNextState of
      -- Pass data if this is a BaseRetAddr
      Read _  -> Just x
      _       -> Just $ x { _data = repeat 0 }
wishboneMasterT st@Write{..} (Just x, PacketStreamS2M{_ready})
  = (nextState, (PacketStreamS2M _ready, Just out))
  where
    meta = (_meta x) { _wCount = wCount' }
    out = x { _data = repeat 0
            , _meta = meta
            }

    wCount = _writesLeft
    wCount' = wCount - 1

    nextState
      | not _ready    = st
      | wCount' == 0  = ReadAddr
      | otherwise     = Write wCount' _addr
wishboneMasterT ReadAddr (Just x, PacketStreamS2M{_ready})
  = (nextState, (PacketStreamS2M _ready, Just out))
  where
    -- Pass through the BaseRetAddr to the BaseWriteAddr field
    -- wCount is set to 0. There is no way to get to this state with wCount > 0
    meta = (_meta x) { _wCount = 0 }
    out = x { _meta = meta }

    rCount = _rCount (_meta x)

    nextState
      | not _ready = ReadAddr
      | rCount > 0 = Read rCount
      | otherwise  = Wait
wishboneMasterT st@Read{..} (Just x, PacketStreamS2M{_ready})
  = (nextState, (PacketStreamS2M _ready, Just out))
  where
    out = x { _data = repeat 0xaa }

    rCount  = _readsLeft
    rCount' = rCount - 1

    nextState
      | not _ready    = st
      | rCount' == 0  = Wait
      | otherwise     = Read rCount'
wishboneMasterT Wait (Just x, _) = (nextState, (PacketStreamS2M True, Nothing))
  where
    -- TODO: Check for end of packet. If found, jump back to WriteOrReadAddr
    -- TODO: Also check this in the ReadAddr Write and Read states, as this is
    -- where it most likely happens
    nextState = Wait


-- wishboneMasterT Wait _ = ()


{-
wishboneMasterT WaitForRecord (Just x, PacketStreamS2M{_ready})
  = (nextState, (PacketStreamS2M _ready, Just out))
  where
    headerVec = takeI @4 @(dataWidth - 4) (_data x)
    meta :: RecordHeader
    meta = unpack $ bitCoerce headerVec

    out = PacketStreamM2S { _data = repeat 0
                          , _last = _last x
                          , _meta = meta
                          , _abort = _abort x}

    nextState
      | not _ready        = WaitForRecord
      | _wCount meta > 0  = trace ("Write " <> show (_data x))  Write meta Nothing
      | _rCount meta > 0  = Read meta Nothing
      | otherwise         = WaitForRecord
wishboneMasterT st@Write{..} (Just x, PacketStreamS2M{_ready})
  = (nextState, (PacketStreamS2M ready, Just out))
  where
    -- TODO: Check wishbone response (and with ack?)
    ready = _ready 
    ack = True
    out = PacketStreamM2S { _data = repeat 0
                          , _last = _last x
                          , _meta = meta
                          , _abort = _abort x
                          }

    meta
      | isNothing _addr       = _metaOut
      | ack && isJust _addr   = _metaOut { _wCount = _wCount _metaOut - 1 }
      | otherwise             = _metaOut

    nextState
      | not ready             = st
      | _wCount meta == 0     = trace "count0" (if _rCount meta > 0
        then trace "w>r" Read meta Nothing
        else trace "w>wfr" WaitForRecord)
      | isNothing _addr       = trace "noAddr" Write meta (Just addr)
      | otherwise             = trace "other" Write meta _addr

    addrBytes :: Vec (DivRU addrWidth 8) (BitVector 8)
    addrBytes = takeI @_ @(dataWidth - DivRU addrWidth 8) (_data x)
    addr :: BitVector addrWidth
    addr = bitCoerce addrBytes
wishboneMasterT st@Read{..} (Just x, PacketStreamS2M{_ready})
  = (nextState, (PacketStreamS2M ready, out))
  where
    ready = _ready

    out
      | not ready = Nothing
      | otherwise = Just PacketStreamM2S { _data = bitCoerce (0xaabbccdd :: BitVector 32) ++ repeat 0
                                         , _last = _last x
                                         , _meta = _metaOut
                                         , _abort = _abort x
                                         }

    nextState
      | not ready       = st
      | isNothing _addr = Read _metaOut (Just addr)
      | otherwise       = st

    addrBytes :: Vec (DivRU addrWidth 8) (BitVector 8)
    addrBytes = takeI (_data x)
    addr :: BitVector addrWidth
    addr = bitCoerce addrBytes
-}


-- Reason for receiving the header in the state machine is so that we can send
-- zeros that word. The depacketizer sends Nothing the cycle of the header. 
--
-- For now addrWidth ~ 32 because there is not wishbone bus connected yet, which
-- sould dictate the width.
wishboneMasterC
  :: forall dom dataWidth addrWidth .
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , KnownNat addrWidth
  , addrWidth ~ 32
  , 4 <= dataWidth
  , DivRU addrWidth 8 <= dataWidth
  , DivRU addrWidth 8 * 8 ~ addrWidth)
  => Circuit (PacketStream dom dataWidth RecordHeader)
             (PacketStream dom dataWidth RecordHeader)
             -- , Wishbone dom Standard 32 (BitVector dataWidth))
wishboneMasterC = Circuit $ fsm
  where
    fsm (fwd, bwd) = mealyB (wishboneMasterT @_ @addrWidth) WriteOrReadAddr
      (fwd, bwd)
    -- tr f = traceSignal1 "" $ foo <$> f


foo :: (KnownNat dataWidth) => Maybe (PacketStreamM2S dataWidth meta) -> BitVector (dataWidth * 8)
foo Nothing  = oneBits
foo (Just x) = bitCoerce $ _data x


-- Final circuit
myCircuit
  :: (HiddenClockResetEnable dom, KnownNat dataWidth, 4 <= dataWidth)
  => Circuit (PacketStream dom dataWidth ()) (PacketStream dom dataWidth ())
myCircuit = circuit $ \pm -> do
  ebpkt <- etherboneDepacketizerC <| traceC "CircIn" -< pm

  -- Filters out invalid, probe and record packets
  [probe, record] <- receiverC -< ebpkt
  
  -- wbmOut <- traceC "WBMOut" <| wishboneMasterC <| traceC "WBMIn " <| recordDepacketizerC <| traceC "RcrdIn" -< record
  -- rpkt <- traceC "InserterOut" <| recordInserterC -< wbmOut

  [recordA, recordB] <- fanout -< record
  wbmOut <- traceC "WBMOut" <| wishboneMasterC <| traceC "WBMIn " <| recordDepacketizerC <| traceC "RcrdIn" -< recordB

  rA <- traceC "ByPass" -< recordA
  rpkt <- traceC "Builder" <| recordBuilderC -< (wbmOut, rA)

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


-- takeILe :: forall f a b. (Resize f, KnownNat a, KnownNat b, b <= a) => f a -> f b
-- takeILe x = leToPlus @b @a $ takeI x


streamInput :: [Maybe (PacketStreamM2S 4 ())]
streamInput = 
  [ packet False $ Just 0x4e6f11ff
  , packet True  $ Just 0x00000086
  , Nothing
  , packet False $ Just 0x4e6f1044
  , packet False $ Just 0xe80f0100
  , packet False $ Just 0x11
  , packet True $ Just 0x22
  -- , packet True  $ Just 0x00
  , Nothing
  , packet False $ Just 0x4e6f1044
  , packet True $ Just 0xe80f0000
  --, packet False $ Just 0xcc
  --, packet True  $ Just 0xdd
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
    circ = exposeClockResetEnable (myCircuit @_ @4) clk rst en


-- Simulate with
-- mapM_ print $ L.take 32 $ simulateC (withClockResetEnable @System clockGen resetGen enableGen (myCircuit)) config streamInput

makeTopEntity 'topEntity


-- main :: IO ()
-- main = do
--   let sim = exposeClockResetEnable simulateC systemClockGen systemResetGen enableGen myCircuit config streamInput
--   -- let  = exposeClockResetEnable mainCounter systemClockGen systemResetGen enableGen
--   vcd <- dumpVCD (0, 100) sim ["main", "sub"]
--   case vcd of
--     Left msg ->
--       error msg
--     Right contents ->
--       writeFile "mainCounter.vcd" contents
