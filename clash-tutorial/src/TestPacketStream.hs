{-# OPTIONS -fplugin=Protocols.Plugin #-}
{-# LANGUAGE RecordWildCards #-}

module TestPacketStream where

-- import Debug.Trace
import Data.Bifunctor as B
import Data.Maybe

import Clash.Prelude
import Clash.Annotations.TH
import Clash.Annotations.SynthesisAttributes
import Clash.Magic

import Protocols
import Protocols.PacketStream
import Protocols.Wishbone
import qualified Prelude as P
import Etherbone.Base
import Etherbone.WishboneMaster (wishboneMasterC, WBData, ByteSize)
import Scratchpad (wishboneScratchpad)
import Numeric (showHex)
import Etherbone (etherboneC)

trace _ x = x


config :: SimulationConfig
config = SimulationConfig 1 maxBound False


-- Concat RecordHeader metadata back into data stream
-- Header is moddified to map to a response record (e.g. WCount = RCount)
recordPacketizerC
  :: (HiddenClockResetEnable dom, KnownNat dataWidth, 1 <= dataWidth)
 => Circuit (PacketStream dom dataWidth RecordHeader) (PacketStream dom dataWidth EBHeader)
recordPacketizerC = packetizerC (const ebHeader) metaHeaderMap
  where
    ebHeader = EBHeader { _magic    = 0x4e6f
                        , _version  = fromIntegral etherboneVersion
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


-- {-# ANN myCircuit (Synthesize
--     { t_name = "myCircuit"
--     , t_inputs = [ PortName "recvInFwd", PortName "recvOutBwd" ]
--     , t_output = PortProduct "" [ PortName "recvInBwd", PortName "rcvOutFwd" ]
--     }
--   ) #-}

    -- dispatcherC drops packets that are not matched.
    -- invalidPred = 



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


-- cycExportC :: Circuit (PacketStream dom dataWidth RecordHeader)
--                       (PacketStream dom dataWidth RecordHeader
--                       , CSignal dom (Maybe Bool))
-- cycExportC = Circuit go
--   where 
--     go (fwd, (bwd, _)) = (bwd, (fwd, cycBit <$> fwd))
--
--     cycBit :: Maybe (PacketStreamM2S dataWidth RecordHeader) -> Maybe Bool
--     cycBit Nothing  = Nothing
--     cycBit (Just x)
--       -- | _wCount hdr == 0 && _rCount hdr == 0 = Nothing
--       | (_wCount hdr .|. _rCount hdr) == 0 = Nothing
--       | otherwise = Just $ _cyc hdr
--         where hdr = _meta x



-- This circuit prints all data that goes through it. Useful for debugging.


-- type WBData bytes = BitVector (bytes*8)

{-
data WishboneMasterState (addrWidth :: Nat)
  -- TODO: Aborted
  --       Aborts will be handled by a component higher in the chain. No need to
  --       do this in the state machine. Though I do need to watch for abort and
  --       last, as these reset the state machine.
  = WriteOrReadAddr

  | Write { _writesLeft :: Unsigned 8
          , _addr :: BitVector addrWidth
          }

  | ReadAddr
  | Read  { _readsLeft :: Unsigned 8 }

  -- | Wait
  deriving (Generic, Show, ShowX, NFDataX)

data WishboneContext (addrWidth :: Nat) = WishboneContext
  { _fsmState :: WishboneMasterState addrWidth
  , _prevCyc :: Bool
  }
  deriving (Generic, Show, ShowX, NFDataX)

initialCtx = WishboneContext
  { _fsmState = WriteOrReadAddr
  , _prevCyc = False
  }


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
  , Div (dataWidth * 8 + 7) 8 ~ dataWidth
  , 4 <= dataWidth
  )
  => WishboneContext addrWidth
  -> ( Maybe (PacketStreamM2S dataWidth RecordHeader)
     , (PacketStreamS2M, WishboneS2M (WBData dataWidth))
     )
  -> ( WishboneContext addrWidth
     , (PacketStreamS2M, ( Maybe (PacketStreamM2S dataWidth RecordHeader)
                         , WishboneM2S addrWidth dataWidth (WBData dataWidth)
                         ))
     )
wishboneMasterT ctx (Nothing, (_, _))
  -- TODO: This is not correct. WB should stay constant (except for STB)
  = (ctx, (PacketStreamS2M True, (Nothing, emptyWishboneM2S{busCycle = _prevCyc ctx})))
wishboneMasterT ctx (Just psFwdL, (psBwdR@PacketStreamS2M{_ready}, wbBwd@WishboneS2M{..}))
  = (newCtx, (psBwdL, (psFwdR, wbFwd)))
  where
    wbAck = (acknowledge || err || retry) && not stall
    ack = case state of
      WriteOrReadAddr -> _ready
      ReadAddr        -> _ready
      _               -> _ready && wbAck

    psWord = pack $ _data psFwdL

    state = _fsmState ctx
    nextState = fsm state psFwdL
    newCtx
      | not ack   = ctx
      -- | last or abort = back to state0
      | otherwise = ctx { _fsmState = nextState
                        , _prevCyc = cyc
                        }

    psBwdL = PacketStreamS2M
      ( case state of
          WriteOrReadAddr -> _ready
          Write _ _       -> ack
          ReadAddr        -> _ready
          Read _          -> ack
      )

    hdr = _meta psFwdL

    psFwdR = case state of
      WriteOrReadAddr -> case nextState of
            Read _    -> Just psFwdL 
            Write _ _ -> Nothing
            _         -> error "Cannot go from WriteOrReadAddr to another state than Write or Read"  -- TODO: This can happen, only on 'abort'
      Write _ _       -> Nothing
      ReadAddr        -> Just psFwdL { _meta = hdr { _wCount = 0 } }
      Read _          -> Just psFwdL { _data = bitCoerce readData}
    
    wbFwd = case state of
      WriteOrReadAddr -> emptyWishboneM2S
      Write _ a       -> wbOut
                           { addr = a
                           , writeData = bitCoerce psWord
                           , writeEnable = True
                           }
      ReadAddr        -> emptyWishboneM2S { busCycle=cyc }
      Read _          -> wbOut
                           { addr = resize psWord
                           , writeEnable = False
                           }

    -- If wishbone does longer over a transaction, the packetstream bus will receive
    -- back-pressure. The other components will ensure that the data on the bus
    -- will not change. So this can work.
    wbOut = (emptyWishboneM2S @addrWidth)
      { addr = undefined
      -- , writeData = bitCoerce $ _data x
      , busSelect   = resize byteEnable
      , lock        = False
      , busCycle    = cyc
      , strobe      = _ready -- TODO: should this be var? for not ack
      }
    byteEnable = _byteEn $ _meta psFwdL
    cyc = case state of
      Write _ _       -> cycSetOrDrop
      Read _          -> cycSetOrDrop
      _               -> _prevCyc ctx
      where
        cycSetOrDrop = case nextState of
          -- Drop CYC if last entry and cyc field in record header is high
          WriteOrReadAddr -> not $ _cyc hdr
          _               -> True

    fsm
      :: WishboneMasterState addrWidth
      -> PacketStreamM2S dataWidth RecordHeader
      -> WishboneMasterState addrWidth
    fsm WriteOrReadAddr x = st'
      where
        wCount = _wCount (_meta x)
        rCount = _rCount (_meta x)

        -- The state if no backpressure is given
        st'
          | wCount > 0 = Write wCount $ resize psWord
          | rCount > 0 = Read rCount
          | otherwise  = WriteOrReadAddr
    fsm Write{..} x = st'
      where
        wCount = _writesLeft
        wCount' = wCount - 1
        rCount = _rCount (_meta x)

        st'
          | wCount' == 0  =
            if rCount == 0
            then WriteOrReadAddr
            else ReadAddr
          | otherwise     = Write wCount' addr'

        -- This also should only happen if acked. This is currently implemented
        -- by the wrapping function by not advancing to the updated state if not
        -- acked.
        addr'
          | _wff (_meta x) = _addr
          | otherwise      = _addr + (natToNum @dataWidth)
    fsm ReadAddr x = st'
      where
        rCount = _rCount (_meta x)

        st'
          | rCount > 0 = Read rCount
          | otherwise  = WriteOrReadAddr
    fsm Read{..} _ = st'
      where
        rCount  = _readsLeft
        rCount' = rCount - 1

        st'
          | rCount' == 0  = WriteOrReadAddr
          | otherwise     = Read rCount'

type WbmInput (dataWidth :: Nat)
  = ( Maybe (PacketStreamM2S dataWidth RecordHeader)
    , (PacketStreamS2M, WishboneS2M (WBData dataWidth)) )

type WbmOutput (dataWidth :: Nat) (addrWidth :: Nat)
  = (PacketStreamS2M, ( Maybe (PacketStreamM2S dataWidth RecordHeader)
                      , WishboneM2S addrWidth dataWidth (WBData dataWidth)
                      ))

wbmInput :: forall dataWidth . (KnownNat dataWidth)
  => [ ( Maybe (PacketStreamM2S dataWidth RecordHeader)
       , (PacketStreamS2M, WishboneS2M (WBData dataWidth)) )
     ]
wbmInput = [ pkt 1 1 False True  False 0xaaaaaaaa undefined
           , pkt 1 1 False True  True  0xdeadbeef undefined
           , pkt 1 1 False True  False 0xbbbbbbbb undefined
           , pkt 1 1 True  True  True  0xcccccccc 0xcafecafe
           -- , pkt 1 0 False False True  0xdeadbeef
           ]
  where
    pkt :: Unsigned 8 -> Unsigned 8 -> Bool -> Bool -> Bool -> WBData dataWidth -> WBData dataWidth ->
      ( Maybe (PacketStreamM2S dataWidth RecordHeader)
       , (PacketStreamS2M, WishboneS2M (WBData dataWidth)) )
    pkt wCount rCount lastFrag bp ack psDat wbDat =
      ( Just ps
      , ( PacketStreamS2M bp, (emptyWishboneS2M @(WBData dataWidth)) {readData=wbDat, acknowledge=ack} )
      )
      where
        ps = PacketStreamM2S (bitCoerce psDat) (if lastFrag then Just 3 else Nothing) meta False
        meta = RecordHeader
          { _bca    = False
          , _rca    = False
          , _rff    = False
          , _res0   = 0
          , _cyc    = True
          , _wca    = False
          , _wff    = False
          , _res1   = 0
          , _byteEn = 0x0f
          , _wCount = wCount
          , _rCount = rCount
          }

simWbm :: forall dataWidth addrWidth .
  ( KnownNat dataWidth
  , KnownNat addrWidth
  , 4 <= dataWidth
  , addrWidth ~ dataWidth * 8
  , Div (addrWidth + 7) 8 ~ dataWidth
  )
  => [WbmInput dataWidth] -> [WbmOutput dataWidth addrWidth]
simWbm inputs = snd $ P.foldl fn (initialCtx, []) inputs
  where
    fn :: (WishboneContext addrWidth, [WbmOutput dataWidth addrWidth])
       -> WbmInput dataWidth
       -> (WishboneContext addrWidth, [WbmOutput dataWidth addrWidth])
    fn (ctx, results) input = (newCtx, results P.++ [result])
      where
        (newCtx, result) = wishboneMasterT ctx input


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
  , (Div (dataWidth * 8 + 7) 8 ~ dataWidth)
  , 4 <= dataWidth)
  => Circuit (PacketStream dom dataWidth RecordHeader)
             ( PacketStream dom dataWidth RecordHeader
             , Wishbone dom Standard addrWidth (WBData dataWidth)
             )
wishboneMasterC = Circuit (B.second unbundle . fsm . B.second bundle)
  where
    fsm (fwd, bwd) = mealyB (wishboneMasterT @_ @addrWidth) initialCtx
      (fwd, bwd)
-}




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


-- Converts a Wishbone stream to a PacketStream
-- Adds one cycle latency
wishboneToPacketStream :: forall dom addrWidth dataWidth dat .
  ( HiddenClockResetEnable dom
  , KnownNat addrWidth
  , KnownNat dataWidth
  , BitPack dat
  , BitSize dat ~ dataWidth * 8
  , NFDataX dat
  )
  => Circuit (Wishbone dom Standard addrWidth dat)
             (PacketStream dom dataWidth ())
wishboneToPacketStream = Circuit (unbundle . mealy go emptyWishboneM2S . bundle)
  where
    go :: WishboneM2S addressWidth selWidth dat
      -> (WishboneM2S addressWidth selWidth dat, PacketStreamS2M)
      -> (WishboneM2S addressWidth selWidth dat,
          (WishboneS2M dat, Maybe (PacketStreamM2S dataWidth ())))
    go prev (wbFwd, PacketStreamS2M{_ready}) = (newPrev, (wbBwd, psFwd))
      where
        newPrev = wbFwd

        okPrev = busCycle prev && strobe prev
        ok = busCycle wbFwd && strobe wbFwd

        wbBwd = emptyWishboneS2M { acknowledge = ok && okPrev && _ready }

        isLast = if okPrev && not ok then Just 3 else Nothing

        psFwd
          | okPrev = if writeEnable prev
            then Just $ PacketStreamM2S (bitCoerce (writeData prev)) isLast () False
            else Nothing
          | otherwise = Nothing

-- This is not fully correct yet.
-- If there is backpressure from the wishbone side, we still get new input data?
-- Or not? This might be working fine.
packetStreamToWishbone :: forall dom dataWidth addrWidth dat .
  ( HiddenClockResetEnable dom
  , KnownNat addrWidth
  , KnownNat dataWidth
  , BitSize dat ~ dataWidth * 8
  , BitPack dat
  , NFDataX dat
  )
  => Circuit (PacketStream dom dataWidth ())
             (Wishbone dom Standard addrWidth dat)
packetStreamToWishbone = Circuit (unbundle . mealy go False . bundle)
  where
    go :: ("CYC" ::: Bool)
      -> (Maybe (PacketStreamM2S dataWidth meta), WishboneS2M dat)
      -> ("CYC" ::: Bool, (PacketStreamS2M, WishboneM2S addrWidth (ByteSize dat) dat))
    go cyc (psFwd, wbBwd) = (nextCyc, (PacketStreamS2M psBwd, wbFwd))
      where
        nextCyc = case psFwd of
          Nothing -> cyc
          Just x  -> isNothing (_last x)

        term = acknowledge wbBwd || err wbBwd || retry wbBwd
        psBwd = term && not (stall wbBwd)

        dat :: dat
        dat = case psFwd of
          Nothing -> unpack 0
          Just x  -> bitCoerce (_data x)

        wbFwd = WishboneM2S
          { addr = 0
          , writeData = dat
          , busSelect = oneBits
          , lock = False
          , busCycle = isJust psFwd || cyc
          , strobe = isJust psFwd
          , writeEnable = True
          , cycleTypeIdentifier = Classic
          , burstTypeExtension = LinearBurst
          }


-- Final circuit
-- myCircuit
--   :: forall dom dataWidth addrWidth dat .
--   ( HiddenClockResetEnable dom
--   , KnownNat dataWidth
--   , KnownNat addrWidth
--   , 4 <= dataWidth
--   , addrWidth <= dataWidth * 8
--   , BitPack dat
--   , BitSize dat ~ dataWidth * 8
--   , NFDataX dat
--   , Show dat
--   , ShowX dat
--   )
--   => Circuit (PacketStream dom dataWidth ())
--              ( PacketStream dom dataWidth ()
--              , Wishbone dom Standard addrWidth dat)
-- myCircuit = circuit $ \pm -> do
--   ebpkt <- etherboneDepacketizerC -< pm
--
--   -- Filters out invalid, probe and record packets
--   [probe, record] <- prefixName @"Recv" receiverC -< ebpkt
--
--   probeOut <- prefixName @"Probe" probeHandlerC -< probe
--   
--   [recordA, recordB] <- fanout -< record
--
--   recordDpkt <- traceC "ProcIn " <| recordDepacketizerC -< recordB
--   (procOut, wbmIn) <- prefixName @"Processor" recordProcessorC -< (recordDpkt, wbmDat)
--   (wbmDat, wbmBus, wbmErr) <- wishboneMasterC -< wbmIn
--
--   signalSink -< wbmErr
--
--   procOut' <- traceC "ProcOut" -< procOut
--   rpkt <- recordBuilderC -< (procOut', recordA)
--
--   resp <- arbiterC -< [rpkt, probeOut]
--
--   udpTx <- etherbonePacketizerC -< resp
--
--   idC -< (udpTx, wbmBus)
--   -- etherbonePacketizerC -< ebpkt
  --
  -- where
  --   signalSink :: Circuit (CSignal dom a) ()
  --   signalSink = Circuit go
  --   go :: (Signal dom a, ()) -> (Signal dom (), ())
  --   go _ = (pure (), ())


-- wbCircuit
--   :: forall dom addrWidth dat .
--   ( HiddenClockResetEnable dom
--   , KnownNat addrWidth
--   , BitPack dat
--   , Bits dat
--   , NFDataX dat
--   , Show dat
--   , ShowX dat
--   , BitSize dat ~ ByteSize dat * 8
--   , 4 <= ByteSize dat
--   , addrWidth <= BitSize dat
--   )
--   => Circuit (Wishbone dom Standard addrWidth dat)
--              (Wishbone dom Standard addrWidth dat)
-- wbCircuit = circuit $ \rx_i -> do
--   rx_ps <- wishboneToPacketStream @_ @_ @(ByteSize dat) -< rx_i
--   (tx_ps, wbBus) <- myCircuit @_ @_ @addrWidth -< rx_ps
--   tx_o <- packetStreamToWishbone -< tx_ps
--
--   wishboneScratchpad @_ @dat d4 -< wbBus
--
--   idC -< tx_o

type DataWidth = 4
type AddrWidth = 32

fullCircuit :: forall dom .
  ( HiddenClockResetEnable dom )
  => Circuit (PacketStream dom DataWidth ())
            (PacketStream dom DataWidth ())
fullCircuit = circuit $ \rx -> do
  (tx, wbBus) <- etherboneC -< rx
  wishboneScratchpad @_ @(Unsigned 32) @AddrWidth d4 -< wbBus
  idC -< tx

topEntity
  :: "clk"  ::: Clock System
  -> "rst" ::: Reset System
  -> "en"   ::: Enable System
  -> "rx_i" ::: Signal System (Maybe (PacketStreamM2S DataWidth ()))
  -> "tx_i" ::: Signal System PacketStreamS2M
  -> ( "rx_o" ::: Signal System PacketStreamS2M
     , "tx_o" ::: Signal System (Maybe (PacketStreamM2S DataWidth ()))
     )
topEntity clk rst en rx_i tx_i = fn
  where
    fn = (rx_o, tx_o)
      where
        (rx_o, tx_o) = toSignals circ (rx_i, tx_i)
    circ = exposeClockResetEnable fullCircuit clk rst en



-- 'read_scratch00': ('4e6f1044a00f00010000800000000000e80f00010000800100000004',
topInput :: [Maybe (PacketStreamM2S DataWidth ())]
topInput = 
  [ pkt Nothing False
  , pkt Nothing False
  , pkt (Just 0x4e6f11ff) False
  , pkt (Just 0x00000086) True
  , pkt Nothing False
  -- Two Writes
  --, pkt (Just 0x4e6f1044) False
  , pkt (Just 0x4e6f1044) False
  , pkt (Just 0x280f0200) False
  , pkt (Just 0x00008000) False
  , pkt (Just 0x00000004) False
  , pkt (Just 0x00000008) True
  , pkt Nothing False
  , pkt Nothing False
  -- Write then read
  --, pkt (Just 0x4e6f1044) False
  , pkt (Just 0x4e6f1044) False
  , pkt (Just 0x280f0203) False
  , pkt (Just 0x00000000) False
  , pkt (Just 0xdeadbeef) False
  , pkt (Just 0xcafecafe) False
  , pkt (Just 0x00008000) False
  , pkt (Just 0x00000000) False
  , pkt (Just 0x00000000) False
  , pkt (Just 0x00000004) True
  ]
  where
    pkt
      :: Maybe (BitVector 32) -> Bool
      -> Maybe (PacketStreamM2S DataWidth ())
    pkt Nothing _ = Nothing
    pkt (Just x) isLast = Just ps
      where
        ps = PacketStreamM2S (bitCoerce x ++ repeat 0) lst () False
        lst
          | isLast    = Just 4
          | otherwise = Nothing

simTop
  :: [Maybe (PacketStreamM2S DataWidth ())]
  -> [Maybe (PacketStreamM2S DataWidth ())]
simTop = simulateC (withClockResetEnable @System clockGen resetGen enableGen fullCircuit) config
-- simTop = simulate @System (bundle . top . unbundle)
--   where
--     top (a, b) = (hideClockResetEnable topEntity) a b

fmtPacketStream :: (KnownNat dataWidth, Show meta) => Maybe (PacketStreamM2S dataWidth meta) -> String
fmtPacketStream Nothing = "-"
fmtPacketStream (Just PacketStreamM2S{..}) = 
  "data: 0x" <> bv2hex (pack _data) <>
  ", last: " <> show (isJust _last) <>
  ", meta: " <> show _meta

bv2hex :: (KnownNat n) => BitVector n -> String
bv2hex bv = showHex bv ""

-- mapM_ putStrLn $ P.map fmtPacketStream $ P.take 32 $ simTop topInput


-- mapM_ print $ L.take 32 $ simulateC (withClockResetEnable @System clockGen resetGen enableGen (myCircuit)) config streamInput


-- topEntity
--   :: "clk"  ::: Clock System
--   -> "rst" ::: Reset System
--   -> "en"   ::: Enable System
--   -> "rx_i" ::: Signal System (WishboneM2S AddrWidth DataWidth WBData)
--   -> "tx_i" ::: Signal System (WishboneS2M WBData)
--   -> ( "rx_o" ::: Signal System (WishboneS2M WBData)
--      , "tx_o" ::: Signal System (WishboneM2S AddrWidth DataWidth WBData)
--      )
-- topEntity clk rst en rx_i tx_i = fn
--   where
--     fn = (rx_o, tx_o)
--       where
--         (rx_o, tx_o) = toSignals circ (rx_i, tx_i)
--     circ = exposeClockResetEnable wbCircuit clk rst en

makeTopEntity 'topEntity

{-
topInput :: [(WishboneM2S AddrWidth DataWidth WBData, WishboneS2M WBData)]
topInput =
  [ pkt Nothing False False
  , pkt Nothing False False
  , pkt (Just 0x4e6f11ff) True False
  , pkt (Just 0x4e6f11ff) True False
  , pkt (Just 0x00000086) True False
  , pkt (Just 0x00000086) True False
  , pkt (Just 0x00000086) True True
  , pkt (Just 0x00000086) True False
  , pkt (Just 0x00000086) True False
  , pkt (Just 0x00000086) True False
  , pkt (Just 0x00000086) True False
  , pkt (Just 0x00000086) True False
  ]
  where
    pkt
      :: Maybe WBData
      -> Bool -> Bool
      -> (WishboneM2S AddrWidth DataWidth WBData, WishboneS2M WBData)
    pkt Nothing  _ ack = (emptyWishboneM2S{busCycle=True}, emptyWishboneS2M {acknowledge=ack})
    pkt (Just x) stb ack =
      ( (emptyWishboneM2S @_ @WBData) {writeData=x, writeEnable=True, strobe=stb, busCycle=True}
      , emptyWishboneS2M {acknowledge=ack})

simTop
  :: [(WishboneM2S AddrWidth DataWidth WBData, WishboneS2M WBData)]
  -> [(WishboneS2M WBData, WishboneM2S AddrWidth DataWidth WBData)]
simTop = simulate @System (bundle . top . unbundle)
  where
    top (a, b) = (hideClockResetEnable topEntity) a b
-}

-- packet :: Bool -> Maybe (BitVector 32) -> Maybe (PacketStreamM2S 4 ())
-- packet _ Nothing  = Nothing
-- packet l (Just x) = Just $ PacketStreamM2S (bitCoerce x) lastIndex () False
--   where lastIndex = if l then Just 3 else Nothing
--
--
-- streamInput :: [Maybe (PacketStreamM2S 4 ())]
-- streamInput = 
--   [ packet False $ Just 0x4e6f11ff
--   , packet True  $ Just 0x00000086
--   , Nothing
--   , packet False $ Just 0x4e6f1044
--   , packet False $ Just 0xe80f0100
--   , packet False $ Just 0x11
--   , packet True $ Just 0x22
--   -- , packet True  $ Just 0x00
--   , Nothing
--   , packet False $ Just 0x4e6f1044
--   , packet True $ Just 0xe80f0000
--   --, packet False $ Just 0xcc
--   --, packet True  $ Just 0xdd
--   , Nothing
--   , packet False $ Just 0x4e6f1044
--   , packet False $ Just 0xe80f0100
--   , packet False $ Just 0xcc
--   , packet True  $ Just 0xdd
--   , Nothing
--   , packet True $ Just 0x00
--   , packet True $ Just 0x00
--   , packet False $ Just 0x4e6f1144
--   , packet False $ Just 0xbb
--   , packet False $ Just 0xcc
--   , packet True  $ Just 0xdd
--   , packet False $ Just 0x4e6f1044
--   , packet False $ Just 0xe80f0101
--   , packet False $ Just 0xaaaaaaaa
--   , packet False $ Just 0xbbbbbbbb
--   , packet False $ Just 0xCCCCCCCC
--   , packet True  $ Just 0xDDDDDDDD
--   , Nothing
--   , Nothing
--   , Nothing
--   , Nothing
--   , Nothing
--   , Nothing
--   , Nothing
--   , Nothing
--   , Nothing
--   , Nothing
--   ]

-- main :: IO ()
-- main = do
--   let out = topEntity systemClockGen 
  


-- Simulate with
-- mapM_ print $ L.take 32 $ simulateC (withClockResetEnable @System clockGen resetGen enableGen (myCircuit)) config streamInput
-- or with
-- simulate (bundle . (hideClockResetEnable topEntity) . unbundle) streamInput


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
