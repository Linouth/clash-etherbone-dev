{-# LANGUAGE RecordWildCards #-}

module Etherbone.RecordProcessor where

import Clash.Prelude
import Protocols
import Protocols.PacketStream

import Etherbone.Base
import Etherbone.WishboneMaster (WishboneMasterInput (..), ByteSize, WBData)
import qualified Protocols.Df as Df
import qualified Data.Bifunctor as B
import qualified Prelude as P
import Data.Maybe
import Debug.Trace


data RecordProcessorState addrWidth
  = WriteOrReadAddr
  | Write { _writesLeft :: Unsigned 8
          , _addr :: BitVector addrWidth }
  | ReadAddr
  | Read  { _readsLeft :: Unsigned 8 }
  | Aborted
  deriving (Generic, NFDataX, Show, ShowX)


recordProcessorT :: forall addrWidth dataWidth dat .
  ( KnownNat dataWidth
  , KnownNat addrWidth
  , BitPack dat
  , BitSize dat ~ dataWidth * 8
  , addrWidth <= BitSize dat
  )
  => RecordProcessorState addrWidth
  -> ( ( Maybe (PacketStreamM2S dataWidth RecordHeader)
       , Df.Data dat
       )
     , (PacketStreamS2M, ())
     )
  -> ( RecordProcessorState addrWidth
     , ( (PacketStreamS2M, Ack)
       , ( Maybe (PacketStreamM2S dataWidth RecordHeader)
         , Maybe (WishboneMasterInput addrWidth (ByteSize dat) dat)
         )
       )
     )
recordProcessorT state ((Nothing, _), _)
  = (state, ((PacketStreamS2M True, Ack False), (Nothing, Nothing)))
recordProcessorT state ((Just psIn, df), (bpOut@PacketStreamS2M{_ready}, ()))
  = (nextState, ((PacketStreamS2M bpIn, Ack ack), (psOut, wbOut)))
  where
    -- Do _not_ progress to the next state if we receive backpressure.
    -- If the WBM is still busy with an operation, `fsm` does not progress the
    -- state.
    nextState = if _ready then state' else trace (show state) state
    state' = fsm state psIn df

    psWord = pack (_data psIn)

    -- Whenever the WBM is busy, give backpressure.
    -- Otherwise forward the backpressure from right to left
    --
    -- If WBM provides Data but we receive backpressure, we should forward this
    -- backpressure as otherwise words will be lost.
    bpIn = case (state, df) of
      (Write _ _, Df.NoData) -> False
      (Read _,    Df.NoData) -> False
      -- ? Is there backpressure missing for the one cycle where _abort is set
      -- high? It can happen that that also has _last set and then we will miss
      -- it.
      -- TODO: Fix, or think of this in the new implementation.
      (Aborted,   Df.NoData) -> False
      _                      -> _ready

    -- Only Ack if we do not receive backpressure
    -- This results in the WBM waiting until the pipeline is ready again.
    ack = case (state, df) of
      (Write _ _, Df.Data _) -> _ready
      (Read _,    Df.Data _) -> _ready
      (Aborted,   Df.Data _) -> True
      _                      -> False

    -- Should only send a fragment if
    --   BaseRetAddr is being read
    --   or Read operation from WBM is finished
    --
    -- If the WBM is reading, is first busy and then finishes while
    -- we are receiving backpressure, the output fragment changes from Nothing
    -- to the read data.
    psOut = case (state, state', df) of
      -- BaseRetAddr is being handled
      (WriteOrReadAddr, Read _, _) -> Just $ psIn { _data = unpack psWord }
      (ReadAddr, _, _)             -> Just $ psIn { _data = unpack psWord }
      -- Read op finished
      (Read _, _, Df.Data d)       -> Just $ psIn { _data = bitCoerce d }
      _                            -> Nothing

    -- If the drop cyc flag is set in the RecordHeader, the busCycle line should
    -- be deasserted after the last entry in the record.
    wbOut = case state of
      -- dropCyc takes i + _rCount, since cyc should be kept high after a write
      -- if there are still reads to do.
      Write i a -> Just $ WishboneMasterInput a (Just dat) sel (dropCyc $ i + _rCount (_meta psIn))
      Read i    -> Just $ WishboneMasterInput addr Nothing sel (dropCyc i)
      _         -> Nothing
      where
        dat = bitCoerce (_data psIn)
        sel = resize $ _byteEn (_meta psIn)

        -- Drop cyc if
        --   This is the last packet (safety measure)
        --   or
        --   This is the last entry from this record, and the dropCyc flag is
        --   set in the record header.
        -- TODO: MultiRecord: Once multiple records are handled, check the EB packet last
        --  flag rather than the current record packet's _last flag.
        dropCyc i = isJust (_last psIn) || (i == 1 && _cyc (_meta psIn))

        addr :: BitVector addrWidth
        addr = resize psWord

    fsm
      :: RecordProcessorState addrWidth
      -> PacketStreamM2S dataWidth RecordHeader
      -> Df.Data dat
      -> RecordProcessorState addrWidth
    fsm _ PacketStreamM2S{_abort = True} _ = Aborted
    fsm st@WriteOrReadAddr (PacketStreamM2S{..}) _ = st'
      where
        wCount = _wCount _meta
        rCount = _rCount _meta

        st'
          | isJust _last = WriteOrReadAddr
          | wCount > 0 = Write wCount $ resize psWord
          | rCount > 0 = Read rCount
          | otherwise = st

    fsm st@Write{} _ Df.NoData = st
    fsm Write{..} (PacketStreamM2S{..}) (Df.Data _) = st'
      where
        wCount = _writesLeft
        wCount' = wCount - 1
        rCount = _rCount _meta

        addr'
          | _wff _meta = _addr
          | otherwise  = _addr + (natToNum @dataWidth)

        st'
          | isJust _last = WriteOrReadAddr
          | wCount' == 0 = if rCount == 0 then WriteOrReadAddr else ReadAddr
          | otherwise = Write wCount' addr'

    fsm ReadAddr (PacketStreamM2S{..}) _ = st'
      where
        rCount = _rCount _meta

        st'
          | isJust _last = WriteOrReadAddr
          | rCount > 0 = Read rCount
          | otherwise  = WriteOrReadAddr

    fsm st@Read{} _ Df.NoData = st
    fsm Read{..} (PacketStreamM2S{..}) (Df.Data _) = st'
      where
        rCount = _readsLeft
        rCount' = rCount - 1

        st'
          | isJust _last = WriteOrReadAddr
          | rCount' == 0 = WriteOrReadAddr
          | otherwise    = Read rCount'
    fsm st@Aborted PacketStreamM2S{} Df.NoData = st
    fsm st@Aborted PacketStreamM2S{..} (Df.Data _) = st'
      where
        st'
          | isJust _last = WriteOrReadAddr
          | otherwise = st


recordProcessorC ::
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , KnownNat addrWidth
  , BitPack dat
  , BitSize dat ~ dataWidth * 8
  , addrWidth <= BitSize dat
  )
  => Circuit ( PacketStream dom dataWidth RecordHeader
             , Df dom dat)
             ( PacketStream dom dataWidth RecordHeader
             , CSignal dom (Maybe (WishboneMasterInput addrWidth (ByteSize dat) dat)))
recordProcessorC = Circuit (B.bimap unbundle unbundle . fsm . B.bimap bundle bundle)
  where
    fsm (fwd, bwd) = mealyB recordProcessorT WriteOrReadAddr (fwd, bwd)


type RpInput dataWidth dat = ( ( Maybe (PacketStreamM2S dataWidth RecordHeader)
                               , Df.Data dat
                               )
                             , (PacketStreamS2M, ()) )

type RpOutput addrWidth dataWidth dat = ( (PacketStreamS2M, Ack)
                                        , ( Maybe (PacketStreamM2S dataWidth RecordHeader)
                                          , Maybe (WishboneMasterInput addrWidth (ByteSize dat) dat)
                                          ) )

rpSim :: forall addrWidth dataWidth dat .
  ( KnownNat addrWidth
  , KnownNat dataWidth
  , BitPack dat
  , BitSize dat ~ dataWidth * 8
  , addrWidth <= dataWidth * 8
  )
  => [RpInput dataWidth dat]
  -> ([RecordProcessorState addrWidth], [RpOutput addrWidth dataWidth dat])
rpSim inputs = P.foldl fn ([WriteOrReadAddr], []) inputs
  where
    fn :: ( [RecordProcessorState addrWidth]
          , [RpOutput addrWidth dataWidth dat])
       -> RpInput dataWidth dat
       -> ( [RecordProcessorState addrWidth]
          , [RpOutput addrWidth dataWidth dat])
    fn (states, results) input = (states P.++ [newState], results P.++ [result])
      where
        (newState, result) = recordProcessorT (P.last states) input

rpInput :: [RpInput (ByteSize WBData) WBData]
rpInput = [ pkt 1 0 False True 0xaaaaaaaa Nothing
          , pkt 1 0 False True 0xdeadbeef Nothing
          , pkt 1 0 False True 0xdeadbeef Nothing
          , pkt 1 0 True  True 0xdeadbeef (Just 0)
          , pkt 0 0 True  True 0x00000000 Nothing
          , pkt 0 2 False True 0xbbbbbbbb Nothing
          , pkt 0 2 False True 0xcccccccc Nothing
          , pkt 0 2 False True 0xcccccccc Nothing
          , pkt 0 2 False True 0xcccccccc Nothing
          , pkt 0 2 False True 0xcccccccc Nothing
          , pkt 0 2 False False 0xcccccccc (Just 0xcafecafe)
          , pkt 0 2 False True 0xcccccccc (Just 0xcafecafe) -- Here fst is done
          -- Single cycle second read
          , pkt 0 2 True  True 0xdddddddd (Just 0xffffffff)
          ]
  where
    pkt
      :: Unsigned 8
      -> Unsigned 8
      -> Bool
      -> Bool
      -> WBData
      -> Maybe WBData
      -> RpInput (ByteSize WBData) WBData
    pkt wCount rCount lastFrag bp psDat wbRet =
      ( (Just ps, Df.maybeToData wbRet)
      , (PacketStreamS2M bp, ())
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

-- Run with
-- mapM_ putStrLn [show x P.++ "\n\t" P.++ show y | (x, y) <- uncurry P.zip (rpSim @32 rpInput)]
