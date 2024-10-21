module Etherbone.RecordBuilder where

import Clash.Prelude

import Etherbone.Base
import Protocols.PacketStream
import Protocols
import Data.Maybe
import qualified Data.Bifunctor as B
import Debug.Trace


recordRxToTx :: RecordHeader -> RecordHeader
recordRxToTx hdr = hdr { _bca = False
                       , _rca = False
                       , _rff = False
                       , _wca = _bca hdr
                       , _wff = _rff hdr
                       , _rCount = 0
                       , _wCount = _rCount hdr
                       }

ebTx :: EBHeader
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

-- Last goes through WBM channel for reads.
-- TODO: For writes it should go through the bypass-channel
-- TODO: Aborts go through the bypass-channel. Nothing comes from the WBM
-- channel on abort
recordBuilderT
  :: forall dataWidth . (KnownNat dataWidth, 4 <= dataWidth)
  => RecordBuilderState 
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
  = (BuilderInit, ( (PacketStreamS2M True, PacketStreamS2M True), Nothing ))
recordBuilderT BuilderInit ((_, Just hdr'), PacketStreamS2M{_ready})
  = (trace (show nextState) nextState, ( (PacketStreamS2M _ready, PacketStreamS2M _ready), Just out ))
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
  = (trace (show nextState) nextState, ( (PacketStreamS2M _ready, PacketStreamS2M True), Just outZeros ))
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
  = (trace (show nextState) nextState, ( (PacketStreamS2M _ready, PacketStreamS2M True), Just outHeader ))
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
        (if isLast then Just (natToNum @dataWidth) else Nothing)
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
        fn s inp = recordBuilderT s inp
