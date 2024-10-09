{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tests.TestPacketStream where

import qualified Prelude as P
import Clash.Prelude

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog

import Protocols.Hedgehog
-- import Protocols.PacketStream.Hedgehog

import TestPacketStream
import Clash.Hedgehog.Sized.BitVector (genBitVector)
import Protocols
import Protocols.PacketStream(PacketStream)
import Protocols.Df(Df)
import qualified Protocols.PacketStream as PS
import qualified Protocols.Df as Df
import Protocols.PacketStream.Hedgehog
import qualified Data.List as L
import qualified Data.Bifunctor as B
import Data.Maybe
import Debug.Trace

-- prop_fail :: Property
-- prop_fail = property $ do
--   n :: Int <- forAll $ Gen.integral Range.constantBounded
--   assert $ n < 10 -- 0 === 1

genRecordBuilder ::
  Gen (ExpectType (PacketStream System 4 RecordHeader, Df System RecordHeader))
genRecordBuilder = do
  _wCount :: Unsigned 8 <- Gen.integral Range.linearBounded
  _rCount :: Unsigned 8 <- Gen.integral Range.linearBounded
  _bca <- Gen.bool
  _rca <- Gen.bool
  _rff <- Gen.bool
  _res0 <- boolToBit <$> Gen.bool
  _cyc <- Gen.bool
  _wca <- Gen.bool
  _wff <- Gen.bool
  _res1 <- boolToBit <$> Gen.bool
  -- _byteEn <- genBitVector
  let
    _byteEn = 0xff
    hdr = RecordHeader
      { _bca
      , _rca
      , _rff
      , _res0
      , _cyc
      , _wca
      , _wff
      , _res1
      , _byteEn
      , _wCount
      , _rCount
      }
    nPsOps = 1 + fromIntegral _wCount + fromIntegral _rCount +
      if _wCount > 0 then 1 else 0 +
      if _rCount > 0 then 1 else 0

  psOps <- genValidPacket (pure hdr) (Range.singleton (nPsOps - 1)) NoAbort

  pure (psOps, [hdr])

prop_genRecordBuilder :: Property
prop_genRecordBuilder = property $ do
  (psOps, df) <- forAll genRecordBuilder
  let
    wCount = _wCount $ L.head df
    rCount = _rCount $ L.head df

  footnote ("rCount:" <> show rCount)
  footnote ("wCount:" <> show wCount)
  footnote ("psLength:" <> show (L.length psOps))
  assert (L.length df == 1)
  assert (L.length psOps == (1 + fromIntegral wCount + fromIntegral rCount +
                             if wCount > 0 then 1 else 0 +
                             if rCount > 0 then 1 else 0))

processPackets
  :: (KnownNat dataWidth, 4 <= dataWidth)
  => RecordBuilderState  -- Initial state
  -> [((Maybe (PS.PacketStreamM2S dataWidth RecordHeader), Df.Data RecordHeader), PS.PacketStreamS2M)]  -- List of inputs
  -> [( (PS.PacketStreamS2M, Ack)
      , Maybe (PS.PacketStreamM2S dataWidth EBHeader))]  -- List of outputs
processPackets initialState inputs = snd $ L.foldl f (initialState, []) inputs
  where
    f (state, results) input = (newState, results L.++ [result])
      where
        (newState, result) = recordBuilderT state input


{- Needs to be updated. Was with Df rather than PS
prop_recordBuilderC :: Property
prop_recordBuilderC = property $ do
  (psOps, df) <- forAll genRecordBuilder
  let
    psOps' = L.map Just psOps
    as = L.zip psOps' (L.map Df.Data df L.++ L.repeat Df.NoData)
    bs = L.repeat $ PS.PacketStreamS2M True
    --res = processPackets @4 BuilderInit $ L.zip as bs

    flatten :: [PS.PacketStreamM2S 4 meta] -> [BitVector 8]
    flatten = P.foldl fn []
      where
        fn res PS.PacketStreamM2S{_data} = res P.++ toList _data

    res = P.foldl fn (BuilderInit, []) $ P.zip as bs
    fn (state, results) input = (newState, results P.++ [result])
      where
        (newState, result) = recordBuilderT @4 state input

    flattenedRes = flatten $ P.map (fromMaybe undefined . snd) (snd res)

    model
      :: [PS.PacketStreamM2S 4 RecordHeader]
      -> RecordHeader
      -> [BitVector 8]
    model ops hdr = zeros P.++ txHdr P.++ values
      where
        zeroCount  = if wCount > 0 then wCount + 1 else 0
        wCount = _wCount hdr

        zeros  = trace ("zeroCount: " <> show zeroCount)
          P.replicate (fromIntegral zeroCount * 4) (0b00000000 :: BitVector 8)
        values = flatten $ P.drop (fromIntegral $ 1 + zeroCount) ops

        txHdr :: [BitVector 8]
        txHdr = toList $ bitCoerce $ recordRxToTx hdr

    modelRes = model psOps (P.head df)
  footnote(show $ P.head df)
  footnote("psOps: " <> show psOps)
  footnote("psOps len: " <> show (P.length psOps * 4) <> ", " <> "res len: " <> show (P.length flattenedRes))
  footnote("flattenedRes: " <> show flattenedRes)
  footnote("model:        " <> show modelRes)

  assert(P.length flattenedRes == P.length modelRes)
  assert(P.length flattenedRes == P.length psOps * 4)
  flattenedRes === model psOps (P.head df)
-}

tests :: TestTree
tests = $(testGroupGenerator)
