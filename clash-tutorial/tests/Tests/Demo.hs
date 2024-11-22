module Tests.Demo where

import Prelude

import Hedgehog

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.Hedgehog

import Protocols.Hedgehog

import Protocols
import Protocols.PacketStream
import qualified Data.Bifunctor as B
import qualified Clash.Prelude as C
import Clash.Prelude (Vec (..))
import Clash.Cores.Ethernet.IP.IPv4Types (IPv4Address (..))
import Clash.Cores.Ethernet.Udp (UdpHeaderLite (..), swapPortsL)
import Clash.Cores.Etherbone.Examples.WishboneBus
import Clash.Cores.Etherbone.Examples.FullEthernetCircuit

pkt
  :: C.BitVector 32 -> Bool
  -> PacketStreamM2S DataWidth ()
pkt x isLast = ps
  where
    ps = PacketStreamM2S (C.bitCoerce x C.++ C.repeat 0) lst () False
    lst
      | isLast    = Just 4
      | otherwise = Nothing

testMetaMap :: PacketStreamM2S dw () -> PacketStreamM2S dw (IPv4Address, UdpHeaderLite)
testMetaMap x =
  x { _meta = testMeta }

testMeta =
  ( IPv4Address $ 0xa :> 0x0 :> 0x0 :> 0x1 :> Nil
  , UdpHeaderLite 5555 5555 16
  )

swapMap :: PacketStreamM2S dw (IPv4Address, UdpHeaderLite) -> PacketStreamM2S dw (IPv4Address, UdpHeaderLite)
swapMap x =
  x { _meta = B.second swapPortsL $ _meta x }

genInput :: Gen [PacketStreamM2S DataWidth (IPv4Address, UdpHeaderLite)]
genInput = do
  -- rCount :: C.Unsigned 8 <- Gen.integral (Range.linear 0 8)
  -- wCount :: C.Unsigned 8 <- Gen.integral (Range.linear 0 8)
  let
    dat = [
          --   pkt 0x4e6f11ff False
          -- , pkt 0x00000086 True
           pkt 0x4e6f1044 False
          , pkt 0x280f0204 False
          , pkt 0x00008000 False
          , pkt 0xaaaaaaaa False
          , pkt 0xbbbbbbbb False
          , pkt 0x00008000 False
          , pkt 0x00000000 False
          , pkt 0x00000004 False
          , pkt 0x00000008 False
          , pkt 0x0000000c True
          ]

  pure $ map testMetaMap dat

prop_fullCircuit :: Property
prop_fullCircuit = do
  idWithModelSingleDomain @C.System
    defExpectOptions
    genInput
    (C.exposeClockResetEnable model)
    (C.exposeClockResetEnable (registerBoth |> fullCircuit |> registerBoth))
  where
    model _ = map (swapMap . testMetaMap)
      [
      --   pkt 0x4e6f1644 False
      -- , pkt 0x00000086 True
       pkt 0x4e6f1444 False
      , pkt 0x00000000 False
      , pkt 0x00000000 False
      , pkt 0x00000000 False
      , pkt 0x0a0f0400 False
      , pkt 0x00008000 False
      , pkt 0xaaaaaaaa False
      , pkt 0xbbbbbbbb False
      , pkt 0x00000002 False
      , pkt 0x00000003 True
      ]

tests :: TestTree
tests = $(testGroupGenerator)

