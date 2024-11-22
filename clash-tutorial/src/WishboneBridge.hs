module WishboneBridge where

import Clash.Annotations.TH (makeTopEntity)
import Clash.Cores.Etherbone.Examples.WishboneBus
import Clash.Cores.Ethernet.IPv4
import Clash.Cores.Ethernet.Udp
import Clash.Prelude
import Protocols
import Protocols.PacketStream
import Protocols.Wishbone

import Data.Maybe
import Clash.Cores.Etherbone.Examples.FullEthernetCircuit


wb2psC :: forall dom.
  ( HiddenClockResetEnable dom )
  => Circuit (Wishbone dom Standard AddrWidth WBData)
             (PacketStream dom DataWidth (IPv4Address, UdpHeaderLite))
wb2psC = Circuit go
  where
    go (wbFwd, psBwd) = (nameHint (SSymbol @"WBPSBwd") $ wbBwd <$> psBwd <*> wbFwd, nameHint (SSymbol @"WBPSFwd") $ psFwd <$> wbFwd <*> prevWb)
      where
        psFwd cur prv =
          case (sel prv, sel cur) of
            (_,    True)  -> Just $ PacketStreamM2S (bitCoerce $ writeData cur) Nothing testMeta False
            (True, False) -> Just $ PacketStreamM2S (repeat 0) (Just 0) testMeta False
            _             -> Nothing
          where
            testMeta =
              ( IPv4Address $ 0xa :> 0x0 :> 0x0 :> 0x1 :> Nil
              , UdpHeaderLite 5555 5555 8
              )
        
        wbBwd curBwd curFwd = (emptyWishboneS2M @WBData) {acknowledge = ack}
          where ack = _ready curBwd && sel curFwd

        sel i = strobe i && busCycle i
        prevWb = register @dom undefined wbFwd


data PSWBState
  = Idle {cyc :: Bool}
  | Data {ps  :: PacketStreamM2S DataWidth (IPv4Address, UdpHeaderLite)}
  | WFA  {cyc :: Bool, dat :: WBData}
  deriving (Generic, NFDataX, Show, ShowX)

ps2wbC :: forall dom .
  ( HiddenClockResetEnable dom )
  => Circuit (PacketStream dom DataWidth (IPv4Address, UdpHeaderLite))
             (Wishbone dom Standard AddrWidth WBData)
ps2wbC = Circuit $ prefixName @"pswbState" mealyB go (Idle False)
  where
    go Idle{..} (Nothing, _) = (Idle cyc, (psBwd, wbFwd))
      where
        wbFwd = wbBase { strobe=False, busCycle = cyc}
        psBwd = PacketStreamS2M True
    go Idle{..} (Just f, _) = (Data f, (psBwd, wbFwd))
      where
        wbFwd = wbBase { strobe=False, busCycle=cyc}
        psBwd = PacketStreamS2M True
    go Data{..} (_, _) = (WFA cyc dat, (psBwd, wbFwd))
      where
        wbFwd = wbBase {strobe=True, busCycle=True, writeData=dat}
        psBwd = PacketStreamS2M False
        dat = pack $ _data ps
        cyc = isNothing $ _last ps
    go WFA{..} (_, WishboneS2M{acknowledge=False}) = (WFA cyc dat, (psBwd, wbFwd))
      where
        wbFwd = wbBase {strobe=True, busCycle=True, writeData=dat}
        psBwd = PacketStreamS2M False
    go WFA{..} (_, WishboneS2M{acknowledge=True}) = (Idle cyc, (psBwd, wbFwd))
      where
        -- strobe should be true in classic, but the CocoTB tb uses pipelined
        wbFwd = wbBase {strobe=False, busCycle=True, writeData=dat}
        psBwd = PacketStreamS2M False
      
    wbBase = (emptyWishboneM2S @_ @WBData) { strobe=False, busCycle=False, writeEnable=True }
{-# NOINLINE ps2wbC #-}

simTop
  :: "clk" ::: Clock System
  -> "rst" ::: Reset System
  -> "rx_i" ::: Signal System (WishboneM2S AddrWidth DataWidth WBData)
  -> "tx_i" ::: Signal System (WishboneS2M WBData)
  -> ( "rx_o" ::: Signal System (WishboneS2M WBData)
     , "tx_o" ::: Signal System (WishboneM2S AddrWidth DataWidth WBData)
     )
simTop clk rst rx_i tx_i = (rx_o, tx_o)
  where
    (rx_o, tx_o) = toSignals ckt (rx_i, tx_i)
      where
        ckt = withClockResetEnable clk rst enableGen
          (  wb2psC
          |> packetFifoC d10 d4 Drop
          |> stripTrailingEmptyC
          |> fullCircuit
          |> packetFifoC d10 d4 Drop
          |> ps2wbC
          )

makeTopEntity 'simTop
