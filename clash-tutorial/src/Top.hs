module Top where

import Clash.Prelude
import Protocols
import Protocols.Wishbone
import Protocols.Wishbone.Standard

import Scratchpad


topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Fwd (Wishbone System Standard 32 WBWord)
  -> Bwd (Wishbone System Standard 32 WBWord)
topEntity clk rst en = fn
  where
    fn x = fst $ toSignals circ (x, ())
    circ = exposeClockResetEnable (wishboneScratchpad d4) clk rst en
    interc = roundrobin |> (wishboneScratchpad :> Nil)
