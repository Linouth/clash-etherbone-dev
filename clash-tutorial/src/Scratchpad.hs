module Scratchpad where

import Clash.Prelude
import Protocols
import Protocols.Wishbone
import Data.Proxy
import Debug.Trace


type WBWord = Unsigned 32

-- NOTE: Problem: There is a conditional logic loop in this circuit. It is most
-- likely because the 'ack' signal is directly dependent on the current 'strobe'
-- and 'cycle' signals. Fixing this by adding a register for 'selected' signal.
wishboneScratchpad
  :: forall dom a addrWidth n .
  ( HiddenClockResetEnable dom
  , NFDataX a
  , Bits a
  , BitPack a
  , KnownNat addrWidth
  , 1 <= n
  , 1 <= Div (BitSize a + 7) 8
  ) => SNat n -> Circuit (Wishbone dom Standard addrWidth a) ()
wishboneScratchpad SNat = fromSignals circ
  where
    circ
      :: (Signal dom (WishboneM2S addrWidth selWidth a), ())
      -> (Signal dom (WishboneS2M a), ())
    -- circ (wb, _) = (out <$> selected <*> scratchpadValue, ())
    circ (wb, _) = (out <$> prevSel <*> prevVal, ())
      where
        selected = (busCycle <$> wb) .&&. strobe <$> wb
        writeEnabled = selected .&&. writeEnable <$> wb
        
        prevSel = register False selected
        prevVal = register undefined scratchpadValue

        scratchpadValue :: Signal dom a
        scratchpadValue = scratchpad $ params <$> writeEnabled <*> wb
        params we x = ( we
                      , unpack (resize index) :: Index n
                      , writeData x
                      )
          where
            bitsToShift = natToNum @(CLog 2 (BitSize a `DivRU` 8))
            index = shiftR (addr x) bitsToShift

        -- @a here is a type application, forcing 'dat' from 'emptyWishboneS2M'
        -- to the 'a' type in this scope (so that the NFDataX constraint is set)
        out :: Bool -> a -> WishboneS2M a
        out ack val = (emptyWishboneS2M @a) { readData=val, acknowledge=ack }
{-# OPAQUE wishboneScratchpad #-}


-- Example for using Proxy to use type space Naturals in code
foo :: forall n. (KnownNat n) => Index n
foo = fromInteger (natVal (Proxy :: Proxy n)-1)


scratchpad
  :: forall n dom a . (KnownNat n, HiddenClockResetEnable dom, NFDataX a, BitPack a)
  => Signal dom (Bool, Index n, a)
  -> Signal dom a
-- scratchpad = mealy scratchpadT (repeat $ unpack 0)
scratchpad = mealy scratchpadT (map unpack $ iterateI (+1) 0)


-- n: number of registers in regfile
scratchpadT :: (KnownNat n, BitPack a) => Vec n a -> (Bool, Index n, a) -> (Vec n a, a)
scratchpadT pad (ok, adr, val) = (pad', el)
  where
    pad'
      | ok        = replace adr val pad
      | otherwise = pad
    el
      | ok        = unpack 0
      | otherwise = pad !! adr
    -- el = pad !! adr


{-# ANN topEntity
  (Synthesize
    { t_name = "scratchpad_top"
    , t_inputs = [ PortName "clk"
                 , PortName "rstn"
                 , PortName "en"
                 , PortProduct "wb_i" [ PortName "ADR"
                                      , PortName "DAT_MOSI"
                                      , PortName "SEL"
                                      , PortName "LOCK"
                                      , PortName "CYC"
                                      , PortName "STB"
                                      , PortName "WE"
                                      , PortName "CTI"
                                      , PortName "BTE"
                                      ]
                 ]
    , t_output = PortProduct "wb_o"   [ PortName "DAT_MISO"
                                      , PortName "ACK"
                                      , PortName "ERR"
                                      , PortName "STALL"
                                      , PortName "RTY"
                                      ]
    })
 #-}
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


inp ::
  (KnownNat (BitSize dat), NFDataX dat) =>
  BitVector 32 -> dat -> Bool -> Bool -> Bool -> WishboneM2S 32 (BitSize dat `DivRU` 8) dat
inp adr dat cyc stb we =
  WishboneM2S
    { addr = adr
    , writeData = dat
    , busSelect = deepErrorX "M2S busSelect not defined"
    , lock = False
    , busCycle = cyc
    , strobe = stb
    , writeEnable = we
    , cycleTypeIdentifier = Classic
    , burstTypeExtension = LinearBurst
    }


testData = [ inp 0 (10::WBWord) True True True
           , inp 0 0 True True False
           , inp 0 10 True True True
           , inp 0 20 True False True
           , inp 0 20 True True True
           , inp 0 20 True True False
           , inp 0 50 False True True
           , inp 0 99 True True False

           , inp 0x4 1 True True True
           , inp 0x8 1 True True True
           , inp 0xc 1 True True True
           , inp 0x10 2 True True True
           , inp 0x14 2 True True True
           , inp 0x18 2 True True True
           , inp 0x1c 2 True True True
           , inp 0x20 3 True True True
           ]
