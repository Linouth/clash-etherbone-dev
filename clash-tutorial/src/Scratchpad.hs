module Scratchpad where

import Clash.Prelude
import Protocols
import Protocols.Wishbone
import Data.Proxy


type WBWord = Unsigned 32

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
    circ (wb, _) = (out <$> selected <*> scratchpadValue, ())
      where
        selected = (busCycle <$> wb) .&&. strobe <$> wb
        writeEnabled = selected .&&. writeEnable <$> wb

        scratchpadValue :: Signal dom a
        scratchpadValue = scratchpad $ params <$> writeEnabled <*> wb
        params we x = ( we
                      , unpack (resize index) :: Index n
                      , writeData x
                      )
          where
            bitsToShift = fromInteger $ natVal (Proxy :: Proxy (CLog 2 (BitSize a `DivRU` 8)))
            index = shiftR (addr x) bitsToShift

        -- @a here is a type application, forcing 'dat' from 'emptyWishboneS2M'
        -- to the 'a' type in this scope (so that the NFDataX constraint is set)
        out :: Bool -> a -> WishboneS2M a
        out ack val = (emptyWishboneS2M @a) { readData=val, acknowledge=ack }


-- Example for using Proxy to use type space Naturals in code
foo :: forall n. (KnownNat n) => Index n
foo = fromInteger (natVal (Proxy :: Proxy n)-1)


scratchpad
  :: (KnownNat n, HiddenClockResetEnable dom, NFDataX a, Bits a)
  => Signal dom (Bool, Index n, a)
  -> Signal dom a
scratchpad = mealy scratchpadT (repeat zeroBits)


-- n: number of registers in regfile
scratchpadT :: (KnownNat n) => Vec n a -> (Bool, Index n, a) -> (Vec n a, a)
scratchpadT pad (ok, adr, val) = (pad', el)
  where
    pad'
      | ok        = replace adr val pad
      | otherwise = pad
    el = pad !! adr


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
