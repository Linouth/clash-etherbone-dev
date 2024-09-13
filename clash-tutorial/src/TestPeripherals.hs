module TestPeripherals where

import Clash.Prelude
import Protocols
import Protocols.Wishbone


type WBWord = Unsigned 32
type Bus dom = Wishbone dom Standard 32 WBWord

type M2S dom = Fwd (Bus dom)
type S2M dom = Bwd (Bus dom)


-- Passthrough
testCircuit :: Circuit (Bus dom) (Bus dom)
testCircuit = Circuit (unbundle . fmap go . bundle)
  where
    go (ms, sm) = (sm, ms)


-- Wrong approach. Does work but a Circuit (Type) () function is more idiomatic
testPeripheral :: Fwd (Bus dom) -> Bwd (Bus dom)
testPeripheral = fmap resp
  where
    resp WishboneM2S { busCycle = True, strobe = True } = sm
    resp _ = emptyWishboneS2M
    sm = (emptyWishboneS2M :: WishboneS2M WBWord) { readData=0xaaaaaaaa, acknowledge=True }


peripheralTwo :: Circuit (Bus dom) ()
peripheralTwo = Circuit (go)
  where
    go :: (Fwd (Bus dom), ()) -> (Bwd (Bus dom), ())
    go (x, _) = (pure resp <$> x, ())
    -- Why is the 'pure' required here, and not in `peripheralThree` below

    respFn WishboneM2S {busCycle = True, strobe = True } = resp
    respFn _ = emptyWishboneS2M

    resp = (emptyWishboneS2M :: WishboneS2M WBWord) { readData=0xaaaaaaaa, acknowledge=True }


-- Same as above, but making use of `fromSignals`.
peripheralThree :: Circuit (Bus dom) ()
peripheralThree = fromSignals fn
  where
    fn :: (Fwd (Bus dom), ()) -> (Bwd (Bus dom), ())
    fn (fw, _) = (resp <$> fw, ())

    resp WishboneM2S { busCycle = True, strobe = True } = sm
    resp _ = emptyWishboneS2M
    sm = (emptyWishboneS2M :: WishboneS2M WBWord) { readData=0xaaaaaaaa, acknowledge=True }


{-# OPAQUE topEntity #-}
topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Fwd (Bus System)
  -> Bwd (Bus System)
topEntity clk rst en input = exposeClockResetEnable fn clk rst en input
  where
    fn x = fst $ (toSignals circ) (x, ())
    circ = testCircuit |> peripheralThree
    

{-# ANN topEntity
  (Synthesize
    { t_name = "test_peripherals"
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
 
{- How to access Bus.Fwd and Bus.Bwd (Instead of manually defining M2S and S2M)
 -
 - How to nicely attach signal names to input and output ports (annotations?)
 - What are the annotation in the WishboneM2S and S2M definitions
 -
 - There is no mealy/moore machine required here, would it be required if the
 - output depends on the input and there is some state in the peripheral (e.g. a
 - counter)? 
 -
 - How can I now attach a peripheral to a circuit? Leave left side open (expose
 - ports as topEntity) and attach peripheral to right port
 - -}

-- Why if I place sm in testPereripheral between resps does it not compile?
