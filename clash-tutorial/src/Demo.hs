{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Demo where

import Clash.Prelude hiding (delay)
import Clash.Cores.Ethernet.Mac
import Clash.Cores.Ethernet.IPv4
import Clash.Cores.Ethernet.Udp
import Clash.Cores.Ethernet.Examples.FullUdpStack
import Clash.Cores.Crc
import Clash.Cores.Crc.Catalog

import Clash.Explicit.Prelude (unsafeSynchronizer, delay)
import Clash.Cores.Xilinx.Ethernet.Gmii
import Clash.Xilinx.ClockGen (clockWizardDifferential)

import qualified Prelude as P
import Data.Maybe
import qualified Data.Bifunctor as B

import Scratchpad
import Protocols
import Protocols.PacketStream
import Clash.Cores.Etherbone
import Protocols.Idle
import Numeric (showHex)
import qualified Clash.Prelude as C
import Clash.Cores.Etherbone.Base (etherboneMagic)

type DataWidth = 4
type AddrWidth = 32

udpPaddingStripperC :: forall dom dataWidth .
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  )
  => Circuit (PacketStream dom dataWidth (IPv4Address, UdpHeaderLite))
             (PacketStream dom dataWidth (IPv4Address, UdpHeaderLite))
udpPaddingStripperC = Circuit $ mealyB go (0 :: Unsigned 16)
  where
    go count (Nothing,   _)
      = (count, (PacketStreamS2M True, Nothing))
    go count (Just iFwd, PacketStreamS2M oBwd)
      = (nextCount, (PacketStreamS2M iBwd, oFwd))
      where
        count' = count + dataWidth
        nextCount
          | count >= payloadSize = case _last iFwd of
            Just _ -> 0
            _      -> count
          | isJust oFwd && oBwd  = case _last iFwd of
            Just _ -> 0
            _      -> count'
          | otherwise            = count

        hdr = snd $ _meta iFwd
        payloadSize = _udplPayloadLength hdr
        dataWidth = natToNum @dataWidth

        iBwd = isNothing oFwd || oBwd

        oFwd
          | count' < payloadSize               = Just $ iFwd { _last = Nothing }
          | count' < (payloadSize + dataWidth) = Just $ iFwd { _last = Just maxBound }
          | otherwise                          = Nothing
{-# OPAQUE udpPaddingStripperC #-}


fullCircuit ::
  ( HiddenClockResetEnable dom )
  => Circuit (PacketStream dom DataWidth (IPv4Address, UdpHeaderLite))
            (PacketStream dom DataWidth (IPv4Address, UdpHeaderLite))
fullCircuit = circuit $ \rx -> do
  [rx', rxM] <- fanout -< rx

  rxS <- mapMeta (const ()) -< rx'
  (txS, wbBus) <- etherboneC -< rxS
  wishboneScratchpad @_ @(Unsigned (DataWidth*8)) @AddrWidth d4 -< wbBus

  -- idleSink -< rxM
  -- tx <- mapMeta (const testMeta) -< txS
  tx <- ethMetaBypassC -< (rxM, txS)
  idC -< tx
{-# OPAQUE fullCircuit #-}

ethMetaBypassC ::
  ( HiddenClockResetEnable dom )
  => Circuit ( PacketStream dom DataWidth (IPv4Address, UdpHeaderLite)
             , PacketStream dom DataWidth () )
             ( PacketStream dom DataWidth (IPv4Address, UdpHeaderLite))
ethMetaBypassC = Circuit $ B.first unbundle . go . B.first bundle
  where
    go = mealyB goT Nothing

    goT Nothing  ((Nothing, _), _)
      = (Nothing, ((PacketStreamS2M True, PacketStreamS2M False), Nothing))
    goT Nothing  ((Just m, _), _)
      = (st', ((PacketStreamS2M True, PacketStreamS2M False), Nothing))
      where
        st'
          | magic == etherboneMagic = Just $ B.second swapPortsL $ _meta m
          | otherwise               = Nothing

        magic = pack $ take d2 $ _data m
    goT st@(Just m) ((_, iFwd), PacketStreamS2M oBwd)
      = (st', ((PacketStreamS2M True, PacketStreamS2M iBwd), oFwd))
      where
        st'
          | oBwd && isJust (iFwd >>= _last) = Nothing
          | otherwise                       = st
        iBwd = oBwd
        metaMap x = x {_meta = m}
        oFwd = metaMap <$> iFwd
{-# OPAQUE ethMetaBypassC #-}

testMeta =
  ( IPv4Address $ 0xa :> 0x0 :> 0x0 :> 0x1 :> Nil
  , UdpHeaderLite 5555 5555 16
  )

fcDut = simulateC (withClockResetEnable @System clockGen resetGen enableGen fullCircuit) (SimulationConfig 1 maxBound False)

fcDutInput = P.map (fmap foo) $ P.map (fmap bitCoerce) dat
  where
    foo x = PacketStreamM2S x Nothing meta False
    meta =
      ( IPv4Address $ 0xa :> 0x0 :> 0x0 :> 0x1 :> Nil
      , UdpHeaderLite 6666 5555 8
      )

    dat :: [Maybe (BitVector 32)]
    dat = (P.replicate 10 Nothing) <> (Just <$> probeDat) <> P.replicate 50 Nothing
    probeDat = [0x4e6f11ff, 0x00000086]
    
-- mapM_ putStrLn $ P.take 32 $ top fcDutInput

topInput :: [Maybe (PacketStreamM2S DataWidth ())]
topInput = 
  [ pkt Nothing False
  , pkt Nothing False
  , pkt (Just 0x4e6f1144) False
  , pkt (Just 0x00000086) True
  , pkt Nothing False
  -- Two Writes
  -- , pkt (Just 0x4e6f1044) False
  , pkt (Just 0x4e6f1044) False
  , pkt (Just 0x280f0002) False
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
  , pkt Nothing False
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

fmtPacketStream :: (KnownNat dataWidth, Show meta) => Maybe (PacketStreamM2S dataWidth meta) -> String
fmtPacketStream Nothing = "-"
fmtPacketStream (Just PacketStreamM2S{..}) = 
  "data: 0x" <> bv2hex (pack _data) <>
  ", last: " <> show (isJust _last) <>
  ", meta: " <> show _meta

bv2hex :: (KnownNat n) => BitVector n -> String
bv2hex bv = showHex bv ""

mapUdp x = x { _meta = testMeta }

-- mapM_ putStrLn $ P.map fmtPacketStream $ P.take 32 $ fcDut (P.map (fmap mapUdp) topInput)


createDomain vXilinxSystem{vName="Ext125", vPeriod= hzToPeriod 125e6, vResetKind=Asynchronous}
createDomain vXilinxSystem{vName="Basic125A", vPeriod= hzToPeriod 125e6}
createDomain vXilinxSystem{vName="Basic125B", vPeriod= hzToPeriod 125e6}
createDomain vXilinxSystem{vName="Basic625", vPeriod=hzToPeriod 625e6, vResetKind=Asynchronous}

$(deriveHardwareCrc Crc32_ethernet d8 d1)
$(deriveHardwareCrc Crc32_ethernet d8 d4)


unsafeGmiiRxC :: forall domRx.
  ( HiddenClockResetEnable domRx )
  => Circuit (CSignal domRx Gmii) (PacketStream domRx 1 ())
unsafeGmiiRxC = Circuit ckt
  where
    ckt (gmiiIn, _) = (pure (), psOut <$> prevInput <*> gmiiIn)
      where
        prevInput = register @domRx gmiiDefault gmiiIn
          where gmiiDefault = Gmii 0 0 0

        psOut Gmii{gmiiValid = 0} Gmii{gmiiValid = 0} = Nothing
        psOut Gmii{gmiiValid = 1, gmiiData, gmiiError} Gmii{gmiiValid = 0}
          = Just $ PacketStreamM2S (singleton gmiiData) (Just maxBound) () (bitToBool gmiiError)
        psOut Gmii{gmiiData, gmiiError} _
          = Just $ PacketStreamM2S (singleton gmiiData) Nothing () (bitToBool gmiiError)
{-# OPAQUE unsafeGmiiRxC #-}


gmiiTxC
  :: ( HiddenClockResetEnable domTx )
  => Signal domTx Bool  -- TX ready
  -> Circuit (PacketStream domTx 1 ()) (CSignal domTx Gmii)
gmiiTxC rdy = Circuit ckt
  where
    -- Generates back-pressure while tx is not ready.
    ckt (psIn, _) = (PacketStreamS2M <$> rdy, gmiiOut <$> psIn)
      where
        gmiiOut i = Gmii (dat i) (valid i) (abort i)
        dat = maybe 0 (head . _data)
        valid = boolToBit . isJust
        abort = maybe 0 (boolToBit . _abort)
{-# OPAQUE gmiiTxC #-}


glue
  :: forall domSys domRx domTx . (KnownDomain domSys, KnownDomain domRx, KnownDomain domTx)
  => Clock domSys
  -> Reset domSys
  -> Clock domRx
  -> Reset domRx
  -> Clock domTx
  -> Reset domTx
  -> Signal domRx Gmii  -- Gmii
  -> Signal domTx Bool -- txRdy
  -> Signal domSys IPv4Address
  -> ( Signal domTx Gmii
     -- , Signal domRx (Maybe (Packet))
     )
glue clk rst rxClk rxRst txClk txRst rxGmii txRdy ipAddr = txGmii
  where
    mac = pure $ MacAddress (0xDE :> 0xAD :> 0xBE :> 0xEF :> 0x01 :> 0x02 :> Nil)
    ipSubnet = let subnet = IPv4Address (0xff :> 0xff :> 0xff :> 0x00 :> Nil)
      in bundle (ipAddr, pure subnet)

    txGmii = out
      where
        out = snd $ toSignals ckt (rxGmii, pure ())

    ethStack = (exposeClockResetEnable fullStackC clk rst enableGen) rxClk rxRst enableGen txClk txRst enableGen mac ipSubnet
    ckt = circuit $ \gmiiIn -> do
      ethPsIn <- withClockResetEnable rxClk rxRst enableGen unsafeGmiiRxC -< gmiiIn
      -- ethPsIn <- withClockResetEnable rxClk rxRst enableGen registerBoth -< psIn
      (udpIn, ethPsOut) <- ethStack -< (udpOut, ethPsIn)
      -- psOut <- withClockResetEnable txClk txRst enableGen registerBoth -< ethPsOut
      udpOut <- withClockResetEnable clk rst enableGen fullCircuit <| withClockResetEnable clk rst enableGen udpPaddingStripperC -< udpIn
      -- udpOut <- withClockResetEnable clk rst enableGen swapC <| withClockResetEnable clk rst enableGen udpPaddingStripperC -< udpIn
      gmiiOut <- withClockResetEnable txClk txRst enableGen gmiiTxC txRdy -< ethPsOut
      idC -< gmiiOut

    swapC :: Circuit (PacketStream dom 4 (IPv4Address, UdpHeaderLite))
                     (PacketStream dom 4 (IPv4Address, UdpHeaderLite))
    swapC = bimapMeta ipMap hdrMap
      where
        ipMap ip = prefixName @"UDPIP" ip
        hdrMap hdr@UdpHeaderLite{..} = hdr { _udplSrcPort = _udplDstPort
                                           , _udplDstPort = _udplSrcPort
                                           }
{-# OPAQUE glue #-}


glueEthernet
  :: Clock Basic125B
  -> Reset Basic125B
  -> DiffClock Basic625
  -> Signal Basic625 Lvds
  -> Signal Basic625 Lvds
glueEthernet sysClk sysRst sgmiiPhyClk sgmiiIn = bridgeLvdsOut
 where
  BridgeOutput{..} = bridge sgmiiIn gmiiOut
  signalDetect = pure True
  anRestart = pure False
  conf = pure def{cAutoNegEnable = True}
  anConf =
    pure
      def
        { cAcknowledge = True
        , cDuplexMode = FullDuplex
        , cLinkSpeed = Speed1000
        , cPhyLinkStatus = True
        }
  bridge = gmiiSgmiiBridge sgmiiPhyClk bridgeRst signalDetect conf anConf anRestart
  rxClk = bridgeClk125 :: Clock Basic125A
  rxRst = bridgeRst125
  bridgeRst = unsafeResetDesynchronizer sysClk sysRst
  myIp = pure $ IPv4Address $ 0xa :> 0x0 :> 0x0 :> 0x2 :> Nil
  gmiiOut = glue sysClk sysRst rxClk rxRst rxClk rxRst bridgeGmiiRx (pure True) myIp

unsafeResetDesynchronizer ::
  forall domA domS.
  (KnownDomain domA, KnownDomain domS, HasSynchronousReset domS, HasAsynchronousReset domA) =>
  -- | Clock in the source domain
  Clock domS ->
  -- | Synchronous reset in the source domain
  Reset domS ->
  -- | Asynchronous reset in the "target" domain
  Reset domA
unsafeResetDesynchronizer clkIn =
  unsafeFromActiveHigh
    . unsafeSynchronizer clkIn clockGen
    . unsafeToActiveHigh
    . delayReset Asserted clkIn

data Asserted = Asserted | Deasserted

delayReset ::
  (HasSynchronousReset dom) =>
  -- | Initial and reset value of register
  Asserted ->
  Clock dom ->
  Reset dom ->
  Reset dom
delayReset asserted clk =
  unsafeFromActiveHigh
    . delay clk enableGen assertedBool
    . unsafeToActiveHigh
 where
  assertedBool =
    case asserted of
      Asserted -> True
      Deasserted -> False


glueTop
  :: DiffClock Ext125
  -> Reset Ext125
  -> DiffClock Basic625
  -> Signal Basic625 Lvds
  -> Signal Basic625 Lvds
glueTop diffClk cpuReset sgmiiClk inp = outp
 where
  (sysClk, sysRst) = clockWizardDifferential diffClk cpuReset
  outp = glueEthernet sysClk sysRst sgmiiClk inp

  -- testStarted :: Signal Basic125B Bool
  -- testStarted = hitlVioBool sysClk testStarted (pure True)
{-# OPAQUE glueTop #-}
{-# ANN
  glueTop
  ( Synthesize
      { t_name = "glueTop"
      , t_inputs =
          [ PortProduct
              "CLK_125MHZ"
              [PortName "p", PortName "n"]
          , PortName "CPU_RESET"
          , PortProduct
              "SGMIICLK"
              [PortName "p", PortName "n"]
          , PortProduct
              "SGMII"
              [PortName "RX_p", PortName "RX_n"]
          ]
      , t_output =
          PortProduct
            "SGMII"
            [PortName "TX_p", PortName "TX_n"]
      }
  )
  #-}

