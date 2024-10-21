{-# OPTIONS -fplugin=Protocols.Plugin #-}

module Etherbone where

import Protocols
import Protocols.PacketStream
import Protocols.Wishbone
import Etherbone.Base
import Etherbone.RecordProcessor (recordProcessorC)
import Etherbone.WishboneMaster (wishboneMasterC)
import Etherbone.RecordBuilder (recordBuilderC)
import Clash.Prelude


recordHandlerC ::
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , KnownNat addrWidth
  , BitPack dat
  , NFDataX dat
  , Show dat
  , ShowX dat
  , addrWidth <= dataWidth * 8
  , BitSize dat ~ dataWidth * 8
  , 4 <= dataWidth
  )
  => Circuit (PacketStream dom dataWidth EBHeader)
             ( PacketStream dom dataWidth EBHeader
             , Wishbone dom Standard addrWidth dat)
recordHandlerC = circuit $ \psIn -> do
  [recordBypass, record] <- fanout -< psIn
  dpkt <- recordDepacketizerC -< record

  (procOut, wbmIn) <- recordProcessorC -< (dpkt, wbmDat)
  (wbmDat, wbmBus, wbmErr) <- wishboneMasterC -< wbmIn

  procOut' <- traceC "ProcOut/BuilderIn" -< procOut
  psOut <- recordBuilderC -< (procOut', recordBypass)
  signalSink -< wbmErr

  idC -< (psOut, wbmBus)

  where
    signalSink :: Circuit (CSignal dom a) ()
    signalSink = Circuit $ const (pure (), ())


etherboneC :: forall dom dataWidth addrWidth dat .
  ( HiddenClockResetEnable dom
  , KnownNat dataWidth
  , KnownNat addrWidth
  , BitPack dat
  , NFDataX dat
  , Show dat
  , ShowX dat
  , BitSize dat ~ dataWidth * 8
  , addrWidth <= dataWidth * 8
  , 4 <= dataWidth
  )
  => Circuit (PacketStream dom dataWidth ())
             ( PacketStream dom dataWidth ()
             , Wishbone dom Standard addrWidth dat)
etherboneC = circuit $ \psIn -> do
  [probe, record] <- receiverC (SNat @addrWidth) <| etherboneDepacketizerC -< psIn

  probeOut <- probeHandlerC (SNat @addrWidth) -< probe
  (recordOut, wbmBus) <- recordHandlerC -< record

  recordOut' <- traceC "BuilderOut/ArbIn" -< recordOut

  pktOut <- arbiterC -< [recordOut', probeOut]
  udpTx <- etherbonePacketizerC <| traceC "EBPktIn" -< pktOut

  idC -< (udpTx, wbmBus)
