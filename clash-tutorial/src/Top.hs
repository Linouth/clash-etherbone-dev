
--{-# LANGUAGE NamedFieldPuns #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE DeriveFoldable, DeriveFunctor #-}

{-# OPTIONS -fplugin=Protocols.Plugin #-}


module Top where

import Data.Maybe (fromMaybe)
import Data.Bool (bool)
import qualified Data.List as L
import qualified Data.Char as Char
import Data.Proxy

import Clash.Prelude
import Clash.Annotations.TH
import Protocols
import Protocols.Internal
import Protocols.Wishbone
import Protocols.Idle

import Scratchpad hiding (topEntity)

{- 
  type     t_sdb_device is record
    abi_class     : std_logic_vector(15 downto 0);
    abi_ver_major : std_logic_vector(7 downto 0);
    abi_ver_minor : std_logic_vector(7 downto 0);
    wbd_endian    : std_logic;          -- 0 = big, 1 = little
    wbd_width     : std_logic_vector(3 downto 0);  -- 3=64-bit, 2=32-bit, 1=16-bit, 0=8-bit
    sdb_component : t_sdb_component;
  end record t_sdb_device;

  type t_sdb_product is record
    vendor_id : std_logic_vector(63 downto 0);
    device_id : std_logic_vector(31 downto 0);
    version   : std_logic_vector(31 downto 0);
    date      : std_logic_vector(31 downto 0);
    name      : string(1 to 19);
  end record t_sdb_product;

  type t_sdb_component is record
    addr_first : std_logic_vector(63 downto 0);
    addr_last  : std_logic_vector(63 downto 0);
    product    : t_sdb_product;
  end record t_sdb_component;

    abi_class     => x"0000",              -- undocumented device
    abi_ver_major => x"01",
    abi_ver_minor => x"01",
    wbd_endian    => c_sdb_endian_big,
    wbd_width     => x"7",                 -- 8/16/32-bit port granularity
    sdb_component => (
      addr_first  => x"0000000000000000",
      addr_last   => x"000000000000000f",
      product     => (
        vendor_id => x"00000000DEADBEEF",
        device_id => x"ff07fc47",
        version   => x"00000001",
        date      => x"20120305",
        name      => "Scratchpad         ")));
 -}
data SdbDevice = SdbDevice
  { abiClass    :: BitVector 16
  , abiVerMajor :: Unsigned 8
  , abiVerMinor :: Unsigned 8
  , endian      :: Bit          -- 0 = big, 1 = little
  , width       :: Unsigned 4

  , addrFirst   :: BitVector 64
  , addrLast    :: BitVector 64

  , vendorId    :: BitVector 64
  , deviceId    :: BitVector 32
  , version     :: Unsigned 32
  , date        :: BitVector 32
  , name        :: Vec 19 (BitVector 8)  -- String
  } deriving (Generic, Show, BitPack)

-- sdbDevice2bv :: SdbDevice -> BitVector 477
-- sdbDevice2bv SdbDevice{..}
--   =   pack abiClass ++# pack abiVerMajor ++# pack abiVerMinor ++# pack endian ++# pack width
--   ++# pack addrFirst ++# pack addrLast
--   ++# pack vendorId ++# pack deviceId ++# pack version ++# pack date ++# pack name

paddedSdb :: SdbDevice -> BitVector 480
paddedSdb x = pack x ++# zeroBits

-- sdbToVec :: SdbDevice -> Vec 15 (BitVector 32)
-- sdbToVec SdbDevice{..} = 

charConvert :: Char -> BitVector 8
charConvert = bitCoerce . resize . pack . Char.ord

scratchpadSdb :: SdbDevice
scratchpadSdb = SdbDevice 0x0000 0x01 0x01 0 0x4 0x0 0xf 0xDEADBEEF 0xff07fc47 0x1 0x20120305 name
  where
    name = fmap charConvert $(listToVecTH "Scratchpad         ")

-- romData :: (KnownNat n) => Vec n (BitVector 8)
romData :: Vec 15 (BitVector 32)
romData = unpack $ paddedSdb scratchpadSdb


myROM :: (HiddenClockResetEnable dom, KnownNat addrWidth)
  => Signal dom (BitVector addrWidth)
  -> Signal dom WBWord
myROM adr = rom (unpack <$> romData) $ unpack <$> adr

myROMWb :: forall dom addrWidth a .
  (HiddenClockResetEnable dom, a ~ WBWord, KnownNat addrWidth)
  => Circuit (Wishbone dom Standard addrWidth a) ()
myROMWb = fromSignals circ
  where
    circ (wb, _) = (liftA2 out dat ack, ())
      where
        ack = (busCycle <$> wb) .&&. (strobe <$> wb)
        adr = addr <$> wb
        dat = myROM $ liftA2 shiftR adr 2

        out x ackn = (emptyWishboneS2M @a) { readData=x, acknowledge=ackn }


-- For now a new peripheral hard wired to `val` as ROM
hardwiredWb :: forall dom addrWidth a .
  (Num a, NFDataX a)
  => a -> Circuit (Wishbone dom Standard addrWidth a) ()
hardwiredWb val = fromSignals circ
  where
    circ (wb, _) = (out <$> ack, ())
      where
        ack = (busCycle <$> wb) .&&. (strobe <$> wb)
    out ack = (emptyWishboneS2M @a) { readData=val, acknowledge=ack }

myCircuit
  :: forall dom . (HiddenClockResetEnable dom)
  => Circuit (Wishbone dom 'Standard 32 WBWord) ()
myCircuit = singleMasterInterconnect (0b0 :> 0b1 :> Nil) |> circuits |> idleSink 
  where
    -- dropUnits = mapCircuit id id (const ()) (const $ repeat ())

    scratch :: Circuit (Wishbone dom 'Standard 31 WBWord) ()
    scratch = wishboneScratchpad d4

    circuits = vecCircuits $ scratch :> myROMWb :> Nil


myCircuit2
  :: forall dom . (HiddenClockResetEnable dom)
  => Circuit (Wishbone dom 'Standard 32 WBWord) ()
myCircuit2 =
  circuit $ \wbm -> do
  [wb1, wb2] <- singleMasterInterconnect @_ @_ @32 (0b0 :> 0b1 :> Nil) -< wbm
  scratch -< wb1
  myROMWb -< wb2
  where
    scratch :: Circuit (Wishbone dom 'Standard 31 WBWord) ()
    scratch = wishboneScratchpad d4


topEntity
  :: "clk"  ::: Clock System
  -> "rstn" ::: Reset System
  -> "en"   ::: Enable System
  -> "wb_i" ::: Fwd (Wishbone System Standard 32 WBWord)
  -> "wb_o" ::: Bwd (Wishbone System Standard 32 WBWord)
topEntity clk rst en = fn
  where
    fn x = fst $ toSignals circ (x, ())
    circ = exposeClockResetEnable myCircuit clk rst en


-- simMyCircuit :: _
-- simMyCircuit = simulateCS myCircuit []


-- Function / types below taken from Bittide
-- https://github.com/bittide/bittide-hardware/blob/e1ba548a2e0dadcdfa59b97bcd32991b4ee0e0d8/bittide/src/Bittide/Wishbone.hs#L49

-- | A vector of base addresses, one for each slave.
type MemoryMap nSlaves = Vec nSlaves (Unsigned (CLog 2 nSlaves))

-- | Size of a bus that results from a `singleMasterInterconnect` with `nSlaves` slaves.
type MappedBusAddrWidth addr nSlaves = addr - CLog 2 nSlaves

{- | Component that maps multiple slave devices to a single master device over the wishbone
bus. It routes the incoming control signals to a slave device based on the 'MemoryMap',
a vector of base addresses.
-}
singleMasterInterconnect ::
  forall dom nSlaves addrW a.
  ( HiddenClockResetEnable dom
  , KnownNat nSlaves
  , 1 <= nSlaves
  , KnownNat addrW
  , (CLog 2 nSlaves <= addrW)
  , BitPack a
  , NFDataX a
  ) =>
  MemoryMap nSlaves ->
  Circuit
    (Wishbone dom 'Standard addrW a)
    (Vec nSlaves (Wishbone dom 'Standard (MappedBusAddrWidth addrW nSlaves) a))
singleMasterInterconnect (fmap pack -> config) = Circuit go
 where
  go (masterS, slavesS) =
    fmap unbundle . unbundle $ route <$> masterS <*> bundle slavesS

  route master@(WishboneM2S{..}) slaves = (toMaster, toSlaves)
   where
    oneHotOrZeroSelected = fmap (== addrIndex) config
    (addrIndex, newAddr) =
      split @_ @_ @(MappedBusAddrWidth addrW nSlaves) addr
    toSlaves =
      (\newStrobe -> (updateM2SAddr newAddr master){strobe = strobe && newStrobe})
        <$> oneHotOrZeroSelected
    toMaster
      | busCycle && strobe =
          foldMaybes
            emptyWishboneS2M{err = True} -- master tries to access unmapped memory
            (maskToMaybes slaves oneHotOrZeroSelected)
      | otherwise = emptyWishboneS2M


{- | Given a vector with elements and a mask, promote all values with a corresponding
'True' to 'Just', others to 'Nothing'.

Example:

>>> maskToMaybes ('a' :> 'b' :> Nil) (True :> False :> Nil)
Just 'a' :> Nothing :> Nil
-}
maskToMaybes :: Vec n a -> Vec n Bool -> Vec n (Maybe a)
maskToMaybes = zipWith (bool Nothing . Just)

{- | Fold 'Maybe's to a single value. If the given vector does not contain any 'Just',
the default value is picked. Prefers the leftmost value when the vector contains
multiple 'Just's.

Example:

>>> foldMaybes 'a' (Nothing :> Just 'c' :> Nil)
'c'
>>> foldMaybes 'a' (Just 'b' :> Just 'c' :> Nil)
'b'
>>> foldMaybes 'a' (Nothing :> Nothing :> Nil)
'a'
-}
foldMaybes :: a -> Vec n (Maybe a) -> a
foldMaybes a Nil = a
foldMaybes dflt v@(Cons _ _) = fromMaybe dflt $ fold (<|>) v


-- | Polymorphic record update of 'addr'.
-- https://github.com/bittide/bittide-hardware/blob/e1ba548a2e0dadcdfa59b97bcd32991b4ee0e0d8/bittide/src/Bittide/SharedTypes.hs#L32
updateM2SAddr ::
  BitVector addressWidthNew ->
  WishboneM2S addressWidthOld selWidth dat ->
  WishboneM2S addressWidthNew selWidth dat
updateM2SAddr newAddr WishboneM2S{..} = WishboneM2S{addr = newAddr, ..}


makeTopEntity 'topEntity
