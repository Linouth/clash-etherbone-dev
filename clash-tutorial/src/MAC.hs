module MAC where

import Clash.Prelude
import Clash.Explicit.Testbench

ma :: Num a => a -> (a, a) -> a
ma acc (x, y) = acc + x * y

macT :: Num a => a -> (a, a) -> (a, a)
macT acc (x, y) = (acc', o)
  where
    acc' = ma acc (x, y)
    o    = acc

mac xy = mealy macT 0 xy


topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Signed 9, Signed 9)
  -> Signal System (Signed 9)
topEntity = exposeClockResetEnable mac

-- testBench :: Signal System Bool
-- testBench = done
--   where
--     testInput    = stimuliGenerator clk rst $(listToVecTH [(1,1) :: (Signed 9,Signed 9),(2,2),(3,3),(4,4)])
--     expectOutput = outputVerifier' clk rst $(listToVecTH [0 :: Signed 9,1,5,14,14,14,14])
--     done         = expectOutput (topEntity clk rst en testInput)
--     en           = enableGen
--     clk          = tbSystemClockGen (not <$> done)
--     rst          = systemResetGen

{--
dotp :: SaturatingNum a
     => Vec (n + 1) a
     -> Vec (n + 1) a
     -> a
dotp as bs = fold boundedAdd (zipWith boundedMul as bs)

fir
  :: ( HiddenClockResetEnable dom
     , KnownNat n
     , SaturatingNum a
     , NFDataX a
     , Default a )
  => Vec (n + 1) a -> Signal dom a -> Signal dom a
fir coeffs x_t = y_t
  where
    y_t = dotp coeffs <$> bundle xs
    xs  = window x_t
--}
