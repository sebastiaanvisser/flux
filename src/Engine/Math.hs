module Engine.Math where

frac :: RealFrac a => a -> a
frac x = x - fromIntegral (floor x :: Integer)

pi2 :: (Floating a) => a
pi2 = 2 * pi

-- Sinus between 0 and 1 with phase between 0 to 1.
sin01 :: Floating a => a -> a
sin01 = (+1/2) . (/2) . sin . (*pi2)

class Num a => Almost a where
  almost :: a -> a
  just   :: a -> a
  just x = x + (1-almost 1)

instance Almost Float where
  almost a = a * (1 - 0.0000001)
  just   a = a * (1 + 0.0000001)

