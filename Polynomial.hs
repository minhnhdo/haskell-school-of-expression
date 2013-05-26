module Polynomial where

newtype Poly = P [Double]
    deriving (Eq, Show)

pn :: Poly
pn = P $ 1 : 0 : 2 : (-4) : 0 : 3 : repeat 0

lift1 :: ([Double] -> [Double]) -> Poly -> Poly
lift1 f (P p) = P . f $ p

lift2 :: ([Double] -> [Double] -> [Double]) -> Poly -> Poly -> Poly
lift2 g (P p1) (P p2) = P (g p1 p2)

scale :: Double -> [Double] -> [Double]
scale a = map (*a)

addPoly :: [Double] -> [Double] -> [Double]
addPoly = zipWith (+)

subPoly :: [Double] -> [Double] -> [Double]
subPoly = zipWith (-)

mulPoly :: [Double] -> [Double] -> [Double]
mulPoly p1 p2 = addPoly (scale (head p1) p2) (0 : mulPoly (tail p1) p2)

divPoly :: [Double] -> [Double] -> [Double]
divPoly p1 p2 = s : divPoly (subPoly (tail p1) (scale s (tail p2))) p2
                where s = head p1 / head p2 

instance Num Poly where
    (+)              = lift2 addPoly
    (*)              = lift2 mulPoly
    negate           = lift1 (scale (-1))
    abs              = lift1 $ map abs
    signum (P (p:_)) = P (signum p : repeat 0)
    fromInteger x    = P (fromInteger x : repeat 0)

instance Fractional Poly where
    (/)            = lift2 divPoly
    fromRational x = P (fromRational x : repeat 0)

poly :: [Double] -> Poly
poly = P . (++ repeat 0)

takePoly :: Int -> Poly -> [Double]
takePoly n (P p) = take n p

fibonacci :: Poly
fibonacci = poly [1] / poly [1, -1, -1]
