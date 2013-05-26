module Behavior where

import Picture
import Region
import Animation

newtype Behavior a = Beh (Time -> a)

animateB :: String -> Behavior Picture -> IO ()
animateB s (Beh pf) = animate s (picToGraphic . pf)

instance Eq (Behavior a) where
    a1 == a2 = error "Can't compare behaviors"

instance Show (Behavior a) where
    showsPrec n a1 = error "<< Behavior >>"

instance Num a => Num (Behavior a) where
    (+)         = lift2 (+)
    (*)         = lift2 (*)
    negate      = lift1 negate
    abs         = lift1 abs
    signum      = lift1 signum
    fromInteger = lift0 . fromInteger

instance Fractional a => Fractional (Behavior a) where
    (/)          = lift2 (/)
    fromRational = lift0 . fromRational

instance Floating a => Floating (Behavior a) where
    pi    = lift0 pi
    sqrt  = lift1 sqrt
    exp   = lift1 exp
    log   = lift1 log
    sin   = lift1 sin
    cos   = lift1 cos
    tan   = lift1 tan
    asin  = lift1 asin
    acos  = lift1 acos
    atan  = lift1 atan
    sinh  = lift1 sinh
    cosh  = lift1 cosh
    tanh  = lift1 tanh
    asinh = lift1 asinh
    acosh = lift1 acosh
    atanh = lift1 atanh

lift0 :: a -> Behavior a
lift0 x = Beh (\t -> x)

lift1 :: (a -> b) -> (Behavior a -> Behavior b)
lift1 f (Beh a) = Beh (\t -> f (a t))

lift2 :: (a -> b -> c) -> (Behavior a -> Behavior b -> Behavior c)
lift2 g (Beh a) (Beh b) = Beh (\t -> g (a t) (b t))

lift3 :: (a -> b -> c -> d) -> Behavior a -> Behavior b -> Behavior c
      -> Behavior d
lift3 g (Beh a) (Beh b) (Beh c) = Beh (\t -> g (a t) (b t) (c t))

time :: Behavior Float
time = Beh (\t -> t)

class Combine a where
    empty :: a
    over :: a -> a -> a

instance Combine Picture where
    empty = EmptyPic
    over  = Over

instance Combine a => Combine (Behavior a) where
    empty = lift0 empty
    over  = lift2 over

overMany :: Combine a => [a] -> a
overMany = foldr over empty

timeTrans :: Behavior Time -> Behavior a -> Behavior a
timeTrans (Beh f) (Beh a) = Beh (a . f)

class Turnable a where
    turn :: Float -> a -> a

instance Turnable Picture where
    turn theta (Region c r)   = Region c (turn theta r)
    turn theta (p1 `Over` p2) = turn theta p1 `Over` turn theta p2
    turn theta EmptyPic       = EmptyPic

instance Turnable a => Turnable (Behavior a) where
    turn theta (Beh b) = Beh (turn theta . b)

rotate :: Float -> Coordinate -> Coordinate
rotate theta (x, y) = (x * c + y * s, y * c - x * s)
                      where (s, c) = (sin theta, cos theta)

instance Turnable Shape where
    turn theta (Polygon ps) = Polygon (map (rotate theta) ps)

instance Turnable Region where
    turn theta (Shape sh) = Shape (turn theta sh)

instance Combine Region where
    empty = Region.Empty
    over  = Union

planets :: Behavior Picture
planets = Beh $ \t -> let p1 = Region White . Shape $ Ellipse 0.5 0.5
                          p2 = Region Green
                                . Translate (1.5 * sin t, 1.2 * cos t)
                                . Shape $ Ellipse 0.2 0.2
                          p3 = Region Yellow
                                . Translate (1.5 * sin t, 1.2 * cos t)
                                . Translate (0.6 * sin (5*t/3),
                                             0.5 * cos (5*t/3))
                                . Shape $ Ellipse 0.1 0.1
                      in p1 `Over` p2 `Over` p3
