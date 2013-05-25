module Region (Region (Shape, Translate, Scale,
                       Complement, Union, Intersect, Empty),
               Coordinate,
               containsS, containsR, fiveCircles,
               module Shape
              ) where

import Shape

data Region = Shape Shape
            | HalfPlane Coordinate Coordinate
            | Translate Vector Region
            | Scale Vector Region
            | Complement Region
            | Region `Union` Region
            | Region `Intersect` Region
            | Empty
            deriving Show

infixr 5 `Union`
infixr 6 `Intersect`

type Vector = (Float, Float)
type Coordinate = (Float, Float)
type Ray = (Coordinate, Coordinate)

oneCircle :: Region
oneCircle = Shape (Ellipse 1 1)

manyCircles :: [Region]
manyCircles = [Translate (x, 0) oneCircle | x <- [0, 2..]]

fiveCircles :: Region
fiveCircles = foldr Union Empty (take 5 manyCircles)

fiveCircles' :: Region
fiveCircles' = Translate (-1, 0) (Shape (Rectangle 10 2)) `Intersect`
                    foldr Union Empty manyCircles

listEqual :: Eq a => [a] -> Bool
listEqual []         = True
listEqual (e : es)   =
    Nothing /= foldr (\x acc ->
                        case acc of Just a -> if a == x
                                                 then Just a
                                                 else Nothing
                                    Nothing -> Nothing)
                     (Just e)
                     es

isLeftOf :: Coordinate -> Ray -> Bool
(px, py) `isLeftOf` ((ax, ay), (bx, by)) = let (s, t) = (px - ax, py - ay)
                                               (u, v) = (px - bx, py - by)
                                            in s * v >= t * u

containsS :: Shape -> Coordinate -> Bool
(Rectangle s1 s2) `containsS` (x, y) = let t1 = s1 / 2
                                           t2 = s2 / 2
                                       in -t1 <= x && x <= t1 &&
                                            -t2 <= y && y <= t2
(Ellipse r1 r2) `containsS` (x, y)   = (x / r1) ^ 2 + (y / r2) ^ 2 <= 1
(Polygon pts) `containsS` p          =
    let leftOfList = map (isLeftOf p)
                         (zip pts (tail pts ++ [head pts]))
    in listEqual leftOfList
(RtTriangle s1 s2) `containsS` p     =
    (Polygon [(0, 0), (s1, 0), (0, s2)]) `containsS` p

containsR :: Region -> Coordinate -> Bool
(Shape s) `containsR` p                                  = s `containsS` p
-- HalfPlane is delimited such that all points on the half plane is to the
-- left of ray from a to b
(HalfPlane a b) `containsR` p                            = p `isLeftOf` (a, b)
{- HalfPlane is delimited such that all points on the half plane is to the
    left of line passing through a and b
(HalfPlane a@(ax, ay) b@(bx, by)) `containsR` p@(px, py)
    | ay == by  = py < ay
    | otherwise =
        let translatedVector = ((1, 0), (bx - ax + 1, by - ay))
        in if (0, 0) `isLeftOf` translatedVector
              then p `isLeftOf` (a, b)
              else p `isLeftOf` (b, a)
-}
(Translate (u, v) r) `containsR` (x, y)                  =
    r `containsR` (x - u, y - v)
(Scale (u, v) r) `containsR` (x, y)                      =
    r `containsR` (x/u, y/v)
(Complement r) `containsR` p                             =
    not (r `containsR` p)
Empty `containsR` p                                      = False
(r1 `Union` r2) `containsR` p                            =
    r1 `containsR` p || r2 `containsR` p
(r1 `Intersect` r2) `containsR` p                        =
    r1 `containsR` p && r2 `containsR` p

annulus :: Radius -> Radius -> Region
annulus ri ro = Complement (Shape (Ellipse ri ri)) `Intersect`
                    Shape (Ellipse ro ro)

-- assume counter-clockwise ordering
polygon :: [Vertex] -> Region
polygon (v1 : v2 : vs) = foldr Intersect
                               (HalfPlane v1 v2)
                               (zipWith HalfPlane vs (tail vs ++ [v1]))

flipX :: Region -> Region
flipX (Shape (RtTriangle s1 s2))    = Shape (RtTriangle s1 (-s2))
flipX (Shape (Polygon l))           =
    Shape (Polygon (map (\(x, y) -> (x, -y)) . reverse $ l))
flipX s@(Shape _)                   = s
flipX (HalfPlane (ax, ay) (bx, by)) = HalfPlane (bx, -by) (ax, -ay)
flipX (Translate (x, y) r)          = Translate (x, -y) (flipX r)
flipX (Scale s r)                   = Scale s (flipX r)
flipX (Complement r)                = Complement (flipX r)
flipX (r1 `Union` r2)               = (flipX r1) `Union` (flipX r2)
flipX (r1 `Intersect` r2)           = (flipX r1) `Intersect` (flipX r2)
flipX Empty                         = Empty

flipY :: Region -> Region
flipY (Shape (RtTriangle s1 s2))    = Shape (RtTriangle (-s1) s2)
flipY (Shape (Polygon l))           =
    Shape (Polygon (map (\(x, y) -> (-x, y)) . reverse $ l))
flipY s@(Shape _)                   = s
flipY (HalfPlane (ax, ay) (bx, by)) = HalfPlane (-bx, by) (-ax, ay)
flipY (Translate (x, y) r)          = Translate (-x, y) (flipY r)
flipY (Scale s r)                   = Scale s (flipY r)
flipY (Complement r)                = Complement (flipY r)
flipY (r1 `Union` r2)               = (flipY r1) `Union` (flipY r2)
flipY (r1 `Intersect` r2)           = (flipY r1) `Intersect` (flipY r2)
flipY Empty                         = Empty
