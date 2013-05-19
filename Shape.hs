module Shape (Shape (..),
              Radius, Side, Vertex,
              square, circle, rtTriangle, regularPolygon,
              distBetween, area, convex
             ) where

data Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [Vertex]
    deriving Show

type Radius = Float
type Side   = Float
type Vertex = (Float, Float)
type Vector = (Float, Float)
type Radian = Float

square  :: Side -> Shape
square s = Rectangle s s

circle  :: Radius -> Shape
circle r = Ellipse r r

rectangle      :: Side -> Side -> Shape
rectangle s1 s2 = Polygon [(0, 0), (s1, 0), (s1, s2), (0, s2)]

rtTriangle      :: Side -> Side -> Shape
rtTriangle s1 s2 = Polygon [(0, 0), (s1, 0), (0, s2)]

rotate         :: Radian -> Vertex -> Vertex
rotate r (x, y) = ((x * cos r - y * sin r), (x * sin r + y * cos r))

regularPolygon    :: Int -> Side -> Shape
regularPolygon n s =
    let n' = fromIntegral n
        angle = 2 * pi / n'
        firstPoint = (sqrt (s ^ 2 / (2 - 2 * cos angle)), 0.0)
    in Polygon $ map (\n -> rotate (n' * angle) firstPoint)
                     [0 .. n-1]

distBetween                  :: Vertex -> Vertex -> Float
distBetween (x1, y1) (x2, y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

triArea         :: Vertex -> Vertex -> Vertex -> Float
triArea v1 v2 v3 = sqrt (s * (s - a) * (s - b) * (s - c))
    where a = distBetween v1 v2
          b = distBetween v2 v3
          c = distBetween v3 v1
          s = (a + b + c) / 2

area'                              :: Shape -> Float
area' (Rectangle s1 s2)             = s1 * s2
area' (RtTriangle s1 s2)            = s1 * s2 / 2
area' (Ellipse r1 r2)               = pi * r1 * r2
area' (Polygon [])                  = 0
area' (Polygon (v1 : vs))           = polyArea vs
    where polyArea                :: [Vertex] -> Float
          polyArea (v2 : v3 : vs') = triArea v1 v2 v3 + polyArea (v3 : vs')
          polyArea _               = 0

convex                              :: Shape -> Bool
convex (Rectangle _ _)               = True
convex (RtTriangle _ _)              = True
convex (Ellipse _ _)                 = True
convex (Polygon (v1 : v2 : v3 : vs)) = check v1 v2 v3 vs (sineSign v1 v2 v3)
    where vector                  :: Vertex -> Vertex -> Vector
          vector (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)
          sineSign         :: Vertex -> Vertex -> Vertex -> Ordering
          sineSign v1 v2 v3 = (x1 * y2) `compare` (x2 * y1)
            where (x1, y1) = vector v2 v1
                  (x2, y2) = vector v2 v3
          check                       :: Vertex -> Vertex -> Vertex ->
                                            [Vertex] -> Ordering -> Bool
          check v1' v2' v3' (v : vs') ordering
            | sameOrdering = check v2' v3' v vs' ordering
            | otherwise    = False
            where sameOrdering = ordering == sineSign v1' v2' v3'
          check v1' v2' v3' _ ordering =
            ordering == sineSign v1' v2' v3' &&
            ordering == sineSign v2' v3' v1
convex _                             = False

area                    :: Shape -> Float
area (Rectangle s1 s2)   = s1 * s2
area (RtTriangle s1 s2)  = s1 * s2 / 2
area (Ellipse r1 r2)     = pi * r1 * r2
area (Polygon (v1 : vs)) =
    a + trapezoidArea lastVertex v1
    where trapezoidArea                  :: Vertex -> Vertex -> Float
          trapezoidArea (x1, y1) (x2, y2) = (y1 + y2) * (x1 - x2) / 2
          result = foldl (\(a, lastVertex) vertex ->
                              (a + trapezoidArea lastVertex vertex, vertex))
                         (0, v1)
                         vs
          a = fst result
          lastVertex = snd result
