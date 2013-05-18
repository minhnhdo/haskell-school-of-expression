module Draw (inchToPixel, pixelToInch, intToFloat,
             xWin, yWin, trans, shapeToGraphics
            ) where

import Shape
import SimpleGraphics (spaceClose)
import Graphics.SOE

inchToPixel  :: Float -> Int
inchToPixel x = round (100 * x)

pixelToInch  :: Int -> Float
pixelToInch n = intToFloat n / 100

intToFloat  :: Int -> Float
intToFloat n = fromInteger (toInteger n)

xWin, yWin :: Int
xWin        = 600
yWin        = 500

trans       :: Vertex -> Point
trans (x, y) = (xWin2 + inchToPixel x,
                yWin2 - inchToPixel y)

xWin2, yWin2 :: Int
xWin2         = xWin `div` 2
yWin2         = yWin `div` 2

transList         :: [Vertex] -> [Point]
transList []       = []
transList (p : ps) = trans p : transList ps

-- shapeToGraphics                   :: Shape -> Draw ()
shapeToGraphics (Rectangle s1 s2)  =
    let s12 = s1 / 2
        s22 = s2 / 2
    in polygon (transList [(-s12, -s22), (-s12, s22), (s12, s22), (s12, -s22)])
shapeToGraphics (Ellipse r1 r2)    =
    ellipse (trans (-r1, -r2)) (trans (r1, r2))
shapeToGraphics (RtTriangle s1 s2) =
    polygon (transList [(0, 0), (s1, 0), (0, s2)])
shapeToGraphics (Polygon vts)      = polygon (transList vts)

sh1, sh2, sh3, sh4 :: Shape
sh1                 = Rectangle 3 2
sh2                 = Ellipse 1 1.5
sh3                 = RtTriangle 3 2
sh4                 = Polygon [(-2.5, 2.5), (-1.5, 2.0), (-1.1, 0.2),
                               (-1.7, -1.0), (-3.0, 0)]
sh5                 = regularPolygon 5 1

type ColoredShapes = [(Color, Shape)]
shs :: ColoredShapes
shs  = [(Red, sh1), (Blue, sh2), (Yellow, sh3), (Magenta, sh4), (Green, sh5)]

drawShapes                :: Window -> ColoredShapes -> IO ()
drawShapes _ []            = return ()
drawShapes w ((c, s) : cs) = do drawInWindow w
                                    (withColor c (shapeToGraphics s))
                                drawShapes w cs

main0 :: IO ()
main0  = runGraphics (
            do w <- openWindow "Drawing Shapes" (xWin, yWin)
               drawShapes w shs
               spaceClose w
         )
