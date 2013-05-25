module Draw (inchToPixel, pixelToInch, intToFloat,
             xWin, yWin, trans, shapeToGraphics
            ) where

import Shape
import SimpleGraphics (spaceClose)
import Graphics.SOE.Gtk

inchToPixel :: Float -> Int
inchToPixel = round . (100*)

pixelToInch :: Int -> Float
pixelToInch = (/ 100) . intToFloat

intToFloat  :: Int -> Float
intToFloat = fromInteger . toInteger

xWin, yWin :: Int
xWin        = 600
yWin        = 500

trans       :: Vertex -> Point
trans (x, y) = (xWin2 + inchToPixel x,
                yWin2 - inchToPixel y)

xWin2, yWin2 :: Int
xWin2         = xWin `div` 2
yWin2         = yWin `div` 2

transList :: [Vertex] -> [Point]
transList = map trans

-- shapeToGraphics                   :: Shape -> Graphics
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
sh1 = Rectangle 3 2
sh2 = Ellipse 1 1.5
sh3 = RtTriangle 3 2
sh4 = Polygon [(-2.5, 2.5), (-1.5, 2.0), (-1.1, 0.2), (-1.7, -1.0), (-3.0, 0)]
sh5 = regularPolygon 5 1

type ColoredShapes = [(Color, Shape)]
shs :: ColoredShapes
shs = [(Red, sh1), (Blue, sh2), (Yellow, sh3), (Magenta, sh4), (Green, sh5)]

drawShapes :: Window -> ColoredShapes -> IO ()
drawShapes w css =
    sequence_ (map aux css)
    where aux (c, s) = drawInWindow w (withColor c (shapeToGraphics s))

main0 :: IO ()
main0 = runGraphics (
            do w <- openWindow "Drawing Shapes" (xWin, yWin)
               drawShapes w shs
               spaceClose w
        )

conCircles :: [Shape]
conCircles = map circle [2.4, 2.1 .. 0.3]

coloredCircles = zip [Black, Blue, Green, Cyan, Red, Magenta, Yellow, White]
                     conCircles

main1 :: IO ()
main1 = runGraphics (
            do w <- openWindow "Bull's Eye" (xWin, yWin)
               drawShapes w coloredCircles
               spaceClose w
        )
