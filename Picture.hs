module Picture (Picture (Region, Over, EmptyPic),
                Color (Black, Blue, Green, Cyan,
                       Red, Magenta, Yellow, White),
                regionToGRegion, shapetoGRegion,
                drawRegionInWindow, drawPic, draw,
                spaceClose,
                module Region
               ) where

import Draw hiding (trans)
import Region
import SimpleGraphics (spaceClose)
import Graphics.SOE.Gtk hiding (Region)
import qualified Graphics.SOE.Gtk as G (Region)

data Picture = Region Color Region
             | Picture `Over` Picture
             | EmptyPic
    deriving Show

type Vector = (Float, Float)

drawRegionInWindow :: Window -> Color -> Region -> IO ()
drawRegionInWindow w c r = drawInWindow w
                             (withColor c
                                (drawRegion (regionToGRegion r)))

drawPic :: Window -> Picture -> IO ()
drawPic w (Region c r)   = drawRegionInWindow w c r
drawPic w (p1 `Over` p2) = do drawPic w p2
                              drawPic w p1
drawPic w EmptyPic       = return ()

regionToGRegion ::Region -> G.Region
regionToGRegion = regToGReg (0, 0) (1, 1)

regToGReg :: Vector -> Vector -> Region -> G.Region
regToGReg loc sca (Shape s) = shapetoGRegion loc sca s
regToGReg loc (sx, sy) (Scale (u, v) r) = regToGReg loc (sx * u, sy * v) r
regToGReg (lx, ly) (sx, sy) (Translate (u, v) r) =
    regToGReg (lx + u * sx, ly + v * sy) (sx, sy) r
regToGReg loc sca Empty = createRectangle (0, 0) (0, 0)
regToGReg loc sca (r1 `Union` r2) = primGRegion loc sca r1 r2 orRegion
regToGReg loc sca (r1 `Intersect` r2) = primGRegion loc sca r1 r2 andRegion
regToGReg loc sca (Complement r) = primGRegion loc sca winRect r diffRegion

primGRegion :: Vector -> Vector -> Region -> Region
            -> (G.Region -> G.Region -> G.Region)
            -> G.Region
primGRegion loc sca r1 r2 op = let gr1 = regToGReg loc sca r1
                                   gr2 = regToGReg loc sca r2
                               in op gr1 gr2

winRect :: Region
winRect = Shape (Rectangle (pixelToInch xWin) (pixelToInch yWin))

shapetoGRegion :: Vector -> Vector -> Shape -> G.Region
shapetoGRegion (lx, ly) (sx, sy) s =
    case s of
        Rectangle s1 s2 ->
            createRectangle (trans (-s1/2, -s2/2)) (trans (s1/2, s2/2))
        Ellipse r1 r2 ->
            createEllipse (trans (-r1, -r2)) (trans (r1, r2))
        Polygon vs ->
            createPolygon . map trans $ vs
        RtTriangle s1 s2 ->
            createPolygon . map trans $ [(0, 0), (s1, 0), (0, s2)]
    where trans :: Vertex -> Point
          trans (x, y) = (xWin2 + inchToPixel (lx + x * sx),
                          yWin2 - inchToPixel (ly + y * sy))

xWin2 = xWin `div` 2
yWin2 = yWin `div` 2

draw :: String -> Picture -> IO ()
draw s p = runGraphics $
            do w <- openWindow s (xWin, yWin)
               drawPic w p
               spaceClose w

xUnion :: Region -> Region -> Region
p1 `xUnion` p2 = (p1 `Intersect` Complement p2) `Union`
                 (p2 `Intersect` Complement p1)

r1, r2, r3, r4 :: Region
r1 = Shape (Rectangle 3 2)
r2 = Shape (Ellipse 1 1.5)
r3 = Shape (RtTriangle 3 2)
r4 = Shape (Polygon [(-2.5, 2.5), (-3, 0), (-1.7, -1.0),
                     (-1.1, 0.2), (-1.5, 2)])

reg1, reg2 :: Region
reg1 = r3 `xUnion` (r1 `Intersect` Complement r2 `Union` r4)
reg2 = let circle = Shape (Ellipse 0.5 0.5)
           square = Shape (Rectangle 1 1)
       in (Scale (2, 2) circle)
          `Union` (Translate (1, 0) square)
          `Union` (Translate (-1, 0) square)

pic1, pic2, pic3, pic4 :: Picture
pic1 = Region Blue reg1
pic2 = Region Yellow (Translate (0, -1) reg2)
pic3 = pic2 `Over` pic1
pic4 = Region Blue . Translate (-1, 0) . Scale (0.5, 0.5) $ fiveCircles

pictToList :: Picture -> [(Color, Region)]
pictToList EmptyPic = []
pictToList (Region c r) = [(c, r)]
pictToList (p1 `Over` p2) = pictToList p1 ++ pictToList p2

adjust :: [(Color, Region)] -> Coordinate
       -> (Maybe (Color, Region), [(Color, Region)])
adjust regs p = case (break (\(_, r) -> r `containsR` p) regs) of
                    (top, hit:rest) -> (Just hit, top ++ rest)
                    (_, [])         -> (Nothing, regs)

loop :: Window -> [(Color, Region)] -> IO ()
loop w regs =
    do clearWindow w
       sequence_ . map (uncurry $ drawRegionInWindow w) $ reverse regs
       (x, y) <- getLBP w
       let aux (_, r) = r `containsR` (pixelToInch (x - xWin2),
                                       pixelToInch (yWin2 - y))
       case (break aux regs) of
            (_, [])        -> closeWindow w
            (top, hit:bot) -> loop w (hit:(top++bot))

draw2 :: String -> Picture -> IO ()
draw2 s p = runGraphics $
                do w <- openWindow s (xWin, yWin)
                   loop w (pictToList p)

p1, p2, p3, p4, pic :: Picture
p1 = Region Red r1
p2 = Region Blue r2
p3 = Region Green r3
p4 = Region Yellow r4
pic = foldl Over EmptyPic [p1, p2, p3, p4]

main0 :: IO ()
main0 = draw2 "Picture Click Test" pic

loop2 :: Window -> [(Color, Region)] -> IO ()
loop2 w = handler
    where drawColoredRegions :: Window -> [(Color, Region)] -> IO ()
          drawColoredRegions w regs =
            do clearWindow w
               sequence_ . map (uncurry $ drawRegionInWindow w) $ reverse regs
          handler regs =
            do drawColoredRegions w regs
               e <- getWindowEvent w
               case e of
                    (Button { pt = pt@(x, y), isDown = id })
                         | id        ->
                             let aux (_, r) =
                                    r `containsR` (pixelToInch (x - xWin2),
                                                   pixelToInch (yWin2 - y))
                             in case (break aux regs) of
                                  (_, [])        -> closeWindow w
                                  (top, hit:bot) -> handlerMove pt
                                                       (hit:(top++bot))
                         | otherwise -> handler regs
                    _ -> handler regs
          handlerMove pt@(x, y) regs@((c,r):crs) =
            do drawColoredRegions w regs
               e <- getWindowEvent w
               case e of
                    MouseMove npt@(nx, ny) ->
                        let delta = (pixelToInch $ nx - x,
                                     pixelToInch $ y - ny)
                        in handlerMove npt ((c, Translate delta r):crs)
                    (Button { isDown = id })
                        | not id    -> handler regs
                        | otherwise -> handlerMove pt regs

draw3 :: String -> Picture -> IO ()
draw3 s p = runGraphics $
                do w <- openWindow s (xWin, yWin)
                   loop2 w (pictToList p)

main1 :: IO ()
main1 = draw3 "Picture Drag and Drop" pic
