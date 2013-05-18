module SimpleGraphics where

import Graphics.SOE

spaceClose :: Window -> IO ()
spaceClose w = do k <- getKey w
                  if k == ' '
                     then closeWindow w
                     else spaceClose w

main0 :: IO ()
main0  = runGraphics (
            do w <- openWindow "My First Graphics Program" (300, 300)
               drawInWindow w (text (100, 200) "HelloGraphicsWorld")
               k <- getKey w
               closeWindow w
         )

main1 :: IO ()
main1  = runGraphics (
            do w <- openWindow "My First Graphics Program" (300, 300)
               drawInWindow w (text (100, 200) "HelloGraphicsWorld")
               spaceClose w
         )

myPutStr         :: String -> IO ()
myPutStr []       = return ()
myPutStr (c : cs) = do putChar c
                       myPutStr cs

myGetLine :: IO String
myGetLine  = helper ""
    where helper    :: String -> IO String
          helper acc = do c <- getChar
                          if c == '\n'
                             then return (reverse acc)
                             else helper (c : acc)

pic1  = withColor Red
                  (ellipse (150, 150) (300, 200))

pic2  = withColor Blue
                  (polyline [(100, 50), (200, 50), (200, 250), (100, 250),
                             (100, 50)])

main2 :: IO ()
main2  = runGraphics (
            do w <- openWindow "Some Graphics Figures" (300, 300)
               drawInWindow w pic1
               drawInWindow w pic2
               spaceClose w
         )

fillTri           :: Window -> Int -> Int -> Int -> IO ()
fillTri w x y size = drawInWindow w
                                  (withColor Blue
                                    (polygon [(x, y), (x + size, y),
                                              (x, y - size), (x, y)]))

minSize :: Int
minSize  = 8

sierpinskiTri           :: Window -> Int -> Int -> Int -> IO ()
sierpinskiTri w x y size = if size <= minSize
                              then fillTri w x y size
                              else let size2 = size `div` 2
                                   in do sierpinskiTri w x y size2
                                         sierpinskiTri w x (y - size2) size2
                                         sierpinskiTri w (x + size2) y size2

main3 :: IO ()
main3  = runGraphics (
            do w <- openWindow "Sierpinski's Triangle" (400, 400)
               sierpinskiTri w 50 300 256
               spaceClose w
         )

fillEquiTri                         :: Window -> Int -> Int -> Int ->
                                            Color -> Bool -> IO ()
fillEquiTri w x y size color rotated =
    let s = fromIntegral size
        x' = fromIntegral x
        y' = fromIntegral y
        height = s * sqrt 3 / 2
    in drawInWindow w
         (withColor color
           (polygon (if rotated
                        then [(fromIntegral . round $ x' - s / 2,
                               fromIntegral . round $ y' - height / 3),
                              (fromIntegral . round $ x' + s / 2,
                               fromIntegral . round $ y' - height / 3),
                              (fromIntegral . round $ x',
                               fromIntegral . round $ y' + height * 2 / 3),
                              (fromIntegral . round $ x' - s / 2,
                               fromIntegral . round $ y' - height / 3)]
                        else [(fromIntegral . round $ x' - s / 2,
                               fromIntegral . round $ y' + height / 3),
                              (fromIntegral . round $ x' + s / 2,
                               fromIntegral . round $ y' + height / 3),
                              (fromIntegral . round $ x',
                               fromIntegral . round $ y' - height * 2 / 3),
                              (fromIntegral . round $ x' - s / 2,
                               fromIntegral . round $ y' + height / 3)])))

fillStartOfDavid                 :: Window -> Int -> Int -> Int -> Color ->
                                        IO ()
fillStartOfDavid w x y size color = do fillEquiTri w x y size color True
                                       fillEquiTri w x y size color False

snowflake           :: Window -> Int -> Int -> Int -> IO ()
snowflake w x y size = helper w x y size (nextColor Green)
    where nextColor       :: Color -> Color
          nextColor Red    = Green
          nextColor Green  = Blue
          nextColor Blue   = Yellow
          nextColor Yellow = Red
          helper                 :: Window -> Int -> Int -> Int -> Color ->
                                        IO ()
          helper w x y size color =
            do fillStartOfDavid w x y size color
               if size <= minSize * 3
                  then return ()
                  else let size2 = size `div` 3
                           s = fromIntegral size
                           height = s * sqrt 3 / 2
                           x' = fromIntegral x
                           y' = fromIntegral y
                           color2 = nextColor color
                       in do helper w x
                                (fromIntegral . round $ y' + height * 4 / 9)
                                size2 color2
                             helper w
                                (fromIntegral . round $ x' - s / 3)
                                (fromIntegral . round $ y' + height * 2 / 9)
                                size2 color2
                             helper w
                                (fromIntegral . round $ x' - s / 3)
                                (fromIntegral . round $ y' - height * 2 / 9)
                                size2 color2
                             helper w x
                                (fromIntegral . round $ y' - height * 4 / 9)
                                size2 color2
                             helper w
                                (fromIntegral . round $ x' + s / 3)
                                (fromIntegral . round $ y' - height * 2 / 9)
                                size2 color2
                             helper w
                                (fromIntegral . round $ x' + s / 3)
                                (fromIntegral . round $ y' + height * 2 / 9)
                                size2 color2

main4 :: IO ()
main4  = runGraphics (
            do w <- openWindow "Snowflake" (400, 400)
               snowflake w 200 200 243
               spaceClose w
         )
