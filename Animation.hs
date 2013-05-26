module Animation where

import Shape
import Draw
import Picture
import Graphics.SOE.Gtk hiding (Region)
import qualified Graphics.SOE.Gtk as G (Region)

type Time = Float
type Animation a = Time -> a

rubberBall :: Animation Shape
rubberBall t = Ellipse (sin t) (cos t)

revolvingBall :: Animation Region
revolvingBall t = let ball = Shape (Ellipse 0.2 0.2)
                  in Translate (sin t, cos t) ball

planets :: Animation Picture
planets t = let p1 = Region Red (Shape (rubberBall t))
                p2 = Region Yellow (revolvingBall t)
            in p1 `Over` p2

tellTime :: Animation String
tellTime t = "The time is: " ++ show t

animate :: String -> Animation Graphic -> IO ()
animate title anim =
    runGraphics $
        do w <- openWindowEx title (Just (0, 0)) (Just (xWin, yWin))
                        drawBufferedGraphic (Just 30)
           t0 <- timeGetTime
           let loop =
                do t <- timeGetTime
                   let ft = (/ 1000) . intToFloat . word32ToInt $ t - t0
                   setGraphic w (anim ft)
                   getWindowTick w
                   loop
           loop

main0 :: IO ()
main0 = animate "Animated Shape"
            (withColor Blue . shapeToGraphics . rubberBall)

main1 :: IO ()
main1 = animate "Animated Text" (text (100, 200) . tellTime)

regionToGraphic :: Region -> Graphic
regionToGraphic = drawRegion . regionToGRegion

main2 :: IO ()
main2 = animate "Animated Region"
            (withColor Yellow . regionToGraphic . revolvingBall)

picToGraphic :: Picture -> Graphic
picToGraphic (Region c r) = withColor c (regionToGraphic r)
picToGraphic (p1 `Over` p2) = picToGraphic p1 `overGraphic` picToGraphic p2
picToGraphic EmptyPic = emptyGraphic

main3 :: IO ()
main3 = animate "Animated Picture" (picToGraphic . planets)
