{-# LANGUAGE Arrows, ScopedTypeVariables, OverloadedStrings #-}

module Blankeroids.Graphics where

import Data.Char(digitToInt)
import Graphics.Blank hiding (Event)
import IdentityList

import Blankeroids.Polygons
import Blankeroids.Types
import Blankeroids.Ship

normalObjectWidth :: Double
normalObjectWidth = 0.002
debrisWidth :: Double
debrisWidth = 0.001

-- | A Canvas action to render the entire scene.
renderScene :: IL Object -> Canvas ()
renderScene a = do
    scaleScene
    fillStyle "black"
    fillRect(0.0,0.0,1.0,1.0)
    renderObjects (elemsIL a)
    return ()

-- | We scale such that (0,0) is the bottom-left of the canvas and (1,1)
--   is the top-right.
scaleScene :: Canvas ()
scaleScene = do
    context <- myCanvasContext
    let w = width context
        h = height context
    translate (0,h)
    scale (w, negate h)

renderPolygon :: Double -> Polygon -> Canvas ()
renderPolygon _      [] = return ()
renderPolygon lwidth p  = do
    beginPath ()
    moveTo (head p)
    mapM_ lineTo (tail p)
    lineWidth lwidth
    strokeStyle "white"
    stroke()

renderPolygons :: Double -> [Polygon] -> Canvas ()
renderPolygons lwidth p = do
    mapM_ (renderPolygon lwidth) p

renderShip :: Object -> Canvas ()
renderShip s = do
    renderPolygon normalObjectWidth (poly s)
    if (thrusting s) && not (null (poly s)) then do
        renderPolygon normalObjectWidth thrustPolygon
    else
        return()

renderAsteroid :: Object -> Canvas ()
renderAsteroid a = do
    renderPolygon normalObjectWidth (poly a)

renderMissile :: Object -> Canvas ()
renderMissile m = do
    renderPolygon normalObjectWidth (poly m)

renderDebris :: Object -> Canvas ()
renderDebris d = do
    renderPolygon debrisWidth (poly d)

renderGame :: Object -> Canvas ()
renderGame g = do
    renderPolygons normalObjectWidth (placedScorePolygons ++ placedLifePolygons)
    if gameOver g then do
        renderPolygons normalObjectWidth placedGameOverPolygons
    else
        return()
  where
    placedLifePolygons :: [Polygon]
    placedLifePolygons = map translatePolyPair
                            (zip shipPositions lifePolygons)
    placedScorePolygons :: [Polygon]
    placedScorePolygons = map translatePolyPair
                            (zip digitPositions scorePolygons)
    placedGameOverPolygons :: [Polygon]
    placedGameOverPolygons = map translatePolyPair
                                (zip gameOverPositions gameOverPolygons)
    translatePolyPair :: (Point, Polygon) -> Polygon
    translatePolyPair (pt,pol) = translatePoly pt pol
    lifePolygons :: [Polygon]
    lifePolygons = take (lives g) (repeat shipPolygon)
    scorePolygons :: [Polygon]
    scorePolygons = map digToPolygon (reverse $ digs (score g))
    gameOverPolygons :: [Polygon]
    gameOverPolygons = [letterGPolygon, letterAPolygon, letterMPolygon,
                        letterEPolygon, letterSpacePolygon, letterOPolygon,
                        letterVPolygon, letterEPolygon, letterRPolygon]
    digToPolygon :: Int -> Polygon
    digToPolygon d = digitPolygons !! d
    digs :: Int -> [Int]
    digs n = map digitToInt $ show n
    digitPositions :: [Point]
    digitPositions = zip [0.220,0.180..0.020] (take 6 $ repeat 0.950)
    shipPositions :: [Point]
    shipPositions =  zip [0.220,0.200..0.120] (take 6 $ repeat 0.890)
    gameOverPositions :: [Point]
    gameOverPositions = zip [0.340,0.380..0.700] (take 9 $ repeat 0.5)

renderObject :: Object -> Canvas ()
renderObject obj = case obj of
    Asteroid _ _ _ _ _ _ _ _ _ _ _ _ -> renderAsteroid obj
    Ship     _ _ _ _ _ _ _ _ _ _ _   -> renderShip obj
    Missile  _ _ _ _ _ _             -> renderMissile obj
    Debris   _ _ _ _ _ _ _           -> renderDebris obj
    Game     _ _ _ _ _ _             -> renderGame obj

renderObjects :: [Object] -> Canvas ()
renderObjects = mapM_ renderObject

---------------------------------------------------

digitZeroPolygon :: Polygon
digitZeroPolygon = [(-0.015, 0.023),( 0.015, 0.023),( 0.015,-0.023),
                    (-0.015,-0.023),(-0.015, 0.023)]

digitOnePolygon :: Polygon
digitOnePolygon = [( 0.015, 0.023),( 0.015,-0.023)]

digitTwoPolygon :: Polygon
digitTwoPolygon = [(-0.015, 0.023),( 0.015, 0.023),( 0.015, 0.000),
                   (-0.015, 0.000),(-0.015,-0.023),( 0.015,-0.023)]

digitThreePolygon :: Polygon
digitThreePolygon = [(-0.015, 0.023),( 0.015, 0.023),( 0.015, 0.000),
                     (-0.015, 0.000),( 0.015, 0.000),( 0.015,-0.023),
                     (-0.015,-0.023)]

digitFourPolygon :: Polygon
digitFourPolygon = [(-0.015, 0.023),(-0.015, 0.000),( 0.015, 0.000),
                    ( 0.015,-0.023),( 0.015, 0.023)]

digitFivePolygon :: Polygon
digitFivePolygon = [( 0.015, 0.023),(-0.015, 0.023),(-0.015, 0.000),
                    ( 0.015, 0.000),( 0.015,-0.023),(-0.015,-0.023)]

digitSixPolygon :: Polygon
digitSixPolygon = [( 0.015, 0.023),(-0.015, 0.023),(-0.015, 0.000),
                   ( 0.015, 0.000),( 0.015,-0.023),(-0.015,-0.023),
                   (-0.015, 0.000)]

digitSevenPolygon :: Polygon
digitSevenPolygon = [(-0.015, 0.023),( 0.015, 0.023),( 0.015,-0.023)]

digitEightPolygon :: Polygon
digitEightPolygon = [(-0.015, 0.023),( 0.015, 0.023),( 0.015,-0.023),
                     (-0.015,-0.023),(-0.015, 0.023),(-0.015, 0.000),
                     ( 0.015, 0.000)]

digitNinePolygon :: Polygon
digitNinePolygon = [(-0.015,-0.023),( 0.015,-0.023),( 0.015, 0.023),
                    (-0.015, 0.023),(-0.015, 0.000),( 0.015, 0.000)]

digitPolygons :: [Polygon]
digitPolygons = [digitZeroPolygon, digitOnePolygon, digitTwoPolygon,
                  digitThreePolygon, digitFourPolygon, digitFivePolygon,
                  digitSixPolygon, digitSevenPolygon, digitEightPolygon,
                  digitNinePolygon]


letterAPolygon :: Polygon
letterAPolygon  = [(-0.015,-0.023),(-0.015, 0.008),( 0.000, 0.023),
                   ( 0.015, 0.008),( 0.015,-0.023),( 0.015,-0.008),
                   (-0.015,-0.008)]

letterEPolygon :: Polygon
letterEPolygon  = [( 0.015,-0.023),(-0.015,-0.023),(-0.015, 0.023),
                   ( 0.015, 0.023),(-0.015, 0.023),(-0.015, 0.000),
                   ( 0.008, 0.000)]

letterGPolygon :: Polygon
letterGPolygon  = [( 0.000,-0.008),( 0.015,-0.008),( 0.015,-0.023),
                   (-0.015,-0.023),(-0.015, 0.023),( 0.015, 0.023),
                   ( 0.015, 0.008)]

letterMPolygon :: Polygon
letterMPolygon  = [(-0.015,-0.023),(-0.015, 0.023),(0.000,0.008),
                   ( 0.015, 0.023),( 0.015,-0.023)]

letterOPolygon :: Polygon
letterOPolygon  = digitZeroPolygon

letterRPolygon :: Polygon
letterRPolygon = [(-0.015,-0.023),(-0.015, 0.023),( 0.015, 0.023),
                  ( 0.015, 0.000),(-0.015, 0.000),(-0.005, 0.000),
                  ( 0.015,-0.023)]

letterVPolygon :: Polygon
letterVPolygon  = [(-0.015, 0.023),(0.000, -0.023),( 0.015, 0.023)]

letterSpacePolygon :: Polygon
letterSpacePolygon = []
