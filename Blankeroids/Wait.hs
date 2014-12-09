{-# LANGUAGE Arrows, ScopedTypeVariables, OverloadedStrings #-}

module Blankeroids.Wait where

import FRP.Yampa

import Blankeroids.Types
import Blankeroids.Graphics
import Blankeroids.Polygons
import Blankeroids.Asteroids
import Blankeroids.Ship

theWait :: Object
theWait = Wait {polys = [], done = NoEvent, spawn = NoEvent }

waitingUser :: RandomGen g => g -> Object -> SFObject
waitingUser g wa = proc ev -> do
    onOff <- accumHoldBy (\a b -> a /= b) True <<< repeatedly 1.0 True -< ()
    returnA -< wa {polys = if onOff then placedPressAnyPolygons else [],
                   done = ev `tag` (),
                   spawn = ev `tag` gameObjects}
  where
    (g1,g2) = split g
    (g3,g4) = split g2
    aSFs = movingRandomAsteroids g1 (genInitialAsteroids g3)
    shipSF = movingShip g4 theShip
    gameObjects = shipSF : aSFs
    placedPressAnyPolygons :: [Polygon]
    placedPressAnyPolygons = map translatePolyPair
                                (zip pressAnyPositions pressAnyPolygons)
    translatePolyPair :: (Point, Polygon) -> Polygon
    translatePolyPair (pt,pol) = translatePoly pt pol
    pressAnyPolygons :: [Polygon]
    pressAnyPolygons = [letterPPolygon, letterRPolygon, letterEPolygon,
                        letterSPolygon, letterSPolygon, letterSpacePolygon,
                        letterAPolygon, letterNPolygon, letterYPolygon,
                        letterSpacePolygon, letterKPolygon, letterEPolygon,
                        letterYPolygon]
    pressAnyPositions :: [Point]
    pressAnyPositions = zip [0.260,0.300..0.780] (take 13 $ repeat 0.75)
