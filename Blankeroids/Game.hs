{-# LANGUAGE Arrows, ScopedTypeVariables, OverloadedStrings #-}

module Blankeroids.Game where

import FRP.Yampa

import Blankeroids.Types
import Blankeroids.Saucer
import Blankeroids.Graphics
import Blankeroids.Polygons
import Blankeroids.Ship
import Blankeroids.Asteroids

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
    (g3,g4) = split g1
    (g5,g6) = split g2
    aSFs = movingRandomAsteroids g3 (genInitialAsteroids g4)
    shipSF = movingShip g5 theShip
    gameSF = playingGame g6 theGame
    gameObjects = [shipSF, gameSF] ++ aSFs
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

bonusScore :: Int
bonusScore = 8000

theGame :: Object
theGame = Game {score = 0, lives = 3, theRound = 1, gameOver = False,
                done = NoEvent, spawn = NoEvent }

playingGame :: RandomGen g => g -> Object -> SFObject
playingGame g gm = proc ev -> do
    (score',lives') <- accumHoldBy accumScoreLives (0,3) -< ev
    round' <- accumHoldBy accumRounds 1 -< ev
    gameOver' <- hold False <<< edgeTag True -< lives' <= 0
    nextGame <- delayEvent 10.0 <<< edge -< gameOver'
    createSaucer <- after (randomTime g2) () -< ()
    returnA -< gm { score = score', lives = lives', theRound = round',
                    gameOver = gameOver',
                    done = nextGame,
                    spawn = mergeBy (++)
                            (createSaucer `tag` newSaucer g3 0)
                            (nextGame `tag` [waitingUser g4 theWait])}
  where
    (g1, g2) = split g
    (g3, g4) = split g1

    accumScoreLives :: (Int, Int) -> GameEvent -> (Int, Int)
    accumScoreLives (startScore, startLives) GameChange { scoreChanged = s,
                                                          shipDestroyed = sd } =
      let
        nextScore = startScore + s
        bonusLife = (nextScore `mod` bonusScore) < (startScore `mod` bonusScore)
        lostLives = if sd then -1 else 0
        gainLives = if bonusLife then 1 else 0
        nextLives = startLives + gainLives + lostLives
      in
        (nextScore, nextLives)
    accumScoreLives (startScore, startLives) _ =
        (startScore, startLives)

    accumRounds :: Int -> GameEvent -> Int
    accumRounds start GameChange {newRound = rc} | rc == True = start + 1
    accumRounds start _                                       = start

    randomTime :: RandomGen g => g -> Double
    randomTime gen' = fst $ randomR (interSaucerInterval/2.0,
                                     interSaucerInterval) gen'
