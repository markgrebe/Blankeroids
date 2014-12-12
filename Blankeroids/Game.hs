{-# LANGUAGE Arrows, ScopedTypeVariables, OverloadedStrings #-}

module Blankeroids.Game where

import FRP.Yampa

import Blankeroids.Types
import Blankeroids.Saucer

theGame :: Object
theGame = Game {score = 0, lives = 3, theRound = 1, gameOver = False,
                done = NoEvent, spawn = NoEvent }

playingGame :: RandomGen g => g -> Object -> SFObject
playingGame g gm = proc ev -> do
    score' <- accumHoldBy accumScore 0 -< ev
    -- ToDo: Bonus lives
    lives' <- accumHoldBy accumLives 3 -< ev
    round' <- accumHoldBy accumRounds 1 -< ev
    gameOver' <- hold False <<< edgeTag True -< lives' <= 0
    createSaucer <- after (randomTime g') () -< ()
    returnA -< gm { score = score', lives = lives', theRound = round',
                    gameOver = gameOver',
                    spawn = createSaucer `tag` newSaucer g''}
  where
    (g', g'') = split g

    accumScore :: Int -> GameEvent -> Int
    accumScore start GameChange { scoreChanged = s} = start + s
    accumScore start _                              = start

    accumLives :: Int -> GameEvent -> Int
    accumLives start GameChange {shipDestroyed = sd} | sd == True = start - 1
    accumLives start _                                            = start

    accumRounds :: Int -> GameEvent -> Int
    accumRounds start GameChange {newRound = rc} | rc == True = start + 1
    accumRounds start _                                       = start

    randomTime :: RandomGen g => g -> Double
    randomTime gen' = fst $ randomR (5.0, 10.0) gen'
