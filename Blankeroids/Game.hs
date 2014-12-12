{-# LANGUAGE Arrows, ScopedTypeVariables, OverloadedStrings #-}

module Blankeroids.Game where

import FRP.Yampa

import Blankeroids.Types
import Blankeroids.Saucer

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
    createSaucer <- after (randomTime g') () -< ()
    returnA -< gm { score = score', lives = lives', theRound = round',
                    gameOver = gameOver',
                    spawn = createSaucer `tag` newSaucer g''}
  where
    (g', g'') = split g

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
    randomTime gen' = fst $ randomR (5.0, 10.0) gen'
