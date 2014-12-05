{-# LANGUAGE Arrows, ScopedTypeVariables, OverloadedStrings #-}

module Blankeroids.Utils where

import Data.Fixed(mod')

import FRP.Yampa
import FRP.Yampa.Vector2

import Control.Monad.Random

import Blankeroids.Types

randGenList :: RandomGen g => g -> [g]
randGenList g = g' : randGenList g''
  where
    (g', g'') = split g

destroyedToUnit :: Event GameEvent -> Event ()
destroyedToUnit (Event Destroyed) = Event ()
destroyedToUnit _                     = NoEvent

reanimateToUnit :: Event GameEvent -> Event ()
reanimateToUnit (Event Reanimate) = Event ()
reanimateToUnit _                 = NoEvent

wrapObject :: Double -> SF Position Position
wrapObject objRadius = proc objPos -> do
    let minCoord = -objRadius
        maxCoord = 1.0 + objRadius
        wrap a = a `mod'` (1.0 + 2.0 * objRadius)
        (x,y) = vector2XY objPos
        objPos' = vector2 (if x <= minCoord || x >= maxCoord
                           then wrap x else x)
                          (if y <= minCoord || y >= maxCoord
                           then wrap y else y)
    returnA -< objPos'

