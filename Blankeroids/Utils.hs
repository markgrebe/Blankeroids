{-# LANGUAGE Arrows, ScopedTypeVariables, OverloadedStrings #-}

module Blankeroids.Utils where

import Data.Fixed(mod')

import FRP.Yampa
import FRP.Yampa.Vector2

import Blankeroids.Types

-- Create an infinite list of generators by splitting an initial generator.
randGenList :: RandomGen g => g -> [g]
randGenList g = g' : randGenList g''
  where
    (g', g'') = split g

-- Utility function to filter GameEvent event into a unit Event if it is
-- of type Destroyed.
destroyedToUnit :: Event GameEvent -> Event ()
destroyedToUnit (Event Destroyed) = Event ()
destroyedToUnit _                 = NoEvent

-- Utility function to filter GameEvent event into a unit Event if it is
-- of type Reanimate.
reanimateToUnit :: Event GameEvent -> Event ()
reanimateToUnit (Event Reanimate) = Event ()
reanimateToUnit _                 = NoEvent

-- Utility Signal Function to wrap an object's position from one edge of
-- the screen to the other when it passes beyond the edge.
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

-- Utility Signal Function to wrap an object's position from one edge of
-- the screen to the other when it passes beyond the edge for vertical edges
-- only
wrapObjectY :: Double -> SF Position Position
wrapObjectY objRadius = proc objPos -> do
    let minCoord = -objRadius
        maxCoord = 1.0 + objRadius
        wrap a = a `mod'` (1.0 + 2.0 * objRadius)
        (x,y) = vector2XY objPos
        objPos' = vector2 x (if y <= minCoord || y >= maxCoord
                             then wrap y else y)
    returnA -< objPos'

