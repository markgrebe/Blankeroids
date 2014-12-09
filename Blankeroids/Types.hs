{-# LANGUAGE Arrows, ScopedTypeVariables, OverloadedStrings #-}

module Blankeroids.Types where

import FRP.Yampa
import FRP.Yampa.Vector2

type Position = Vector2 Double
type Velocity = Vector2 Double
type Acceleration = Vector2 Double
type AngPosition = Double
type AngVelocity = Double
type Point = (Double, Double)
type Polygon = [Point]
type Edge = (Point, Point)

---------------------------------------------------

data Object = Asteroid { basePoly :: Polygon,
                         poly   :: Polygon,
                         pos    :: Position,
                         vel    :: Velocity,
                         angPos :: AngPosition,
                         angVel :: AngVelocity,
                         radius :: Double,
                         gen    :: Int,
                         gameRound  :: Int,
                         reqReanimate :: Bool,
                         done   :: Event (),
                         spawn  :: Event [SFObject]
                       }
            | Ship     { basePoly :: Polygon,
                         poly   :: Polygon,
                         thrustPoly :: Polygon,
                         pos    :: Position,
                         vel    :: Velocity,
                         angPos :: AngPosition,
                         radius :: Double,
                         thrusting :: Bool,
                         done   :: Event (),
                         reqReanimate :: Bool,
                         reanimate :: Event(),
                         spawn  :: Event [SFObject]
                       }
            | Missile  { basePoly :: Polygon,
                         poly   :: Polygon,
                         pos    :: Position,
                         vel    :: Velocity,
                         done   :: Event (),
                         spawn  :: Event [SFObject]
                       }
            | Debris   { basePoly :: Polygon,
                         poly   :: Polygon,
                         pos    :: Position,
                         vel    :: Velocity,
                         life   :: Double,
                         done   :: Event (),
                         spawn  :: Event [SFObject]
                       }
            | Game     { score  :: Int,
                         lives  :: Int,
                         theRound  :: Int,
                         gameOver :: Bool,
                         done   :: Event (),
                         spawn  :: Event [SFObject]
                       }
            | Wait     { polys  :: [Polygon],
                         done   :: Event (),
                         spawn  :: Event [SFObject]
                       }

-- Utility functions for determinine type of Object data structure.

isWait :: Object -> Bool
isWait obj = case obj of
    Wait _ _ _ -> True
    _          -> False

isGame :: Object -> Bool
isGame obj = case obj of
    Game _ _ _ _ _ _ -> True
    _                -> False

isShip :: Object -> Bool
isShip obj = case obj of
    Ship _ _ _ _ _ _ _ _ _ _ _ _ -> True
    _                            -> False

isMissile :: Object -> Bool
isMissile obj = case obj of
    Missile _ _ _ _ _ _ -> True
    _                   -> False

isAsteroid :: Object -> Bool
isAsteroid obj = case obj of
    Asteroid _ _ _ _ _ _ _ _ _ _ _ _  -> True
    _                                 -> False

data GameEvent = TurnLeft |
                 TurnRight |
                 Thruster |
                 Fire |
                 Hyperspace |
                 OtherKey Int |
                 Destroyed |
                 DestroyedLast |
                 Reanimate |
                 GameChange { scoreChanged :: Int,
                              shipDestroyed :: Bool,
                              newRound :: Bool }
    deriving (Show, Eq)

type SFObject = SF (Event GameEvent) Object
