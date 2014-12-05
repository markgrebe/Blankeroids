{-# LANGUAGE Arrows, ScopedTypeVariables, OverloadedStrings #-}

module Blankeroids.Ship where

import FRP.Yampa
import FRP.Yampa.Vector2

import Control.Monad.Random

import Blankeroids.Polygons
import Blankeroids.Types
import Blankeroids.Utils
import Blankeroids.Debris
import Blankeroids.Missiles

shipThrust    :: Double
shipThrust    = 10.0
shipDrag      :: Double
shipDrag      = 3.0

theShip :: Object
theShip = Ship {basePoly = shipPolygon, poly = shipPolygon,
                pos = vector2 0.5 0.5, vel = vector2 0.0 0.0,
                angPos = 0.0, radius = 0.016, thrusting = False,
                done = NoEvent, reqReanimate = False,
                reanimate = NoEvent, spawn = NoEvent }

movingShip :: RandomGen g => g -> Object -> SFObject
movingShip g s = proc ev -> do
    angPos' <- accumHoldBy accumAngPos 0.0 -< ev
    trigger' <- arr fireCannon -< ev
    (_, fire) <- magazine 4 4.5 -< trigger'
    (destroyed', hyperspace') <-
            accumHoldBy destroyHyperOccured (False, False) -< ev
    reqReanimate' <-
            accumHoldBy destroyOccured False <<< delayEvent 4.0 -< ev
    reqHyperEnd <-
            accumHoldBy hyperOccured False <<< delayEvent 1.0 -< ev
    rec
        accel <- arr calcAccel -< (ev, angPos', vel')
        vel' <- integral -< accel
    pos' <- wrapObject (radius s) <<< ((pos s) ^+^) ^<< integral -< vel'
    returnA -< s { poly = if (destroyed') then []
                          else transformPoly angPos'
                                (pos2Point pos') (basePoly s),
                   pos = pos', vel = vel', angPos = angPos',
                   thrusting = ((vector2Rho accel) > 0.1),
                   done = reanimateToUnit ev,
                   reqReanimate = reqReanimate' || reqHyperEnd,
                   spawn = foldl (mergeBy (++)) NoEvent
                        [(fire `tag` (addMissile pos' angPos')),
                         (destroyedToUnit ev `tag`
                            (evalRand (newDebris pos') g')),
                         (reanimateToUnit ev `tag`
                            (reanimateShip angPos' hyperspace'))] }
  where
    (g', g'') = split g

    destroyHyperOccured :: (Bool, Bool) -> GameEvent -> (Bool, Bool)
    destroyHyperOccured _       Destroyed  = (True, False)
    destroyHyperOccured _       Hyperspace = (True, True)
    destroyHyperOccured initial _          = initial

    destroyOccured :: Bool -> GameEvent -> Bool
    destroyOccured _       Destroyed  = True
    destroyOccured initial _          = initial

    hyperOccured :: Bool -> GameEvent -> Bool
    hyperOccured _       Hyperspace  = True
    hyperOccured initial _           = initial

    addMissile :: Position -> AngPosition -> [SFObject]
    addMissile p' ap' = [movingMissile $ newMissile p' ap']

    newMissile :: Position -> AngPosition -> Object
    newMissile p' ap' = Missile { basePoly = missilePolygon,
                            poly = missilePolygon,
                            pos = vector2 ((vector2X p') - 0.016 * sin ap')
                                          ((vector2Y p') + 0.016 * cos ap'),
                            vel = vector2 (-missileVel * sin ap')
                                          ( missileVel * cos ap'),
                            done = NoEvent, spawn  = NoEvent }

    -- Ammunition magazine. Reloaded up to maximal capacity at constant rate.
    -- n ... Maximal and initial number of missiles.
    -- f .......... Reload rate.
    -- input ...... Trigger.
    -- output ..... Tuple: #1: Current number of missiles in magazine.
    --                     #2: Missile fired event.
    -- Reused from Yampa Space Invaders Example
    -- Copyright (c) Yale University, 2003
    magazine :: Int -> Double -> SF (Event ()) (Int, Event ())
    magazine n f = proc trigger' -> do
      reload <- repeatedly (1/f) () -< ()
      (level,canFire) <- accumHold (n,True) -< (trigger' `tag` dec)
                                                `lMerge` (reload `tag` inc)
      returnA -< (level, trigger' `gate` canFire)
      where
        inc :: (Int,Bool) -> (Int, Bool)
        inc (l,_) | l < n = (l + 1, l > 0)
                  | otherwise = (l, True)
        dec :: (Int,Bool) -> (Int, Bool)
        dec (l,_) | l > 0 = (l - 1, True)
                  | otherwise = (l, False)

    reanimateShip :: AngPosition -> Bool-> [SFObject]
    reanimateShip ap' h' = [movingShip g'' (newShip ap' h')]

    randomPosition :: RandomGen g => Rand g Position
    randomPosition = do
        x <- getRandomR(0.1,0.9)
        y <- getRandomR(0.1,0.9)
        return (vector2 x y)

    newShip :: AngPosition -> Bool -> Object
    newShip ap'' h'' = Ship {basePoly = shipPolygon, poly = shipPolygon,
                         pos = startPos, vel = vector2 0.0 0.0,
                         angPos = ap'', radius = 0.016, thrusting = False,
                         done = NoEvent, reqReanimate = False,
                         reanimate = NoEvent, spawn = NoEvent }
      where
        startPos = if h''
                   then evalRand randomPosition g'
                   else vector2 0.5 0.5

    accumAngPos :: Double -> GameEvent -> Double
    accumAngPos start TurnRight = start - (pi / 8)
    accumAngPos start TurnLeft  = start + (pi / 8)
    accumAngPos start _         = start

    fireCannon :: Event GameEvent -> Event ()
    fireCannon (Event Fire) = Event ()
    fireCannon _            = NoEvent

    calcAccel :: (Event GameEvent, AngPosition, Velocity) -> Acceleration
    calcAccel (Event Thruster, currAngPos, _) =
        vector2 (-shipThrust * sin currAngPos) (shipThrust * cos currAngPos)
    calcAccel (_             , _     , currVel) =
        let mag = vector2Rho currVel
            (xVel,yVel) = vector2XY currVel
        in if (mag < shipDrag)
            then vector2 (-xVel) (-yVel)
            else vector2 (-shipDrag * xVel / mag) (-shipDrag * yVel / mag)

    newDebris :: RandomGen g => Position -> Rand g [SFObject]
    newDebris p = do
        debris <- mapM (oneDebris p) [0..(length shipDebrisPolygons - 1)]
        return debris

    oneDebris :: RandomGen g => Position -> Int -> Rand g SFObject
    oneDebris p i = do
        life'  <- getRandomR(0.8,1.6)
        vel'   <- getRandomR(0.03,0.08)
        angle' <- getRandomR(0.0, 2*pi)
        return (movingDebris
                Debris { basePoly = shipDebrisPolygons !! i,
                         poly = shipDebrisPolygons !! i, pos = p,
                         vel = vector2 (vel' * sin angle') (vel' * cos angle'),
                         life = life', done = NoEvent, spawn = NoEvent } )

---------------------------------------------------

shipPolygon :: Polygon
shipPolygon = [( -0.008,-0.016),(0.000,0.016),(0.008,-0.016),
                ( 0.000,-0.012),(-0.008,-0.016)]

thrustPolygon :: Polygon
thrustPolygon = [(0.000,-0.012),(0.006,-0.018),(0.000,-0.024),
                ( -0.006,-0.018),(0.000,-0.012)]

