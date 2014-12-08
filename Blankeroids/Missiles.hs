{-# LANGUAGE Arrows, ScopedTypeVariables, OverloadedStrings #-}

module Blankeroids.Missiles where

import FRP.Yampa

import Blankeroids.Polygons
import Blankeroids.Types
import Blankeroids.Utils

missileVel    :: Double
missileVel    = 0.5
missileLife   :: Double
missileLife   = 1.0

movingMissile :: Object -> SFObject
movingMissile m = proc ev -> do
    -- Calculate the xy position, based on integral of velocity.
    pos' <- ((pos m) ^+^) ^<< integral -< (vel m)
    -- Missile self destructs after a certain lifetime
    done' <- after missileLife () -< ()
    returnA -< m {poly = translatePoly (pos2Point pos') (basePoly m),
                  pos = pos', done = merge done' (destroyedToUnit ev)}

---------------------------------------------------

missilePolygon :: Polygon
missilePolygon = [( -0.001,-0.001),(-0.001, 0.001),(0.001, 0.001),
                  ( 0.001,-0.001)]
