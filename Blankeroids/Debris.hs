{-# LANGUAGE Arrows, ScopedTypeVariables, OverloadedStrings #-}

module Blankeroids.Debris where

import FRP.Yampa

import Blankeroids.Polygons
import Blankeroids.Types

movingDebris :: Object -> SFObject
movingDebris d = proc _ -> do
    -- Calculate the xy position, based on integral of velocity.
    pos' <- ((pos d) ^+^) ^<< integral -< (vel d)
    -- Debris fades away after a certain lifetime
    done' <- after (life d) () -< ()
    returnA -< d { poly = translatePoly (pos2Point pos') (basePoly d),
                   pos = pos', done = done'}

---------------------------------------------------

dustPolygon :: Polygon
dustPolygon = [( -0.001,-0.001),(-0.001, 0.001),(0.001, 0.001),
                  ( 0.001,-0.001)]

shipDebrisPolygons :: [Polygon]
shipDebrisPolygons = [[( -0.006,-0.016),(0.000,0.016)],
                      [(0.000,0.016),(0.006,-0.016)],
                      [(0.000,-0.024),( -0.004,-0.018)],
                      [( 0.000,-0.012),(-0.006,-0.016)]]
