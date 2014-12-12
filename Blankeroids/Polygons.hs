{-# LANGUAGE Arrows, ScopedTypeVariables, OverloadedStrings #-}

module Blankeroids.Polygons where

import FRP.Yampa.Vector2
import Blankeroids.Types

pos2Point :: Position -> Point
pos2Point p = (vector2X p, vector2Y p)

-- Create a List of edges of a polygon from a list of it's vertices.
-- A polygon must have two vertices to have an edge.
polyEdges :: Polygon -> [Edge]
polyEdges []      = []
polyEdges [_]     = []
polyEdges polygon = zip polygon (tail polygon ++ [head polygon])

rotatePoly :: AngPosition -> Polygon -> Polygon
rotatePoly ang polygon = map rotatePoint polygon
  where
    rotatePoint p = (x', y')
      where
        x = fst p
        y = snd p
        x' = x * cos ang - y * sin ang
        y' = x * sin ang + y * cos ang

translatePoly :: Point -> Polygon -> Polygon
translatePoly dp polygon = map translatePoint polygon
  where
    translatePoint p = (fst p + fst dp, snd p + snd dp)

translatePolys :: Point -> [Polygon] -> [Polygon]
translatePolys dp polygons = map (translatePoly dp) polygons

transformPoly :: AngPosition -> Point -> Polygon -> Polygon
transformPoly ang dp = (translatePoly dp).(rotatePoly ang)

scalePoly :: Double -> Polygon -> Polygon
scalePoly scaleFactor polygon = map scalePoint polygon
 where
    scalePoint p = (scaleFactor * fst p, scaleFactor * snd p)

-- Return a boolean indicating if a point is inside of a polygon.
pointInPolygon :: Point -> Polygon -> Bool
pointInPolygon point polygon = foldr acumNode False (polyEdges polygon)
  where
    px = fst point
    py = snd point

    acumNode :: Edge -> Bool -> Bool
    acumNode ed b = ((polY1 < py && polY2 >= py ||
                      polY2 < py && polY1 >= py) &&
                     (polX1 +
                      (py - polY1)/(polY2 - polY1)*(polX2-polX1) < px)) /= b
      where
        polX1 = fst $ fst ed
        polX2 = fst $ snd ed
        polY1 = snd $ fst ed
        polY2 = snd $ snd ed

pointInPolygons :: Point -> [Polygon] -> Bool
pointInPolygons point polygons = or $ map (pointInPolygon point) polygons

-- Return a boolean indicating if polygons intersect
polygonInPolygon :: Polygon -> Polygon -> Bool
polygonInPolygon poly1 poly2 = or $ map (flip pointInPolygon poly2) poly1

polygonInPolygons :: Polygon -> [Polygon] -> Bool
polygonInPolygons poly' polys' = or $ map (polygonInPolygon poly') polys'

polygonsInPolygons :: [Polygon] -> [Polygon] -> Bool
polygonsInPolygons polys1 polys2 = or $ map (flip polygonInPolygons polys1) polys2

