{-# LANGUAGE Arrows, ScopedTypeVariables, OverloadedStrings #-}

module Main where

import Graphics.Blank hiding (Event)
import qualified Graphics.Blank (Event)
import FRP.Yampa
import Data.List(nub)
import Data.Maybe

import FRP.Yampa.Canvas

import Control.Monad.Random

import IdentityList

import Blankeroids.Graphics
import Blankeroids.Polygons
import Blankeroids.Types
import Blankeroids.Asteroids
import Blankeroids.Game
import Blankeroids.Ship

---------------------------------------------------
main :: IO ()
main = do
    g <- newStdGen
    blankCanvas 3000 { events = ["keydown"] } (animateAsteriods g)

-- | Display an animation of multiple asteroids.
animateAsteriods :: RandomGen g => g -> DeviceContext -> IO ()
animateAsteriods g = reactimateSFinContext handleKeyEvents
                                           renderScene
                                           (movingObjects
                                              g' (genInitialAsteroids g'')
                                              theShip theGame)
  where
    (g', g'') = split g

---------------------------------------------------

rightKey :: Int
rightKey = 39
leftKey  :: Int
leftKey  = 37
upKey    :: Int
upKey    = 38
downKey  :: Int
downKey  = 40
spaceKey :: Int
spaceKey = 32

handleKeyEvents :: Graphics.Blank.Event -> Canvas (Event GameEvent)
handleKeyEvents blankEvent = do
    let keyEvent = case (eWhich blankEvent) of
                    Just a | a == rightKey -> Event TurnRight
                           | a == leftKey  -> Event TurnLeft
                           | a == upKey    -> Event Thruster
                           | a == downKey  -> Event Hyperspace
                           | a == spaceKey -> Event Fire
                           | otherwise     -> NoEvent
                    Nothing                -> NoEvent
    return keyEvent

-- | Construct a list of moving game objects from a list of
--   initial configurations.
movingObjects :: RandomGen g => g -> [Object] -> Object -> Object ->
                    SF (Event GameEvent) (IL Object)
movingObjects g as ship gm = playGame (listToIL ([gameSF, shipSF] ++ aSFs))
  where
    (g1, g2) = split g
    aSFs = movingRandomAsteroids g1 as
    shipSF = movingShip g2 ship
    gameSF = playingGame gm

playGame :: IL SFObject -> SF (Event GameEvent) (IL Object)
playGame sfobjs = proc ev -> do
  rec
    objs <- game sfobjs -< (ev, objsp)
    objsp <- iPre emptyIL -< objs
  returnA -< objs
    where
        game :: IL SFObject -> SF (Event GameEvent, IL Object) (IL Object)
        game objs = dpSwitch route objs
                             (arr killOrSpawn >>> notYet)
                             (\sfs f -> game (f sfs))

route :: (Event GameEvent, IL Object) -> IL sf -> IL (Event GameEvent, sf)
route (keyEv,objs) sfs = mapIL route' sfs
  where
    ships = assocsIL $ filterIL (\(_, obj) -> isShip obj) objs
    asteroids = assocsIL $ filterIL (\(_, obj) -> isAsteroid obj) objs
    missiles = assocsIL $ filterIL (\(_, obj) -> isMissile obj) objs
    games = assocsIL $ filterIL (\(_, obj) -> isGame obj) objs
    ship' = if null ships then Nothing else Just (fst $ head ships)
    shipObj = if null ships then Nothing else lookupIL (fromJust ship') objs
    asteroid' = if null asteroids then Nothing else Just (fst $ head asteroids)
    game' = if null games then Nothing else Just (fst $ head games)
    gameObj = fromJust $ lookupIL (fromJust game') objs

    sAsHits :: [(ILKey, Object)] -> [(ILKey, Object)] -> [(ILKey,ILKey)]
    sAsHits ((sk, so):[]) ((ak,ao):rest) =
      if polygonInPolygon (poly so) (poly ao)
      then (sk, ak) : (sAsHits [(sk, so)] rest)
      else sAsHits [(sk, so)] rest
    sAsHits _        _              = []

    mAsHits :: (ILKey, Object) -> [(ILKey, Object)] -> [(ILKey,ILKey)]
    mAsHits _        []             = []
    mAsHits (mk, mo) ((ak,ao):rest) =
      if pointInPolygon (pos2Point (pos mo)) (poly ao)
      then (mk, ak) : (mAsHits (mk, mo) rest)
      else mAsHits (mk, mo) rest

    msAsHits :: [(ILKey, Object)] -> [(ILKey, Object)] -> [(ILKey, ILKey)]
    msAsHits []     _  = []
    msAsHits (m:ms) as = (mAsHits m as) ++ (msAsHits ms as)

    getAsteroidValue :: ILKey -> Int
    getAsteroidValue k = asteroidValues !! (gen obj)
      where
        obj = fromJust $ lookupIL k objs

    safeDistance = 0.10
    safeZone = [(0.5 - safeDistance, 0.5 - safeDistance),
                (0.5 - safeDistance, 0.5 + safeDistance),
                (0.5 + safeDistance, 0.5 + safeDistance),
                (0.5 + safeDistance, 0.5 - safeDistance),
                (0.5 - safeDistance, 0.5 - safeDistance)]
    safeToReanimate :: [(ILKey, Object)] -> Bool
    safeToReanimate []             = True
    safeToReanimate ((_,ao):rest) =
      if polygonInPolygon (poly ao) safeZone
      then False
      else safeToReanimate rest

    (missileHits, asteroidMissileHits) = unzip $ msAsHits missiles asteroids
    (shipHits, asteroidShipHits) = unzip $ sAsHits ships asteroids
    asteroidHits = nub $ asteroidMissileHits ++ asteroidShipHits
    missileHits' = nub missileHits
    shipHits' = nub shipHits
    allHits = asteroidHits ++ missileHits' ++ shipHits'
    roundComplete = length asteroidHits == length asteroids
    scoreChanged' = sum $ map getAsteroidValue asteroidHits
    shipDestroyed' = length shipHits /= 0

    route' :: (ILKey, sf) -> (Event GameEvent, sf)
    route' (k,sfObj) | isJust game' && game' == Just k = routeGame (k,sfObj)
    route' (k,sfObj) | isJust ship' && ship' == Just k = routeShip (k,sfObj)
    route' (k,sfObj) | isJust asteroid' && asteroid' == Just k =
                       routeAsteroid (k,sfObj)
    route' (k,sfObj) = routeRest (k,sfObj)

    routeShip :: (ILKey, sf) -> (Event GameEvent, sf)
    routeShip (_,sfObj) | reqReanimate $ fromJust shipObj =
                          if safeToReanimate asteroids &&
                             (lives gameObj) > 0
                          then (Event Reanimate, sfObj)
                          else (NoEvent, sfObj)
    routeShip (k,sfObj) = routeRest (k,sfObj)

    routeAsteroid :: (ILKey, sf) -> (Event GameEvent, sf)
    routeAsteroid (k,sfObj) | reqReanimate $ fromJust $ lookupIL k objs =
                              (Event Reanimate, sfObj)
    routeAsteroid (_,sfObj') = if roundComplete
                                then (Event DestroyedLast, sfObj')
                                else (NoEvent, sfObj')

    routeGame :: (ILKey, sf) -> (Event GameEvent, sf)
    routeGame (_,sfObj) =
        if scoreChanged' == 0 && (not shipDestroyed') && (not roundComplete)
        then (NoEvent, sfObj)
        else (Event (GameChange {scoreChanged = scoreChanged',
                                 shipDestroyed = shipDestroyed',
                                 newRound = roundComplete}), sfObj)

    routeRest :: (ILKey, sf) -> (Event GameEvent, sf)
    routeRest (k',sfObj') = if k' `elem` allHits
                            then (Event Destroyed, sfObj')
                            else (keyEv, sfObj')

killOrSpawn :: ((Event GameEvent, IL Object) , IL Object) ->
                    Event (IL SFObject -> IL SFObject)
killOrSpawn (_, objs) = foldl (mergeBy (.)) noEvent (doneEvents ++ spawnEvents)
  where
    doneEvents :: [Event (IL SFObject -> IL SFObject)]
    doneEvents = [ (done obj) `tag` (deleteIL k) | (k,obj) <- assocsIL objs ]

    spawnEvents :: [Event (IL SFObject -> IL SFObject)]
    spawnEvents = [ fmap (foldl (.) id . map insertIL_) (spawn obj) |
                        (_,obj) <- assocsIL objs ]
