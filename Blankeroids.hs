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
import Blankeroids.Wait

---------------------------------------------------
main :: IO ()
main = do
    g <- newStdGen
    blankCanvas 3000 { events = ["keydown"] } (animateAsteriods g)

-- | main reactimate loop with key event handling input function, Canvas
--   output function.
animateAsteriods :: RandomGen g => g -> DeviceContext -> IO ()
animateAsteriods g = reactimateSFinContext handleKeyEvents
                                           renderScene
                                           (initialObjects g)

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
                           | otherwise     -> Event (OtherKey a)
                    Nothing                -> NoEvent
    return keyEvent

-- | Construct a list of intial objects for the game, in this case the
--   game object which tracks score, and the wait object which asks
--   the user to press any key.
initialObjects :: RandomGen g => g -> SF (Event GameEvent) (IL Object)
initialObjects g = playGame (listToIL [waitSF g, gameSF])
  where
    waitSF g' = waitingUser g' theWait
    gameSF = playingGame theGame

-- Main signal game signal function, looping the object data structures back in
-- as inputs.
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

-- Route routine which routes input Events to the proper Object signal
-- functions.
route :: (Event GameEvent, IL Object) -> IL sf -> IL (Event GameEvent, sf)
route (keyEv,objs) sfs = mapIL route' sfs
  where
    -- Ship data structures
    ships = assocsIL $ filterIL (\(_, obj) -> isShip obj) objs
    -- Asteroid data structures
    asteroids = assocsIL $ filterIL (\(_, obj) -> isAsteroid obj) objs
    -- Misile data structures
    missiles = assocsIL $ filterIL (\(_, obj) -> isMissile obj) objs
    -- Game data structures
    games = assocsIL $ filterIL (\(_, obj) -> isGame obj) objs
    -- First ship key
    ship' = if null ships then Nothing else Just (fst $ head ships)
    -- Ship data structure
    shipObj = if null ships then Nothing else lookupIL (fromJust ship') objs
    -- First asteroid key
    asteroid' = if null asteroids then Nothing else Just (fst $ head asteroids)
    -- Game key
    game' = if null games then Nothing else Just (fst $ head games)
    -- Game data structure
    gameObj = fromJust $ lookupIL (fromJust game') objs

    -- Calculate the list of Ships and Asteroids which intersect
    sAsHits :: [(ILKey, Object)] -> [(ILKey, Object)] -> [(ILKey,ILKey)]
    sAsHits ((sk, so):[]) ((ak,ao):rest) =
      if polygonInPolygon (poly so) (poly ao)
      then (sk, ak) : (sAsHits [(sk, so)] rest)
      else sAsHits [(sk, so)] rest
    sAsHits _        _              = []

    -- Calculate the list of Missile and Asteroid collisions from a single
    -- missile
    mAsHits :: (ILKey, Object) -> [(ILKey, Object)] -> [(ILKey,ILKey)]
    mAsHits _        []             = []
    mAsHits (mk, mo) ((ak,ao):rest) =
      if pointInPolygon (pos2Point (pos mo)) (poly ao)
      then (mk, ak) : (mAsHits (mk, mo) rest)
      else mAsHits (mk, mo) rest

    -- Calculate the list of Missile and Asteroid collisions from all
    -- missile
    msAsHits :: [(ILKey, Object)] -> [(ILKey, Object)] -> [(ILKey, ILKey)]
    msAsHits []     _  = []
    msAsHits (m:ms) as = (mAsHits m as) ++ (msAsHits ms as)

    -- Calculate the value of a destroyed asteroid
    getAsteroidValue :: ILKey -> Int
    getAsteroidValue k = asteroidValues !! (gen obj)
      where
        obj = fromJust $ lookupIL k objs

    -- Function which takes a list of asteroid associations, and makes sure
    -- none of the asteroids are in the safe zone around where the ship will
    -- be reanimated.
    safeToReanimate :: [(ILKey, Object)] -> Bool
    safeToReanimate []             = True
    safeToReanimate ((_,ao):rest) =
        if polygonInPolygon (poly ao) safeZone
        then False
        else safeToReanimate rest
      where
        safeDistance = 0.10
        safeZone = [(0.5 - safeDistance, 0.5 - safeDistance),
                    (0.5 - safeDistance, 0.5 + safeDistance),
                    (0.5 + safeDistance, 0.5 + safeDistance),
                    (0.5 + safeDistance, 0.5 - safeDistance),
                    (0.5 - safeDistance, 0.5 - safeDistance)]

    (missileHits, asteroidMissileHits) = unzip $ msAsHits missiles asteroids
    (shipHits, asteroidShipHits) = unzip $ sAsHits ships asteroids
    asteroidHits = nub $ asteroidMissileHits ++ asteroidShipHits
    -- Unique missiles destroyed
    missileHits' = nub missileHits
    -- Unique Ships destoryed
    shipHits' = nub shipHits
    allHits = asteroidHits ++ missileHits' ++ shipHits'
    -- All asteroids destroyed??
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
    routeAsteroid (k,sfObj') = if roundComplete
                                then (Event DestroyedLast, sfObj')
                                else routeRest (k,sfObj')

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

-- Function which creates and deletes signal functions based on requests from
-- curently running signal functions through their done and spawn fields of
-- the output data structures.
killOrSpawn :: ((Event GameEvent, IL Object) , IL Object) ->
                    Event (IL SFObject -> IL SFObject)
killOrSpawn (_, objs) = foldl (mergeBy (.)) noEvent (doneEvents ++ spawnEvents)
  where
    doneEvents :: [Event (IL SFObject -> IL SFObject)]
    doneEvents = [ (done obj) `tag` (deleteIL k) | (k,obj) <- assocsIL objs ]

    spawnEvents :: [Event (IL SFObject -> IL SFObject)]
    spawnEvents = [ fmap (foldl (.) id . map insertIL_) (spawn obj) |
                        (_,obj) <- assocsIL objs ]
