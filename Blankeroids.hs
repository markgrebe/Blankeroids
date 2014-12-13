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
import Blankeroids.Saucer
import Blankeroids.Game

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
initialObjects g = playGame (listToIL gameObjects)
  where
    (g1,g2) = split g
    (g3,g4) = split g1
    aSFs = movingRandomAsteroids g2 (genInitialAsteroids g4)
    gameSF = playingGame g3 theGame
    waitSF = waitingUser g4 theWait
    gameObjects = [waitSF, gameSF] ++ aSFs

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
    -- Saucer data structures
    saucers = assocsIL $ filterIL (\(_, obj) -> isSaucer obj) objs
    -- Asteroid data structures
    asteroids = assocsIL $ filterIL (\(_, obj) -> isAsteroid obj) objs
    -- Misile data structures
    shipMissiles = assocsIL $ filterIL (\(_, obj) -> isShipMissile obj) objs
    -- Misile data structures
    saucerMissiles = assocsIL $ filterIL (\(_, obj) -> isSaucerMissile obj) objs
    -- Game data structures
    games = assocsIL $ filterIL (\(_, obj) -> isGame obj) objs
    -- First ship key
    ship' = if null ships then Nothing else Just (fst $ head ships)
    -- Ship data structure
    shipObj = if null ships then Nothing else lookupIL (fromJust ship') objs
    -- First saucer key
    saucer' = if null saucers then Nothing else Just (fst $ head saucers)
    -- First asteroid key
    asteroid' = if null asteroids then Nothing else Just (fst $ head asteroids)
    -- Game key
    game' = if null games then Nothing else Just (fst $ head games)
    -- Game data structure
    gameObj = fromJust $ lookupIL (fromJust game') objs

    -- Calculate the list of Ships and Objects which intersect
    sObjHits :: [(ILKey, Object)] -> [(ILKey, Object)] -> [(ILKey,ILKey)]
    sObjHits ((sk, so):[]) ((ak,ao):rest) =
      if polygonsInPolygons (polys so) (polys ao)
      then (sk, ak) : (sObjHits [(sk, so)] rest)
      else sObjHits [(sk, so)] rest
    sObjHits _        _              = []

    -- Calculate the list of Missile and Object collisions from a single
    -- missile
    mObjHits :: (ILKey, Object) -> [(ILKey, Object)] -> [(ILKey,ILKey)]
    mObjHits _        []             = []
    mObjHits (mk, mo) ((ok,oo):rest) =
      if pointInPolygons (pos2Point (pos mo)) (polys oo)
      then (mk, ok) : (mObjHits (mk, mo) rest)
      else mObjHits (mk, mo) rest

    -- Calculate the list of Missile and Object collisions from all
    -- missile
    msObjHits :: [(ILKey, Object)] -> [(ILKey, Object)] -> [(ILKey, ILKey)]
    msObjHits []     _  = []
    msObjHits (m:ms) as = (mObjHits m as) ++ (msObjHits ms as)

    -- Calculate the value of a destroyed asteroid
    getAsteroidValue :: ILKey -> Int
    getAsteroidValue k = asteroidValues !! (gen obj)
      where
        obj = fromJust $ lookupIL k objs

    -- Calculate the value of a destroyed saucer
    getSaucerValue :: ILKey -> Int
    getSaucerValue k = saucerValues !! (kind obj)
      where
        obj = fromJust $ lookupIL k objs

    -- Function which takes a list of asteroid associations, and makes sure
    -- none of the asteroids are in the safe zone around where the ship will
    -- be reanimated.
    safeToReanimate :: [(ILKey, Object)] -> Bool
    safeToReanimate []             = True
    safeToReanimate ((_,ao):rest) =
        if polygonInPolygons safeZone (polys ao)
        then False
        else safeToReanimate rest
      where
        safeDistance = 0.10
        safeZone = [(0.5 - safeDistance, 0.5 - safeDistance),
                    (0.5 - safeDistance, 0.5 + safeDistance),
                    (0.5 + safeDistance, 0.5 + safeDistance),
                    (0.5 + safeDistance, 0.5 - safeDistance),
                    (0.5 - safeDistance, 0.5 - safeDistance)]

    (shipMissileAsteroidsHits, asteroidShipMissileHits) =
        unzip $ msObjHits shipMissiles asteroids
    (saucerMissileAsteroidHits, asteroidSaucerMissileHits) =
        unzip $ msObjHits saucerMissiles asteroids
    (shipMissileSaucerHits, saucerShipMissileHits) =
        unzip $ msObjHits shipMissiles saucers
    (saucerMissileShipHits, shipSaucerMissileHits) =
        unzip $ msObjHits saucerMissiles ships
    (shipAsteroidHits, asteroidShipHits) = unzip $ sObjHits ships asteroids
    (saucerAsteroidHits, asteroidSaucerHits) = unzip $ sObjHits saucers asteroids
    (shipSaucerHits, saucerShipHits) = unzip $ sObjHits ships saucers
    shipMissileHits = shipMissileAsteroidsHits ++ shipMissileSaucerHits
    saucerMissileHits = saucerMissileAsteroidHits ++ saucerMissileShipHits

    -- Unique Asteroids destroyed
    asteroidHits = nub $ asteroidShipMissileHits ++ asteroidSaucerMissileHits ++
                         asteroidShipHits ++ asteroidSaucerHits
    -- Unique missiles destroyed
    missileHits = nub $ shipMissileHits ++ saucerMissileHits
    -- Unique Ships destoryed
    shipHits = nub $ shipAsteroidHits ++
                     shipSaucerMissileHits ++ shipSaucerHits
    -- Unique Saucers destoryed
    saucerHits = nub $ saucerAsteroidHits ++
                       saucerShipMissileHits ++ saucerShipHits
    -- List of all destroyed objects
    allHits = asteroidHits ++ missileHits ++ shipHits ++ saucerHits

    -- Find the values of asteroids and saucers destroyed by the ship, and
    -- calculate the updated score
    asteroidScoreHits = nub $ asteroidShipMissileHits ++ asteroidShipHits
    saucerScoreHits = saucerShipMissileHits ++ saucerShipHits
    scoreChanged' = sum $ map getAsteroidValue asteroidScoreHits ++
                          map getSaucerValue saucerScoreHits

    -- Have all asteroids been destroyed??
    roundComplete = length asteroidHits == length asteroids

    shipDestroyed' = length shipHits /= 0

    route' :: (ILKey, sf) -> (Event GameEvent, sf)
    route' (k,sfObj) | isJust game' && game' == Just k = routeGame (k,sfObj)
    route' (k,sfObj) | isJust ship' && ship' == Just k = routeShip (k,sfObj)
    route' (k,sfObj) | isJust saucer' && saucer' == Just k =
                       routeSaucer (k,sfObj)
    route' (k,sfObj) | isJust asteroid' && asteroid' == Just k =
                       routeAsteroid (k,sfObj)
    route' (k,sfObj) = routeRest (k,sfObj)

    routeShip :: (ILKey, sf) -> (Event GameEvent, sf)
    routeShip (_,sfObj) | reqReanimate $ fromJust shipObj =
                          if safeToReanimate (asteroids ++ saucers) &&
                             (lives gameObj) > 0
                          then (Event Reanimate, sfObj)
                          else (NoEvent, sfObj)
    routeShip (k,sfObj) = routeRest (k,sfObj)

    routeSaucer :: (ILKey, sf) -> (Event GameEvent, sf)
    routeSaucer (k',sfObj') = if k' `elem` allHits
                              then (Event Destroyed, sfObj')
                              else if isJust shipObj
                                  then (Event (ShipPosition
                                           (pos (fromJust shipObj))), sfObj')
                                  else (NoEvent, sfObj')

    routeAsteroid :: (ILKey, sf) -> (Event GameEvent, sf)
    routeAsteroid (k,sfObj') | reqReanimate $ fromJust $ lookupIL k objs =
                              (Event Reanimate, sfObj')
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
killOrSpawn (_, objs) = foldl (mergeBy (.)) noEvent (spawnEvents ++ doneEvents)
  where
    deleteAllIL :: IL SFObject -> IL SFObject
    deleteAllIL _ = emptyIL

    doneEvents :: [Event (IL SFObject -> IL SFObject)]
    doneEvents = [ if isWait obj
                   then (done obj) `tag` (deleteAllIL)
                   else (done obj) `tag` (deleteIL k) |
                   (k,obj) <- assocsIL objs ]

    spawnEvents :: [Event (IL SFObject -> IL SFObject)]
    spawnEvents = [ fmap (foldl (.) id . map insertIL_) (spawn obj) |
                        (_,obj) <- assocsIL objs ]
