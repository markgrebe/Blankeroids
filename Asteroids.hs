{-# LANGUAGE Arrows, ScopedTypeVariables, OverloadedStrings #-}

module Main where

import Graphics.Blank hiding (Event)
import qualified Graphics.Blank (Event)
import FRP.Yampa
import FRP.Yampa.Vector2
import Data.Char(digitToInt)
import Data.Fixed(mod')
import Data.List(nub)
import Data.Maybe

import FRP.Yampa.Canvas

import Control.Monad
import Control.Monad.Random

import IdentityList

import Debug.Trace
-- import Debug.Hood.Observe

---------------------------------------------------
type Position = Vector2 Double
type Velocity = Vector2 Double
type Acceleration = Vector2 Double
type AngPosition = Double
type AngVelocity = Double

---------------------------------------------------

data Object = Asteroid { poly   :: Polygon,
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
            | Ship     { poly   :: Polygon,
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
            | Missile  { poly   :: Polygon,
                         pos    :: Position,
                         vel    :: Velocity,
                         done   :: Event (),
                         spawn  :: Event [SFObject]
                       }
            | Debris   { poly   :: Polygon,
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

type SFObject = SF (Event GameEvent) Object

isGame :: Object -> Bool
isGame obj = case obj of
    Game _ _ _ _ _ _ -> True
    _                -> False

isShip :: Object -> Bool
isShip obj = case obj of
    Ship _ _ _ _ _ _ _ _ _ _ -> True
    _                        -> False

isMissile :: Object -> Bool
isMissile obj = case obj of
    Missile _ _ _ _ _ -> True
    _                 -> False

isAsteroid :: Object -> Bool
isAsteroid obj = case obj of
    Asteroid _ _ _ _ _ _ _ _ _ _ _  -> True
    _                               -> False

initAsterVel  :: Double
initAsterVel  = 0.1
initAsterNum  :: Int
initAsterNum  = 4
newAsterNum   :: Int
newAsterNum   = 6
centerMinEdge :: Double
centerMinEdge = 0.4
centerMaxEdge :: Double
centerMaxEdge = 0.6
asterGen1Rad  :: Double
asterGen1Rad  = 0.064
shipThrust    :: Double
shipThrust    = 10.0
shipDrag      :: Double
shipDrag      = 3.0
missileVel    :: Double
missileVel    = 0.5
missileLife   :: Double
missileLife   = 1.0
asteroidValues :: [Int]
asteroidValues = [20, 50, 100]

randGenList :: RandomGen g => g -> [g]
randGenList g = g' : randGenList g''
  where
    (g', g'') = split g

initAsteroid :: RandomGen g => Int -> Rand g Object
initAsteroid round' = do
    -- Get an initial position on the edge of the screen.
    initPos <- getRandomR (0.0, 4.0)
    let (x,y) = if initPos < 1.0 then (initPos, 0.0)
           else if initPos < 2.0 then (1.0, initPos - 1.0)
           else if initPos < 3.0 then (initPos - 2.0, 1.0)
           else                       (0.0, initPos - 3.0)
    -- Get a random angle for velocity
    thetaRand <- getRandomR (0.0, pi)
    -- Get a random angle for position
    angRand <- getRandomR (0.0, 2 * pi)
    -- Get a random angular velocity
    angVelRand <- getRandomR (-0.5,0.5)
    -- Get a random asteroid shape
    asterIndex <- getRandomR (0,length(asterPolygons) - 1)
    -- Based on quadrant of the screen of x,y, set velocity in the general
    -- direction of the center
    let thetav = if y == 0.0 then thetaRand -- ^ thetaRand - pi / 2
            else if x == 1.0 then thetaRand + pi / 2 -- ^ thetaRand - pi
            else if y == 1.0 then thetaRand + pi -- ^ thetaRand + pi / 2
            else                  thetaRand - pi / 2 -- ^ thetaRand
    -- Magnitude of the velocity should be initAsterVel
    let xv = initAsterVel * cos thetav
    let yv = initAsterVel * sin thetav
    return Asteroid {   pos    = vector2 x y,
                        vel    = vector2 xv yv,
                        angPos = angRand,
                        angVel = angVelRand,
                        gen    = 0,
                        gameRound  = round',
                        radius = asterGen1Rad,
                        reqReanimate = False,
                        done   = NoEvent,
                        spawn  = NoEvent,
                        poly   = asterPolygons !! asterIndex
                    }

newAsteroids :: RandomGen g => Position -> Int -> Int -> Rand g [Object]
newAsteroids p currgen round' = do
    asters <- replicateM 2 oneAsteroid
    return asters
  where
    sizeScale :: Double
    sizeScale = 1.0 / fromIntegral ((currgen + 1) * 2)
    velScale :: Double
    velScale = if currgen == 0 then 1.5 else 2.0
    oneAsteroid :: RandomGen g => Rand g Object
    oneAsteroid = do
        -- Get a random angle for velocity
        thetaRand <- getRandomR (0.0, 2 * pi)
        -- Get a random angle for position
        angRand <- getRandomR (0.0, 2 * pi)
        -- Get a random angular velocity
        angVelRand <- getRandomR (-0.5,0.5)
        -- Get a random asteroid
        asterIndex <- getRandomR (0,length(asterPolygons) - 1)
        let xv = velScale * initAsterVel * cos thetaRand
        let yv = velScale * initAsterVel * sin thetaRand
        return
            Asteroid { pos = p, vel = vector2 xv yv,
                       angPos = angRand, angVel = angVelRand,
                       gen = currgen + 1, gameRound = round',
                       radius = sizeScale * asterGen1Rad,
                       reqReanimate = False, done = NoEvent, spawn = Event [],
                       poly = scalePoly sizeScale (asterPolygons !! asterIndex)}

initAsteroids :: RandomGen g => Int -> Int -> Rand g [Object]
initAsteroids count round' = replicateM count (initAsteroid round')

genInitialAsteroids :: RandomGen g => g -> [Object]
genInitialAsteroids g = evalRand (initAsteroids initAsterNum 1) g

theShip :: Object
theShip = Ship {poly = shipPolygon,
                pos = vector2 0.5 0.5, vel = vector2 0.0 0.0,
                angPos = 0.0, radius = 0.016, thrusting = False,
                done = NoEvent, reqReanimate = False,
                reanimate = NoEvent, spawn = NoEvent }

theGame :: Object
theGame = Game {score = 0, lives = 3, theRound = 1, gameOver = False,
                done = NoEvent, spawn = NoEvent }

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

data GameEvent = TurnLeft |
                 TurnRight |
                 Thruster |
                 Fire |
                 Hyperspace |
                 Destroyed |
                 DestroyedLast |
                 Reanimate |
                 GameChange { scoreChanged :: Int,
                              shipDestroyed :: Bool,
                              newRound :: Bool }
    deriving (Show, Eq)

destroyedToUnit :: Event GameEvent -> Event ()
destroyedToUnit (Event Destroyed) = Event ()
destroyedToUnit _                     = NoEvent

reanimateToUnit :: Event GameEvent -> Event ()
reanimateToUnit (Event Reanimate) = Event ()
reanimateToUnit _                 = NoEvent

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

movingRandomAsteroids :: RandomGen g => g -> [Object] -> [SFObject]
movingRandomAsteroids g as = map randomAster gensAsters
  where
    gens = take (length as) (randGenList g)
    gensAsters = zip gens as
    randomAster (g', a) = movingAsteroid g' a

movingAsteroid :: RandomGen g => g -> Object -> SFObject
movingAsteroid g a = proc ev -> do
    pos' <- wrapObject (radius a) <<< ((pos a) ^+^) ^<< integral -< (vel a)
    angPos' <- ((angPos a) +) ^<< integral -< (angVel a)
    destroyed' <- accumHoldBy destroyedOccured False -< ev
    reqReanimate' <- accumHoldBy destroyedOccured False <<< delayEvent 5.0 -< ev
    returnA -< a { poly = if (destroyed') then [] else (poly a),
                   pos = pos', angPos = angPos',
                   reqReanimate = reqReanimate',
                   done = merge (reanimateToUnit ev) (destroyedToUnit ev),
                   spawn = mergeBy (++)
                           (anyDestroyedToUnit ev `tag` newObjects a pos')
                           (reanimateToUnit ev `tag` reanimateAsteroids ) }
  where
    (g1, g2) = split g
    (g3, g4) = split g1

    destroyedOccured :: Bool -> GameEvent -> Bool
    destroyedOccured _       Destroyed     = True
    destroyedOccured _       DestroyedLast = True
    destroyedOccured initial _             = initial

    anyDestroyedToUnit :: Event GameEvent -> Event ()
    anyDestroyedToUnit (Event Destroyed)     = Event ()
    anyDestroyedToUnit (Event DestroyedLast) = Event ()
    anyDestroyedToUnit _                     = NoEvent

    reanimateAsteroids :: [SFObject]
    reanimateAsteroids =
        movingRandomAsteroids g3 (evalRand (initAsteroids numAsteroids gameRound') g4)
      where
        gameRound' = gameRound a
        numAsteroids = initAsterNum + 2 * gameRound'

    newObjects :: Object -> Position -> [SFObject]
    newObjects a' p =
        evalRand (newDebris p) g2 ++
        if gen a' <= 1
        then movingRandomAsteroids g3 (evalRand (newAsteroids p (gen a') (gameRound a')) g4)
        else []

    newDebris :: RandomGen g => Position -> Rand g [SFObject]
    newDebris p = do
        debris <- replicateM 16 (oneDebris p)
        return debris

    oneDebris :: RandomGen g => Position -> Rand g SFObject
    oneDebris p = do
        life'  <- getRandomR(0.1,0.8)
        vel'   <- getRandomR(0.05,0.15)
        angle' <- getRandomR(0.0, 2*pi)
        return (movingDebris
                Debris { poly = dustPolygon, pos = p,
                         vel = vector2 (vel' * sin angle') (vel' * cos angle'),
                         life = life', done = NoEvent, spawn = NoEvent } )

movingMissile :: Object -> SFObject
movingMissile m = proc ev -> do
    pos' <- ((pos m) ^+^) ^<< integral -< (vel m)
    done' <- after missileLife () -< ()
    returnA -< m { pos = pos', done = merge done' (destroyedToUnit ev)}

movingDebris :: Object -> SFObject
movingDebris d = proc _ -> do
    pos' <- ((pos d) ^+^) ^<< integral -< (vel d)
    done' <- after (life d) () -< ()
    returnA -< d { pos = pos', done = done'}

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
    returnA -< s { poly = if (destroyed') then [] else shipPolygon,
                   pos = pos', vel = vel', angPos = angPos',
                   thrusting = ((vector2Rho accel) > 0.1),
                   done = reanimateToUnit ev,
                   reqReanimate = reqReanimate' || reqHyperEnd,
                   spawn = foldl (mergeBy (++)) NoEvent
                        [(fire `tag` (addMissile pos' angPos')),
                         (destroyedToUnit ev `tag` (evalRand (newDebris pos') g')),
                         (reanimateToUnit ev `tag` (reanimateShip angPos' hyperspace'))] }
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
    newMissile p' ap' = Missile { poly = missilePolygon,
                                  pos = vector2 ((vector2X p') - 0.016 * sin ap')
                                                ((vector2Y p') + 0.016 * cos ap'), -- ToDo: caculate ship tip pos
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
    newShip ap'' h'' = Ship {poly = shipPolygon,
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
                Debris { poly = shipDebrisPolygons !! i, pos = p,
                         vel = vector2 (vel' * sin angle') (vel' * cos angle'),
                         life = life', done = NoEvent, spawn = NoEvent } )

playingGame :: Object -> SFObject
playingGame gm = proc ev -> do
    score' <- accumHoldBy accumScore 0 -< ev
    -- ToDo: Bonus lives
    lives' <- accumHoldBy accumLives 3 -< ev
    round' <- accumHoldBy accumRounds 1 -< ev
    gameOver' <- hold False <<< edgeTag True -< lives' <= 0
    returnA -< gm { score = score', lives = lives', theRound = round',
                    gameOver = gameOver' }
  where
    accumScore :: Int -> GameEvent -> Int
    accumScore start GameChange { scoreChanged = s} = start + s
    accumScore start _         = start

    accumLives :: Int -> GameEvent -> Int
    accumLives start GameChange {shipDestroyed = sd} | sd == True = start - 1
    accumLives start _             = start

    accumRounds :: Int -> GameEvent -> Int
    accumRounds start GameChange {newRound = rc} | rc == True = start + 1
    accumRounds start _                                       = start

-- | Construct a list of moving game objects from a list of initial configurations.
movingObjects :: RandomGen g => g -> [Object] -> Object -> Object -> SF (Event GameEvent) (IL Object)
movingObjects g as ship gm = playGame (listToIL ([gameSF, shipSF] ++ aSFs))
  where
    (g1, g2) = split g
    aSFs = movingRandomAsteroids g1 as
    shipSF = movingShip g2 ship
    gameSF = playingGame gm

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
      if polygonInPolygon sPolygon aPolygon
      then (sk, ak) : (sAsHits [(sk, so)] rest)
      else sAsHits [(sk, so)] rest
        where
          sPolygon = transformPoly (angPos so) (pos2Point (pos so)) (poly so)
          aPolygon = transformPoly (angPos ao) (pos2Point (pos ao)) (poly ao)
    sAsHits _        _              = []

    mAsHits :: (ILKey, Object) -> [(ILKey, Object)] -> [(ILKey,ILKey)]
    mAsHits _        []             = []
    mAsHits (mk, mo) ((ak,ao):rest) =
      if pointInPolygon (pos2Point (pos mo)) polygon'
      then (mk, ak) : (mAsHits (mk, mo) rest)
      else mAsHits (mk, mo) rest
        where
          polygon' = transformPoly (angPos ao) (pos2Point (pos ao)) (poly ao)

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
      if polygonInPolygon polygon' safeZone
      then False
      else safeToReanimate rest
        where
          polygon' = transformPoly (angPos ao) (pos2Point (pos ao)) (poly ao)

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

killOrSpawn :: ((Event GameEvent, IL Object) , IL Object) -> Event (IL SFObject -> IL SFObject)
killOrSpawn (_, objs) = foldl (mergeBy (.)) noEvent (doneEvents ++ spawnEvents)
  where
    doneEvents :: [Event (IL SFObject -> IL SFObject)]
    doneEvents = [ (done obj) `tag` (deleteIL k) | (k,obj) <- assocsIL objs ]

    spawnEvents :: [Event (IL SFObject -> IL SFObject)]
    spawnEvents = [ fmap (foldl (.) id . map insertIL_) (spawn obj) | (_,obj) <- assocsIL objs ]

game :: IL SFObject -> SF (Event GameEvent, IL Object) (IL Object)
game objs = dpSwitch route objs (arr killOrSpawn >>> notYet) (\sfs f -> game (f sfs))

playGame :: IL SFObject -> SF (Event GameEvent) (IL Object)
playGame sfobjs = proc ev -> do
  rec
    objs <- game sfobjs -< (ev, objsp)
    objsp <- iPre emptyIL -< objs
  returnA -< objs

---------------------------------------------------

type Point = (Double, Double)
type Polygon = [Point]
type Edge = (Point, Point)

pos2Point :: Position -> Point
pos2Point p = (vector2X p, vector2Y p)

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

transformPoly :: AngPosition -> Point -> Polygon -> Polygon
transformPoly ang dp = (translatePoly dp).(rotatePoly ang)

scalePoly :: Double -> Polygon -> Polygon
scalePoly scaleFactor polygon = map scalePoint polygon
 where
    scalePoint p = (scaleFactor * fst p, scaleFactor * snd p)

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

polygonInPolygon :: Polygon -> Polygon -> Bool
polygonInPolygon poly1 poly2 = or $ map (flip pointInPolygon poly2) poly1

---------------------------------------------------

-- | A Canvas action to render the entire scene.
renderScene :: IL Object -> Canvas ()
renderScene a = do
    scaleScene
    fillStyle "black"
    fillRect(0.0,0.0,1.0,1.0)
    renderObjects (elemsIL a)
    return ()

-- | We scale such that (0,0) is the bottom-left of the canvas and (1,1) is the top-right.
scaleScene :: Canvas ()
scaleScene = do
    context <- myCanvasContext
    let w = width context
        h = height context
    translate (0,h)
    scale (w, negate h)

renderPolygon :: Polygon -> Canvas ()
renderPolygon [] = return ()
renderPolygon p  = do
    beginPath ()
    moveTo (head p)
    mapM_ lineTo (tail p)

renderPolygonStroke :: Polygon -> Canvas ()
renderPolygonStroke p = do
    renderPolygon p
    lineWidth 0.002
    strokeStyle "white"
    stroke()

renderPolygons :: [Polygon] -> Canvas ()
renderPolygons p = do
    mapM_ renderPolygonStroke p

renderShip :: Object -> Canvas ()
renderShip s = do
    save ()
    translate(vector2X (pos s), vector2Y (pos s))
    rotate (angPos s)
    renderPolygon (poly s)
    lineWidth 0.002
    strokeStyle "white"
    stroke()
    if (thrusting s) && not (null (poly s)) then do
        renderPolygon thrustPolygon
        stroke()
    else lineWidth 0.002
    restore ()

renderAsteroid :: Object -> Canvas ()
renderAsteroid a = do
    save ()
    translate(vector2X (pos a), vector2Y (pos a))
    rotate (angPos a)
    renderPolygon (poly a)
    lineWidth 0.002
    strokeStyle "white"
    stroke()
    restore ()

renderMissile :: Object -> Canvas ()
renderMissile m = do
    save ()
    translate(vector2X (pos m), vector2Y (pos m))
    renderPolygon (poly m)
    lineWidth 0.002
    strokeStyle "white"
    stroke()
    restore ()

renderDebris :: Object -> Canvas ()
renderDebris d = do
    save ()
    translate(vector2X (pos d), vector2Y (pos d))
    renderPolygon (poly d)
    lineWidth 0.001
    strokeStyle "white"
    stroke()
    restore ()

renderGame :: Object -> Canvas ()
renderGame g = do
    save ()
    renderPolygons (placedScorePolygons ++ placedLifePolygons)
    lineWidth 0.001
    strokeStyle "white"
    stroke()
    if gameOver g then do
       renderPolygon junkPolygon
       stroke()
    else
       lineWidth 0.002
    restore ()
  where
    placedLifePolygons :: [Polygon]
    placedLifePolygons = map translatePolyPair (zip shipPositions lifePolygons)
    placedScorePolygons :: [Polygon]
    placedScorePolygons = map translatePolyPair (zip digitPositions scorePolygons)
    translatePolyPair :: (Point, Polygon) -> Polygon
    translatePolyPair (pt,pol) = translatePoly pt pol
    lifePolygons :: [Polygon]
    lifePolygons = take (lives g) (repeat shipPolygon)
    scorePolygons :: [Polygon]
    scorePolygons = map digToPolygon (reverse $ digs (score g))
    digToPolygon :: Int -> Polygon
    digToPolygon d = digitPolygons !! d
    digs :: Int -> [Int]
    digs n = map digitToInt $ show n
    digitPositions :: [Point]
    digitPositions = zip [0.220,0.180..0.020] (take 6 $ repeat 0.950)
    shipPositions :: [Point]
    shipPositions =  zip [0.220,0.200..0.120] (take 6 $ repeat 0.890)

renderObject :: Object -> Canvas ()
renderObject obj = case obj of
    Asteroid _ _ _ _ _ _ _ _ _ _ _ -> renderAsteroid obj
    Ship     _ _ _ _ _ _ _ _ _ _   -> renderShip obj
    Missile  _ _ _ _ _             -> renderMissile obj
    Debris   _ _ _ _ _ _           -> renderDebris obj
    Game     _ _ _ _ _ _           -> renderGame obj

renderObjects :: [Object] -> Canvas ()
renderObjects = mapM_ renderObject

---------------------------------------------------

asteroid1 :: Polygon
asteroid1 = [(-0.064,-0.030),(-0.018,-0.030),(-0.032,-0.060),
                ( 0.016,-0.060),( 0.062,-0.030),( 0.064,-0.014),
                ( 0.016, 0.000),( 0.058, 0.030),( 0.032, 0.060),
                ( 0.014, 0.046),(-0.032, 0.060),(-0.064, 0.016),
                (-0.064,-0.030)]

asteroid2 :: Polygon
asteroid2 = [(-0.054,-0.026),(-0.054, 0.033),(-0.026, 0.062),
             ( 0.005, 0.035),( 0.033, 0.062),( 0.060, 0.035),
             ( 0.047, 0.005),( 0.060,-0.026),( 0.018,-0.056),
             (-0.026,-0.056),(-0.054,-0.026)]

asteroid3 :: Polygon
asteroid3 = [(-0.056, 0.029),(-0.029, 0.056),( 0.002, 0.044),
             ( 0.030, 0.057),( 0.059, 0.029),( 0.032, 0.014),
             ( 0.057,-0.015),( 0.030,-0.060),(-0.014,-0.045),
             (-0.027,-0.059),(-0.056,-0.030),(-0.044,-0.002),
             (-0.056, 0.029)]

asterPolygons :: [Polygon]
asterPolygons = [asteroid1,asteroid2,asteroid3]

shipPolygon :: Polygon
shipPolygon = [( -0.008,-0.016),(0.000,0.016),(0.008,-0.016),
                ( 0.000,-0.012),(-0.008,-0.016)]

thrustPolygon :: Polygon
thrustPolygon = [(0.000,-0.012),(0.006,-0.018),(0.000,-0.024),
                ( -0.006,-0.018),(0.000,-0.012)]

junkPolygon :: Polygon
junkPolygon = [(0.1,0.1),(0.1,0.8),(0.8,0.8),(0.8,0.1),(0.1,0.1)]

missilePolygon :: Polygon
missilePolygon = [( -0.001,-0.001),(-0.001, 0.001),(0.001, 0.001),
                  ( 0.001,-0.001)]

dustPolygon :: Polygon
dustPolygon = [( -0.001,-0.001),(-0.001, 0.001),(0.001, 0.001),
                  ( 0.001,-0.001)]

shipDebrisPolygons :: [Polygon]
shipDebrisPolygons = [[( -0.006,-0.016),(0.000,0.016)],
                      [(0.000,0.016),(0.006,-0.016)],
                      [(0.000,-0.024),( -0.004,-0.018)],
                      [( 0.000,-0.012),(-0.006,-0.016)]]

digitZeroPolygon :: Polygon
digitZeroPolygon = [(-0.015, 0.023),( 0.015, 0.023),( 0.015,-0.023),
                    (-0.015,-0.023),(-0.015, 0.023)]

digitOnePolygon :: Polygon
digitOnePolygon = [( 0.015, 0.023),( 0.015,-0.023)]

digitTwoPolygon :: Polygon
digitTwoPolygon = [(-0.015, 0.023),( 0.015, 0.023),( 0.015, 0.000),
                   (-0.015, 0.000),(-0.015,-0.023),( 0.015,-0.023)]

digitThreePolygon :: Polygon
digitThreePolygon = [(-0.015, 0.023),( 0.015, 0.023),( 0.015, 0.000),
                     (-0.015, 0.000),( 0.015, 0.000),( 0.015,-0.023),
                     (-0.015,-0.023)]

digitFourPolygon :: Polygon
digitFourPolygon = [(-0.015, 0.023),(-0.015, 0.000),( 0.015, 0.000),
                    ( 0.015,-0.023),( 0.015, 0.023)]

digitFivePolygon :: Polygon
digitFivePolygon = [( 0.015, 0.023),(-0.015, 0.023),(-0.015, 0.000),
                    ( 0.015, 0.000),( 0.015,-0.023),(-0.015,-0.023)]

digitSixPolygon :: Polygon
digitSixPolygon = [( 0.015, 0.023),(-0.015, 0.023),(-0.015, 0.000),
                   ( 0.015, 0.000),( 0.015,-0.023),(-0.015,-0.023),
                   (-0.015, 0.000)]

digitSevenPolygon :: Polygon
digitSevenPolygon = [(-0.015, 0.023),( 0.015, 0.023),( 0.015,-0.023)]

digitEightPolygon :: Polygon
digitEightPolygon = [(-0.015, 0.023),( 0.015, 0.023),( 0.015,-0.023),
                     (-0.015,-0.023),(-0.015, 0.023),(-0.015, 0.000),
                     ( 0.015, 0.000)]

digitNinePolygon :: Polygon
digitNinePolygon = [(-0.015,-0.023),( 0.015,-0.023),( 0.015, 0.023),
                    (-0.015, 0.023),(-0.015, 0.000),( 0.015, 0.000)]

digitPolygons :: [Polygon]
digitPolygons = [digitZeroPolygon, digitOnePolygon, digitTwoPolygon,
                  digitThreePolygon, digitFourPolygon, digitFivePolygon,
                  digitSixPolygon, digitSevenPolygon, digitEightPolygon,
                  digitNinePolygon]
