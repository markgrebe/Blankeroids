{-# LANGUAGE Arrows, ScopedTypeVariables, OverloadedStrings #-}

module Main where

import Graphics.Blank hiding (Event)
import qualified Graphics.Blank (Event)
import FRP.Yampa
import FRP.Yampa.Vector2
import Data.Fixed(mod')
import Data.List(nub, mapAccumL)

import FRP.Yampa.Canvas

import Control.Monad
import Control.Monad.Random

import IdentityList

import Debug.Trace
import Debug.Hood.Observe

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
                         done   :: Event (),
                         spawn  :: Event [SFObject]
                       }

type SFObject = SF (Event KeyEvent) Object

isGame :: Object -> Bool
isGame obj = case obj of
    Game _ _ _ _ -> True
    _            -> False

isShip :: Object -> Bool
isShip obj = case obj of
    Ship _ _ _ _ _ _ _ _ -> True
    _                    -> False

isMissile :: Object -> Bool
isMissile obj = case obj of
    Missile _ _ _ _ _ -> True
    _                 -> False

isAsteroid :: Object -> Bool
isAsteroid obj = case obj of
    Asteroid _ _ _ _ _ _ _ _ _ -> True
    _                          -> False

initAsterVel  :: Double
initAsterVel  = 0.1
initAsterNum  :: Int
initAsterNum  = 4
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

randGenList :: RandomGen g => g -> [g]
randGenList g = g' : randGenList g''
  where
    (g', g'') = split g

initAsteroid :: RandomGen g => Rand g Object
initAsteroid = do
    -- Get random lists of x,y coordinates
    xs <- getRandomRs (0.0, 1.0)
    ys <- getRandomRs (0.0, 1.0)
    -- Find an x,y pair that is not in center of the screen
    let (x,y) = head . dropWhile inCenter . zip xs $ ys
    -- Get a random angle for velocity
    thetaRand <- getRandomR (0.0, pi / 2)
    -- Get a random angle for position
    angRand <- getRandomR (0.0, 2 * pi)
    -- Get a random angular velocity
    angVelRand <- getRandomR (-0.5,0.5)
    -- Get a random asteroid
    asterIndex <- getRandomR (0,length(asterPolygons) - 1)
    -- Based on quadrant of the screen of x,y, set velocity in the general
    -- direction of the center
    let thetav = if x <= 0.5 && y <= 0.5 then thetaRand
            else if x <= 0.5 && y >  0.5 then thetaRand + 3 * pi / 2
            else if x >  0.5 && y >  0.5 then thetaRand + pi
            else                              thetaRand + pi / 2
    -- Magnitude of the velocity should be initAsterVel
    let xv = initAsterVel * cos thetav
    let yv = initAsterVel * sin thetav
    return Asteroid {   pos    = vector2 x y,
                        vel    = vector2 xv yv,
                        angPos = angRand,
                        angVel = angVelRand,
                        gen    = 0,
                        radius = asterGen1Rad,
                        done   = NoEvent,
                        spawn  = Event [],
                        poly   = asterPolygons !! asterIndex
                    }
  where
    inCenter :: (Double, Double) -> Bool
    inCenter (x, y) = x >= centerMinEdge && x <= centerMaxEdge &&
                      y >= centerMinEdge && y <= centerMaxEdge

newAsteroids :: RandomGen g => Position -> Int -> Rand g [Object]
newAsteroids p currgen = do
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
                       gen = currgen + 1, radius = sizeScale * asterGen1Rad,
                       done = NoEvent, spawn = Event [],
                       poly = scalePoly sizeScale (asterPolygons !! asterIndex)}

initAsteroids :: RandomGen g => Rand g [Object]
initAsteroids = replicateM initAsterNum initAsteroid

genInitialAsteroids :: RandomGen g => g -> [Object]
genInitialAsteroids g = evalRand initAsteroids g

theShip :: Object
theShip = Ship {poly = shipPolygon,
                pos = vector2 0.5 0.5, vel = vector2 0.0 0.0,
                angPos = 0.0, radius = 0.016, thrusting = False,
                done = NoEvent, spawn = NoEvent }

theGame :: Object
theGame = Game {score = 0, lives = 3,
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

data KeyEvent = TurnRight | TurnLeft | Thruster | Fire | Destroyed | Spawn
    deriving (Show, Eq)

destroyedToUnit :: Event KeyEvent -> Event ()
destroyedToUnit (Event Destroyed) = Event ()
destroyedToUnit _                 = NoEvent

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

handleKeyEvents :: Graphics.Blank.Event -> Canvas (Event KeyEvent)
handleKeyEvents blankEvent = do
    let keyEvent = case (eWhich blankEvent) of
                    Just a | a == rightKey -> Event TurnRight
                           | a == leftKey  -> Event TurnLeft
                           | a == upKey    -> Event Thruster
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
    returnA -< a { pos = pos', angPos = angPos',
                   done = destroyedToUnit ev,
                   spawn = destroyedToUnit ev `tag` newObjects a pos' }
  where
    (g1, g2) = split g
    (g3, g4) = split g1

    newObjects :: Object -> Position -> [SFObject]
    newObjects a' p = evalRand (newDebris p) g2 ++
                     if gen a' <= 1
                     then movingRandomAsteroids g3 (evalRand (newAsteroids p (gen a')) g4)
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
    fire <- arr fireCannon -< ev
    rec
        accel <- arr calcAccel -< (ev, angPos', vel')
        vel' <- integral -< accel
    pos' <- wrapObject (radius s) <<< ((pos s) ^+^) ^<< integral -< vel'
    returnA -< s { pos = pos', vel = vel', angPos = angPos',
                   thrusting = ((vector2Rho accel) > 0.1),
                   done = destroyedToUnit ev,
                   spawn = (mergeBy (++)) (fire `tag` (addMissile pos' angPos'))
                           (destroyedToUnit ev `tag` (evalRand (newDebris pos') g)) }
  where
    addMissile :: Position -> AngPosition -> [SFObject]
    addMissile p' ap' = [movingMissile $ launchMissile p' ap']

    launchMissile :: Position -> AngPosition -> Object
    launchMissile p' ap' = Missile { poly = missilePolygon,
                                     pos = p', --ToDo caculate ship tip pos
                                     vel = vector2 (-missileVel * sin ap')
                                                   ( missileVel * cos ap'),
                                     done = NoEvent, spawn  = NoEvent }

    accumAngPos :: Double -> KeyEvent -> Double
    accumAngPos start TurnRight = start - (pi / 8)
    accumAngPos start TurnLeft  = start + (pi / 8)
    accumAngPos start _         = start

    fireCannon :: Event KeyEvent -> Event ()
    fireCannon (Event Fire) = Event ()
    fireCannon _            = NoEvent

    calcAccel :: (Event KeyEvent, AngPosition, Velocity) -> Acceleration
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

playingGame :: RandomGen g => g -> Object -> SFObject
playingGame g gm = proc ev -> do
--    score' <-    -< ev
--    lives' <-    -< ev
    returnA -< gm { score = score', lives = lives' }
  where
    score' = 0
    lives' = 0

-- | Construct a list of moving game objects from a list of initial configurations.
movingObjects :: RandomGen g => g -> [Object] -> Object -> Object -> SF (Event KeyEvent) (IL Object)
movingObjects g as ship gm = playGame (listToIL ([shipSF] ++ aSFs))
  where
    (g1, g2) = split g
    (g3, g4) = split g1
    aSFs = movingRandomAsteroids g2 as
    shipSF = movingShip g3 ship
    gameSF = playingGame g4 gm

route :: (Event KeyEvent, IL Object) -> IL sf -> IL (Event KeyEvent, sf)
route (keyEv,objs) sfs = mapIL route' sfs
  where
    ships = assocsIL $ filterIL (\(_, obj) -> isShip obj) objs
    shipKeys = map fst ships
    asteroids = assocsIL $ filterIL (\(_, obj) -> isAsteroid obj) objs
    missiles = assocsIL $ filterIL (\(_, obj) -> isMissile obj) objs
    game' = head $ assocsIL $ filterIL (\(_, obj) -> isGame obj) objs
    sAsHits :: [(ILKey, Object)] -> [(ILKey, Object)] -> [ILKey]
    sAsHits ((sk, so):[]) ((_,ao):rest) =
      if polygonInPolygon sPolygon aPolygon
      then [sk]
      else sAsHits [(sk, so)] rest
        where
          sPolygon = transformPoly (angPos so) (pos2Point (pos so)) (poly so)
          aPolygon = transformPoly (angPos ao) (pos2Point (pos ao)) (poly ao)
    sAsHits _        _              = []
    mAsHits :: (ILKey, Object) -> [(ILKey, Object)] -> [ILKey]
    mAsHits _        []             = []
    mAsHits (mk, mo) ((ak,ao):rest) =
      if pointInPolygon (pos2Point (pos mo)) polygon'
      then mk : ak : (mAsHits (mk, mo) rest)
      else mAsHits (mk, mo) rest
        where
          polygon' = transformPoly (angPos ao) (pos2Point (pos ao)) (poly ao)
    msAsHits :: [(ILKey, Object)] -> [(ILKey, Object)] -> [ILKey]
    msAsHits []     _  = []
    msAsHits (m:ms) as = (mAsHits m as) ++ (msAsHits ms as)
    hits = nub $ (msAsHits missiles asteroids ++ sAsHits ships asteroids)
    route' :: (ILKey, sf) -> (Event KeyEvent, sf)
    route' (k,sfObj) = if k `elem` hits
                       then (Event Destroyed, sfObj)
                       else (keyEv, sfObj)

killOrSpawn :: ((Event KeyEvent, IL Object) , IL Object) -> Event (IL SFObject -> IL SFObject)
killOrSpawn (_, objs) = foldl (mergeBy (.)) noEvent (doneEvents ++ spawnEvents)
  where
    doneEvents :: [Event (IL SFObject -> IL SFObject)]
    doneEvents = [ (done obj) `tag` (deleteIL k) | (k,obj) <- assocsIL objs ]

    spawnEvents :: [Event (IL SFObject -> IL SFObject)]
    spawnEvents = [ fmap (foldl (.) id . map insertIL_) (spawn obj) | (_,obj) <- assocsIL objs ]

game :: IL SFObject -> SF (Event KeyEvent, IL Object) (IL Object)
game objs = dpSwitch route objs (arr killOrSpawn >>> notYet) (\sfs f -> game (f sfs))

playGame :: IL SFObject -> SF (Event KeyEvent) (IL Object)
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
pointInPolygon point polygon = foldr acumNode False edges
  where
    px = fst point
    py = snd point
    edges :: [Edge]
    edges = polyEdges $ tail polygon
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
renderPolygon p = do
    beginPath ()
    moveTo (head p)
    mapM_ lineTo (tail p)
    closePath ()

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
shipPolygon = [( -0.006,-0.016),(0.000,0.016),(0.006,-0.016),
                ( 0.000,-0.012),(-0.006,-0.016)]

thrustPolygon :: Polygon
thrustPolygon = [(0.000,-0.012),(0.004,-0.018),(0.000,-0.024),
                ( -0.004,-0.018),(0.000,-0.012)]

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

renderShip :: Object -> Canvas ()
renderShip s = do
    save ()
    translate(vector2X (pos s), vector2Y (pos s))
    rotate (angPos s)
    renderPolygon (poly s)
    lineWidth 0.002
    strokeStyle "white"
    stroke()
    if (thrusting s) then do
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
    restore ()

renderObject :: Object -> Canvas ()
renderObject obj = case obj of
    Asteroid _ _ _ _ _ _ _ _ _ -> renderAsteroid obj
    Ship _ _ _ _ _ _ _ _       -> renderShip obj
    Missile _ _ _ _ _          -> renderMissile obj
    Debris _ _ _ _ _ _         -> renderDebris obj
    Game _ _ _ _               -> renderGame obj

renderObjects :: [Object] -> Canvas ()
renderObjects = mapM_ renderObject
