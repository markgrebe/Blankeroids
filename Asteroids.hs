{-# LANGUAGE Arrows, ScopedTypeVariables, OverloadedStrings #-}

module Main where

import Graphics.Blank hiding (Event)
import qualified Graphics.Blank (Event)
import FRP.Yampa
import FRP.Yampa.Vector2
import Data.Fixed(mod')
import Data.Text(Text)

import FRP.Yampa.Canvas

import System.Random (newStdGen)
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

data Object = Asteroid { pos    :: Position,
                         vel    :: Velocity,
                         angPos :: AngPosition,
                         angVel :: AngVelocity,
                         radius :: Double,
                         gen    :: Int,
                         done   :: Event ()
                       }
            | Ship     { pos    :: Position,
                         vel    :: Velocity,
                         angPos :: AngPosition,
                         radius :: Double,
                         thrusting :: Bool,
                         fire   :: Event (),
                         done   :: Event ()
                       }
            | Missile  { pos    :: Position,
                         vel    :: Velocity,
                         done   :: Event ()
                       }
    deriving (Show, Eq)

type SFObject = SF (Event KeyEvent) Object

isShip :: Object -> Bool
isShip obj = case obj of
    Ship _ _ _ _ _ _ _ -> True
    _                  -> False

isMissile :: Object -> Bool
isMissile obj = case obj of
    Missile _ _ _  -> True
    _              -> False

initAsterVel  = 0.1
initAsterNum  = 4
centerMinEdge = 0.4
centerMaxEdge = 0.6
asterGen1Rad  = 0.064
shipThrust    = 10.0
shipDrag      = 3.0
missileVel    = 0.5
missileLife   = 1.0

initAsteroid :: RandomGen g => Rand g Object
initAsteroid = do
    -- Get random lists of x,y coordinates
    xs <- getRandomRs (0.0, 1.0)
    ys <- getRandomRs (0.0, 1.0)
    -- Find an x,y pair that is not in center of the screen
    let (x,y) = head . dropWhile inCenter . zip xs $ ys
    -- Get a random angle
    thetaRand <- getRandomR (0.0, pi / 2)
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
                        angPos = 0.0,
                        angVel = -0.5,
                        gen    = 0,
                        radius = asterGen1Rad,
                        done   = NoEvent
                    }
  where
    inCenter :: (Double, Double) -> Bool
    inCenter (x, y) = x >= centerMinEdge && x <= centerMaxEdge &&
                      y >= centerMinEdge && y <= centerMaxEdge

initAsteroids :: RandomGen g => Rand g [Object]
initAsteroids = replicateM initAsterNum initAsteroid

genInitialAsteroids :: RandomGen g => g -> [Object]
genInitialAsteroids g = evalRand initAsteroids g

theShip = Ship {pos = vector2 0.5 0.5, vel = vector2 0.0 0.0,
                      angPos = 0.0, radius = 0.016, thrusting = False,
                      fire = NoEvent, done = NoEvent }

launchMissile :: Object -> Object
launchMissile s = Missile { pos = (pos s), --ToDo caculate ship tip pos
                            vel = vector2 (-missileVel * sin (angPos s))
                                          ( missileVel * cos (angPos s)),
                            done = NoEvent }

---------------------------------------------------
main :: IO ()
main = do
    g <- newStdGen
    blankCanvas 3000 { events = ["keydown"] } (animateAsteriods g)

-- | Display an animation of multiple asteroids.
animateAsteriods :: RandomGen g => g -> DeviceContext -> IO ()
animateAsteriods g = reactimateSFinContext handleKeyEvents renderScene (movingObjects (genInitialAsteroids g) theShip)

---------------------------------------------------

data KeyEvent = TurnRight | TurnLeft | Thruster | Fire
    deriving Show

rightKey = 39
leftKey  = 37
upKey    = 38
downKey  = 40
spaceKey = 32

handleKeyEvents :: Graphics.Blank.Event -> Canvas (Event KeyEvent)
handleKeyEvents event = do
    let keyEvent = case (eWhich event) of
                    Just a | a == rightKey -> Event TurnRight
                           | a == leftKey  -> Event TurnLeft
                           | a == upKey    -> Event Thruster
                           | a == spaceKey -> Event Fire
                           | otherwise     -> NoEvent
                    Nothing                -> NoEvent
    return keyEvent

wrapObject :: Double -> SF Position Position
wrapObject radius = proc pos -> do
    let minCoord = -radius
        maxCoord = 1.0 + radius
        wrap a = a `mod'` (1.0 + 2.0 * radius)
        (x,y) = vector2XY pos
        pos' = vector2 (if x <= minCoord || x >= maxCoord
                        then wrap x else x)
                       (if y <= minCoord || y >= maxCoord
                        then wrap y else y)
    returnA -< pos'

movingAsteroid :: Object -> SFObject
movingAsteroid a = proc _ -> do
    p <- wrapObject (radius a) <<< ((pos a) ^+^) ^<< integral -< (vel a)
    ap <- ((angPos a) +) ^<< integral -< (angVel a)
    returnA -< updateAsteroidPos p ap
  where
    updateAsteroidPos :: Position -> AngPosition -> Object
    updateAsteroidPos p' ap' = a { pos = p', angPos = ap'}

movingMissile :: Object -> SFObject
movingMissile m = proc _ -> do
    p <- ((pos m) ^+^) ^<< integral -< (vel m)
    done <- after missileLife () -< ()
    returnA -< m { pos = p, done = done }

movingShip :: Object -> SFObject
movingShip a = proc ev -> do
    ap <- accumHoldBy accumAngPos 0.0 -< ev
    fire <- arr fireCannon -< ev
    rec
        a <- arr calcAccel -< (ev, ap, v)
        v <- integral -< a
    p <- wrapObject (radius a) <<< ((pos a) ^+^) ^<< integral -< v
    returnA -< updateShip p v ap ((vector2Rho a) > 0.1) fire
  where
    updateShip :: Position -> Velocity -> AngPosition -> Bool -> Event () -> Object
    updateShip p' v' ap' t' f' = a { pos = p', vel = v',
                                     angPos = ap', thrusting = t',
                                     fire = f' }

    accumAngPos :: Double -> KeyEvent -> Double
    accumAngPos start TurnRight = start - (pi / 8)
    accumAngPos start TurnLeft  = start + (pi / 8)
    accumAngPos start _         = start

    fireCannon :: Event KeyEvent -> Event ()
    fireCannon (Event Fire) = Event ()
    fireCannon _            = NoEvent

    calcAccel :: (Event KeyEvent, AngPosition, Velocity) -> Acceleration
    calcAccel (Event Thruster, angPos, _) =
        vector2 (-shipThrust * sin angPos) (shipThrust * cos angPos)
    calcAccel (_             , _     , vel) =
        let mag = vector2Rho vel
            (x,y) = vector2XY vel
        in if (mag < shipDrag)
            then vector2 (-x) (-y)
            else vector2 (-shipDrag * x / mag) (-shipDrag * y / mag)

-- | Construct a list of moving game objects from a list of initial configurations.
movingObjects :: [Object] -> Object -> SF (Event KeyEvent) (IL Object)
movingObjects as ship = gameCore (listToIL ([shipSF] ++ aSFs))
  where
    aSFs = map movingAsteroid as
    shipSF = movingShip ship

killOrSpawn :: (Event KeyEvent, IL Object) -> Event (IL SFObject -> IL SFObject)
killOrSpawn (_, objs) = foldl (mergeBy (.)) noEvent ([fireEvent] ++ doneEvents)
  where
    shipObj :: Object
    shipObj = filter isShip (elemsIL objs) !! 0

    addMissile :: IL SFObject -> IL SFObject
    addMissile objs = insertIL_ (movingMissile $ launchMissile shipObj) objs

    fireEvent :: Event (IL SFObject -> IL SFObject)
    fireEvent = (fire shipObj) `tag` addMissile

    doneEvents :: [Event (IL SFObject -> IL SFObject)]
    doneEvents = [ (done obj) `tag` (deleteIL k) | (k,obj) <- assocsIL objs ]

gameCore :: IL SFObject -> SF (Event KeyEvent) (IL Object)
gameCore objs = dpSwitchB objs (arr killOrSpawn >>> notYet) (\sfs f -> gameCore (f sfs))

---------------------------------------------------

-- | A Canvas action to render the entire scene.
renderScene :: IL Object -> Canvas ()
renderScene a = do
    scaleScene
    fillStyle "black"
    fillRect(0.0,0.0,1.0,1.0)
    renderObjects (elemsIL a)
    -- renderShip theShip
    return ()

-- | We scale such that (0,0) is the bottom-left of the canvas and (1,1) is the top-right.
scaleScene :: Canvas ()
scaleScene = do
    context <- myCanvasContext
    let w = width context
        h = height context
    translate (0,h)
    scale (w, negate h)

type Polygon = [(Double, Double)]

renderPolygon :: Polygon -> Canvas ()
renderPolygon p = do
    beginPath ()
    moveTo (head p)
    mapM_ lineTo (tail p)
    closePath ()

asteroidGen1 :: Polygon
asteroidGen1 = [(-0.064,-0.030),(-0.018,-0.030),(-0.032,-0.060),
                ( 0.016,-0.060),( 0.062,-0.030),( 0.064,-0.014),
                ( 0.016, 0.000),( 0.058, 0.030),( 0.032, 0.060),
                ( 0.014, 0.046),(-0.032, 0.060),(-0.064, 0.016),
                (-0.064,-0.030)]

asteroidGens :: [Polygon]
asteroidGens = [asteroidGen1]

shipPolygon :: Polygon
shipPolygon = [( -0.006,-0.016),(0.000,0.016),(0.006,-0.016),
                ( 0.000,-0.012),(-0.006,-0.016)]

thrustPolygon :: Polygon
thrustPolygon = [(0.000,-0.012),(0.004,-0.018),(0.000,-0.024),
                ( -0.004,-0.018),(0.000,-0.012)]

missilePolygon :: Polygon
missilePolygon = [( -0.001,-0.001),(-0.001, 0.001),(0.001, 0.001),
                  ( 0.001,-0.001)]

renderShip :: Object -> Canvas ()
renderShip s = do
    save ()
    translate(vector2X (pos s), vector2Y (pos s))
    rotate (angPos s)
    renderPolygon shipPolygon
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
    renderPolygon (asteroidGens !! (gen a))
    lineWidth 0.002
    strokeStyle "white"
    stroke()
    restore ()

renderMissile :: Object -> Canvas ()
renderMissile a = do
    save ()
    translate(vector2X (pos a), vector2Y (pos a))
    renderPolygon missilePolygon
    lineWidth 0.002
    strokeStyle "white"
    stroke()
    restore ()

renderObject :: Object -> Canvas ()
renderObject obj = case obj of
    Asteroid _ _ _ _ _ _ _ -> renderAsteroid obj
    Ship _ _ _ _ _ _ _     -> renderShip obj
    Missile _ _ _          -> renderMissile obj

renderObjects :: [Object] -> Canvas ()
renderObjects = mapM_ renderObject
