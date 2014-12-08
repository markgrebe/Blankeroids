{-# LANGUAGE Arrows, ScopedTypeVariables, OverloadedStrings #-}

module Blankeroids.Asteroids where

import FRP.Yampa
import FRP.Yampa.Vector2

import Control.Monad
import Control.Monad.Random

import Blankeroids.Polygons
import Blankeroids.Types
import Blankeroids.Utils
import Blankeroids.Debris

initAsterVel  :: Double
initAsterVel  = 0.1
initAsterNum  :: Int
initAsterNum  = 4
newAsterNum   :: Int
newAsterNum   = 6
asterGen1Rad  :: Double
asterGen1Rad  = 0.064
asteroidValues :: [Int]
asteroidValues = [20, 50, 100]

-- Generate initial asteroids for a round at the edges of the screen.
-- The integer parameter specifies the round number of the asteroid.
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
                        basePoly = asterPolygons !! asterIndex,
                        poly   = asterPolygons !! asterIndex
                    }

-- Create smaller asteroids at the position of an asteroid that
-- was destroy.  The parameters specify asteroid position, current generation,
-- and current game round.
newAsteroids :: RandomGen g => Position -> Int -> Int -> Rand g [Object]
newAsteroids p currgen round' = do
    asters <- replicateM 2 oneAsteroid
    return asters
  where
    -- Each generation is half the size of the previous
    sizeScale :: Double
    sizeScale = 1.0 / fromIntegral ((currgen + 1) * 2)
    -- The velocity of the asteroid increases with each generation.
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
                basePoly = scalePoly sizeScale (asterPolygons !! asterIndex),
                poly = scalePoly sizeScale (asterPolygons !! asterIndex)}

-- Generate the asteroids for a round, specifying the count and round number.
initAsteroids :: RandomGen g => Int -> Int -> Rand g [Object]
initAsteroids count round' = replicateM count (initAsteroid round')

-- Generate the asteroids for the first game round.
genInitialAsteroids :: RandomGen g => g -> [Object]
genInitialAsteroids g = evalRand (initAsteroids initAsterNum 1) g

-- Create the Asteroid signal functions, given the Asteroid data structures
-- and an initial random generator.  The generator is split multiple times,
-- so each signal function gets a unique generator.
movingRandomAsteroids :: RandomGen g => g -> [Object] -> [SFObject]
movingRandomAsteroids g as = map randomAster gensAsters
  where
    gens = take (length as) (randGenList g)
    gensAsters = zip gens as
    randomAster (g', a) = movingAsteroid g' a

-- Asteroid signal function
movingAsteroid :: RandomGen g => g -> Object -> SFObject
movingAsteroid g a = proc ev -> do
    -- Calculate the xy position, based on integral of velocity, wrappnig
    -- at the edge of the screen as required.
    pos' <- wrapObject (radius a) <<< ((pos a) ^+^) ^<< integral -< (vel a)
    -- Calculate the angular position as the integral of angular velocity
    angPos' <- ((angPos a) +) ^<< integral -< (angVel a)
    -- Calculate a boolean flag that indicates if an asteroid has been destroyed
    -- based on incoming destroyed events.
    destroyed' <- accumHoldBy destroyedOccured False -< ev
    -- Calculate a flag which is used to request a reanimation event for the
    -- next round of asteroids.  This occurs if this asteroid is the last in
    -- the round to be destroyed, and occurs 5 seconds after the destruction.
    reqReanimate' <- accumHoldBy destroyedOccured False <<< delayEvent 5.0 -< ev
    returnA -< a { poly = if (destroyed') then []
                          else transformPoly angPos'
                                    (pos2Point pos') (basePoly a),
                   pos = pos', angPos = angPos',
                   reqReanimate = reqReanimate',
                   -- Asteroid SF shold be removed from list if it received
                   -- a Destroyed or Reanimate Event.
                   done = merge (reanimateToUnit ev) (destroyedToUnit ev),
                   -- New SF's should be created for the smaller asteoids and
                   -- debris or for the next round.
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

    -- Create the next round of Asteroid signal functions
    reanimateAsteroids :: [SFObject]
    reanimateAsteroids =
        movingRandomAsteroids g3
            (evalRand (initAsteroids numAsteroids gameRound') g4)
      where
        gameRound' = gameRound a
        numAsteroids = initAsterNum + 2 * gameRound'

    -- Create the new, smaller asteroids from a destroyed one.
    newObjects :: Object -> Position -> [SFObject]
    newObjects a' p =
        evalRand (newDebris p) g2 ++
        if gen a' <= 1
        then movingRandomAsteroids g3
                (evalRand (newAsteroids p (gen a') (gameRound a')) g4)
        else []

    -- Create the debris from an asteroid destruction
    newDebris :: RandomGen g => Position -> Rand g [SFObject]
    newDebris p = do
        debris <- replicateM 16 (oneDebris p)
        return debris

    -- Create one piece of debris, at a random life span, velocity and angle.
    oneDebris :: RandomGen g => Position -> Rand g SFObject
    oneDebris p = do
        life'  <- getRandomR(0.1,0.8)
        vel'   <- getRandomR(0.05,0.15)
        angle' <- getRandomR(0.0, 2*pi)
        return (movingDebris
                Debris { basePoly = dustPolygon, poly = dustPolygon, pos = p,
                         vel = vector2 (vel' * sin angle') (vel' * cos angle'),
                         life = life', done = NoEvent, spawn = NoEvent } )

---------------------------------------------------

-- Polygons for the Asteroids.  There are 3 different asteroid shapes which
-- are randomly chosen.
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

