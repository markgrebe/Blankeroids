{-# LANGUAGE Arrows, ScopedTypeVariables, OverloadedStrings #-}

module Blankeroids.Saucer where

import FRP.Yampa
import FRP.Yampa.Vector2

import Control.Monad
import Control.Monad.Random

import Blankeroids.Polygons
import Blankeroids.Types
import Blankeroids.Utils
import Blankeroids.Debris
import Blankeroids.Missiles

saucerVel  :: Double
saucerVel  = 0.1
saucerRad  :: Double
saucerRad  = 0.035
saucerValues :: [Int]
saucerValues = [1000,200]
saucerCourseChangeInterval :: Double
saucerCourseChangeInterval = 2.0
saucerFireInterval :: Double
saucerFireInterval = 1.0
interSaucerInterval :: Double
interSaucerInterval = 10.0

movingSaucer :: RandomGen g => g -> Object -> SFObject
movingSaucer g s = proc ev -> do
    -- Saucer occasionally changes course by 45 deg.
    changeCourse <- occasionally g saucerCourseChangeInterval () -< ()
    (vel', course') <-
            accumHoldBy courseAdjust (vel s, course s) -< changeCourse
    -- Saucer occasionally fires it's missiles.
    fire <- occasionally g saucerFireInterval () -< ()
    fireAng' <- accumHoldBy (\fa _ -> fa + 1) (fireAng s) -< fire
    -- Calculate the xy position, based on integral of velocity, wrappnig
    -- at the edge of the screen as required.
    pos' <- wrapObjectY (radius s) <<< ((pos s) ^+^) ^<< integral -< vel'
    -- See if saucer has moved off of sides of screen
    offEdge' <- offEdge -< pos'
    -- Calculate boolean flags for destroyed and reanimate,
    -- based on input events and delays
    let doneEvent = mergeBy (\_ _ -> ()) (destroyedToUnit ev) offEdge'
    destroyed' <- accumHoldBy (\_ _ -> True) False -< doneEvent
    reanimate' <- delayEvent interSaucerInterval -< doneEvent
    -- Track Ship position so small saucer can fire at it
    shipPos <- hold (vector2 0.5 0.5) -< shipPositionToPos ev
    let dAng = vector2Theta (shipPos ^-^ pos') - pi/2
        missileAng = if (kind s) == 0
                     then dAng + ((fireAngs s) !! fireAng')
                     else ((fireAngs s) !! fireAng')
    returnA -< s { polys = if (destroyed') then []
                           else translatePolys (pos2Point pos') (basePolys s),
                   pos = pos', vel = vel', course = course', fireAng = fireAng',
                   -- Saucer should be removed if it is time for a new one
                   done = reanimate',
                   -- New SF's should be created for fire missile, saucer
                   -- debris, or a new saucer.
                   spawn = foldl (mergeBy (++)) NoEvent
                        [(gate fire (not destroyed') `tag`
                            (addMissile pos' missileAng)),
                         (destroyedToUnit ev `tag`
                            (evalRand (newDebris pos') g')),
                         (reanimate' `tag` (newSaucer g'' ((gen s) + 1)))] }
  where
    (g', g'') = split g

    shipPositionToPos :: Event GameEvent -> Event Position
    shipPositionToPos (Event (ShipPosition p)) = Event p
    shipPositionToPos _                        = NoEvent

    courseAdjust :: (Velocity, Int) -> () -> (Velocity, Int)
    courseAdjust (_, c1) _ = (newVelocity, nextIndex)
      where
        nextIndex = c1 + 1
        newCourse = (courses s) !! nextIndex
        newVelocity = vector2 ((velMag s) * sin newCourse)
                              ((velMag s) * cos newCourse)

    -- Signal function which creates an event when saucer goes off side.
    offEdge :: SF Position (Event ())
    offEdge = proc p -> do
        returnA -< off (vector2X p)
      where
        off x | x < 0.0 - saucerRad = Event ()
        off x | x > 1.0 + saucerRad = Event ()
        off _ | otherwise = NoEvent

    addMissile :: Position -> AngPosition -> [SFObject]
    addMissile p' ap' = [movingMissile $ newMissile p' ap' ]

    newMissile :: Position -> AngPosition -> Object
    newMissile p' ap' = Missile { poly = missilePolygon,
                            source = SaucerMissile,
                            pos = vector2 ((vector2X p') - 0.016 * sin ap')
                                          ((vector2Y p') + 0.016 * cos ap'),
                            vel = vector2 (-missileVel * sin ap')
                                          ( missileVel * cos ap'),
                            done = NoEvent, spawn  = NoEvent }

    -- Create the debris from an saucer destruction
    newDebris :: RandomGen g => Position -> Rand g [SFObject]
    newDebris p = do
        debris <- replicateM 32 (oneDebris p)
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

-- Generate saucer for a round at the edges of the screen.
newSaucer :: RandomGen g => g -> Int -> [SFObject]
newSaucer g newGen = [movingSaucer g' (evalRand initSaucer g'')]
  where
    (g', g'') = split g

    initSaucer :: RandomGen g => Rand g Object
    initSaucer = do
        -- Get an initial position on the edge of the screen.
        initPos <- getRandomR (0.0, 2.0)
        -- Choose a random course for the saucer
        courseIndex <- getRandomR (0, (length saucerCoursesLeft) - 1)
        let (x,y,courses') = if initPos < 1.0
                        then (0.0, initPos, saucerCoursesLeft !! courseIndex)
                        else (1.0, initPos, saucerCoursesRight !! courseIndex)
        -- Get the type of saucer. The first 3 are large, then the fourth is
        -- small, and after that it is random.
        randKind <- getRandomR (0,1)
        let kind' = if newGen < 3
                    then 1
                    else if newGen == 3
                         then 0
                         else randKind
        -- The missiles are fired either totally randomly (in the case of a
        -- large saucer), or in a error range around the vector from the
        -- saucer to the ship (in the case of the small saucer).  The error
        -- range gets smaller with successive small saucer generations.
        let angError = 0.4 / (1.0 + 0.3 * fromIntegral (newGen - 3))
        let angRange1 = if kind' == 1 then 0    else -angError
        let angRange2 = if kind' == 1 then 2*pi else  angError
        fireAngs' <- getRandomRs (angRange1, angRange2)
        -- Small saucer is faster than large one also.
        let velMag' = if kind' == 1 then saucerVel else saucerVel * 1.5
        let v = if x == 0.0 then vector2 velMag'    0.0
                else             vector2 (-velMag') 0.0
        let polys' = if kind' == 0 then smallSaucerPolygons
                                   else saucerPolygons
        return Saucer { pos    = vector2 x y,
                        vel    = v,
                        velMag = velMag',
                        radius = saucerRad,
                        course = 0,
                        courses = courses',
                        fireAng = 0,
                        fireAngs = fireAngs',
                        gen    = newGen,
                        kind   = kind',
                        done   = NoEvent,
                        spawn  = NoEvent,
                        basePolys = polys',
                        polys  = polys'
                        }

---------------------------------------------------

saucerPolygon1 :: Polygon
saucerPolygon1 = [( 0.008, 0.038),(-0.008, 0.038),(-0.014, 0.021),
                  ( 0.015, 0.021),( 0.008, 0.038)]

saucerPolygon2 :: Polygon
saucerPolygon2 = [( 0.015, 0.021),(-0.014, 0.021),(-0.035, 0.008),
                  ( 0.035, 0.008),( 0.015, 0.021)]

saucerPolygon3 :: Polygon
saucerPolygon3 = [( 0.035, 0.008),(-0.035, 0.008),(-0.014,-0.008),
                  ( 0.017,-0.008),( 0.035, 0.008)]

saucerPolygons :: [Polygon]
saucerPolygons = [saucerPolygon1, saucerPolygon2, saucerPolygon3]

smallSaucerPolygons :: [Polygon]
smallSaucerPolygons = map (scalePoly 0.4) saucerPolygons

saucerCoursesLeft :: [[AngPosition]]
saucerCoursesLeft = [cycle [pi / 2, pi / 4, pi / 2, 3 * pi / 4],
                     cycle [pi / 2, pi / 4, pi / 2, pi / 4],
                     cycle [pi / 2, 3 * pi / 4, pi / 2, pi / 4],
                     cycle [pi / 2, 3 * pi / 4, pi / 2, 3 * pi / 4]
                    ]

saucerCoursesRight :: [[AngPosition]]
saucerCoursesRight = [cycle [3 * pi / 2, 5 * pi / 4, 3 *pi / 2, 7 * pi / 4],
                      cycle [3 * pi / 2, 5 * pi / 4, 3 * pi / 2, 5 * pi / 4],
                      cycle [3 * pi / 2, 7 * pi / 4, 3 * pi / 2, 5 * pi / 4],
                      cycle [3 * pi / 2, 7 * pi / 4, 3 * pi / 2, 7 * pi / 4]
                     ]
