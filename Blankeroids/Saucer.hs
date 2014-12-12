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
saucerValue :: Int
saucerValue = 200

movingSaucer :: RandomGen g => g -> Object -> SFObject
movingSaucer g s = proc ev -> do
    -- Saucer occasionally changes course by 45 deg.
    changeCourse <- occasionally g 2.0 () -< ()
    (vel', course') <-
            accumHoldBy courseAdjust (vel s, course s) -< changeCourse
    -- Saucer occasionally fires it's missiles.
    fire <- occasionally g 1.0 () -< ()
    fireAng' <- accumHoldBy (\fa _ -> fa + 1) (fireAng s) -< fire
    -- Calculate the xy position, based on integral of velocity, wrappnig
    -- at the edge of the screen as required.
    pos' <- wrapObjectY (radius s) <<< ((pos s) ^+^) ^<< integral -< vel'
    -- See if saucer has moved off of sides of screen
    offEdge' <- offEdge -< pos'
    -- Calculate boolean flags for destroyed and requestreanmiation,
    -- based on input events and delays
    let doneEvent = mergeBy (\_ _ -> ()) (destroyedToUnit ev) offEdge'
    destroyed' <- accumHoldBy destroyOccured False -< doneEvent
    reanimate' <- delayEvent 10.0 -< doneEvent
    returnA -< s { polys = if (destroyed') then []
                           else translatePolys (pos2Point pos') saucerPolygons,
                   pos = pos', vel = vel', course = course', fireAng = fireAng',
                   -- Saucer should be removed if it is time for a new one
                   done = reanimate',
                   -- New SF's should be created for fire missile, saucer
                   -- debris, or a new saucer.
                   spawn = foldl (mergeBy (++)) NoEvent
                        [(gate fire (not destroyed') `tag`
                            (addMissile pos' ((fireAngs s) !! fireAng'))),
                         (destroyedToUnit ev `tag`
                            (evalRand (newDebris pos') g')),
                         (reanimate' `tag` (newSaucer g''))] }
  where
    (g', g'') = split g

    courseAdjust :: (Velocity, Int) -> () -> (Velocity, Int)
    courseAdjust (_, c1) _ = (newVelocity, nextIndex)
      where
        nextIndex = c1 + 1
        newCourse = (courses s) !! nextIndex
        newVelocity = vector2 (saucerVel * sin newCourse)
                              (saucerVel * cos newCourse)

    offEdge :: SF Position (Event ())
    offEdge = proc p -> do
        returnA -< off (vector2X p)
      where
        off x | x < 0.0 - saucerRad = Event ()
        off x | x > 1.0 + saucerRad = Event ()
        off _ | otherwise = NoEvent

    destroyOccured :: Bool -> () -> Bool
    destroyOccured _ _ = True

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
newSaucer :: RandomGen g => g -> [SFObject]
newSaucer g = [movingSaucer g' (evalRand initSaucer g'')]
  where
    (g', g'') = split g

    initSaucer :: RandomGen g => Rand g Object
    initSaucer = do
        -- Get an initial position on the edge of the screen.
        initPos <- getRandomR (0.0, 2.0)
        fireAngs' <- getRandomRs (0.0, 2*pi)
        courseIndex <- getRandomR (0, (length saucerCoursesLeft) - 1)
        let (x,y,courses') = if initPos < 1.0
                        then (0.0, initPos, saucerCoursesLeft !! courseIndex)
                        else (1.0, initPos, saucerCoursesRight !! courseIndex)
        let v = if x == 0.0 then vector2 saucerVel    0.0
                else             vector2 (-saucerVel) 0.0
        return Saucer { pos    = vector2 x y,
                        vel    = v,
                        radius = saucerRad,
                        course = 0,
                        courses = courses',
                        fireAng = 0,
                        fireAngs = fireAngs',
                        done   = NoEvent,
                        spawn  = NoEvent,
                        polys   = saucerPolygons
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
