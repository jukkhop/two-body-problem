module World where

import Prelude

import Math (pow, sqrt)
import Types (Constants, Masses, State, Vector)
import Vector (vmap, vmul)

infixl 8 pow as **

initialState :: Constants -> State
initialState { massRatio, eccentricity } =
  {
    pos: { x: 1.0, y: 0.0 },
    vel: { x: 0.0, y: initialVelocity 1.0 eccentricity },
    masses: {
      m1: 1.0,
      m2: massRatio,
      ratio: massRatio,
      total: 1.0 + massRatio
    },
    positions: [
      { x: 0.0, y: 0.0 },
      { x: 0.0, y: 0.0 }
    ]
  }

updateState :: Constants -> State -> State
updateState consts state =
  let
    { timeStep } = consts
    { pos, vel, masses } = state

    radius = hypotenuse pos
    unitVect = vmap (flip div radius <<< negate) pos
    currAccel = accel consts masses radius unitVect
    newPos = verletPos pos vel currAccel timeStep

    newRadius = hypotenuse newPos
    newUnitVect = vmap (flip div newRadius <<< negate) newPos
    newAccel = accel consts masses newRadius newUnitVect
    newVel = verletVel vel currAccel newAccel timeStep
  in
    {
      pos: newPos,
      vel: newVel,
      masses,
      positions: [
        newPos `vmul` (masses.m2 / masses.total),
        newPos `vmul` -(masses.m1 / masses.total)
      ]
    }

accel :: Constants -> Masses -> Number -> Vector -> Vector
accel { gravity } { m1, m2 } radius unitv = unitv `vmul` scalar
  where
    scalar = (gravity * m1 * m2) / radius ** 2.0

verletPos :: Vector -> Vector -> Vector -> Number -> Vector
verletPos pos vel acc dt =
  pos + (vel `vmul` dt) + (acc `vmul` (0.5 * dt ** 2.0))

verletVel :: Vector -> Vector -> Vector -> Number -> Vector
verletVel vel currAccel nextAccel dt =
  vel + ((currAccel + nextAccel) `vmul` (0.5 * dt))

hypotenuse :: Vector -> Number
hypotenuse { x, y } = sqrt (x ** 2.0 + y ** 2.0)

initialVelocity :: Number -> Number -> Number
initialVelocity ratio ecc = sqrt (1.0 + ratio) * (1.0 + ecc)
