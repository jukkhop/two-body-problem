module World where

import Prelude

import Data.Int (toNumber)
import Effect (Effect, foreachE)
import Graphics.Canvas (Context2D, Dimensions, arc, beginPath, clearRect, fill, getContext2D, moveTo, setFillStyle)
import Math (pi, pow, sqrt)
import Partial.Unsafe (unsafePartial)
import Vector (vmap, vmul)
import Web.HTML (Window, window)
import Web.HTML.Window (innerHeight, innerWidth, requestAnimationFrame)
import Types (Constants, Masses, State, Vector)

infixl 8 pow as **

consts :: Constants
consts = {
  eccentricity: 0.7,
  gravity: 12.5,
  massRatio: 1.0,
  timeStep: 0.005
}

initialState :: Number -> Number -> State
initialState massRatio eccentricity =
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

updateState :: State -> Number -> State
updateState state deltaTime =
  let
    { pos, vel, masses } = state

    radius = hypotenuse pos
    unitVect = vmap (flip div radius <<< negate) pos
    currAccel = accel masses radius unitVect
    newPos = verletPos pos vel currAccel deltaTime

    newRadius = hypotenuse newPos
    newUnitVect = vmap (flip div newRadius <<< negate) newPos
    newAccel = accel masses newRadius newUnitVect
    newVel = verletVel vel currAccel newAccel deltaTime
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

accel :: Masses -> Number -> Vector -> Vector
accel { m1, m2 } radius unitv = unitv `vmul` scalar
  where
    scalar = (consts.gravity * m1 * m2) / radius ** 2.0

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
