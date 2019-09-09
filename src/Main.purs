module Main where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect, foreachE)
import Effect.Console (log)
import Effect.Ref (Ref, new, read, write)
import Graphics.Canvas (CanvasElement, Context2D, Dimensions, arc, beginPath, clearRect, fill, getCanvasElementById, getContext2D, moveTo, scale, setCanvasHeight, setCanvasWidth, setFillStyle)
import Math (floor, pi, pow, sqrt)
import Web.HTML (Window, window)
import Web.HTML.Window (innerHeight, innerWidth, requestAnimationFrame)

infixl 8 pow as **

type Constants = {
  eccentricity :: Number,
  gravity :: Number,
  massRatio :: Number,
  timeStep :: Number
}

type Config = {
  bodyColor :: String,
  bodyRadius :: Number,
  pixelRatio :: Number,
  scale :: Number
}

type Vector = { x :: Number, y :: Number }

type Masses = {
  m1 :: Number,
  m2 :: Number,
  ratio :: Number,
  total :: Number
}

type State = {
  pos :: Vector,
  vel :: Vector,
  masses :: Masses,
  positions :: Array Vector
}

consts :: Constants
consts = {
  eccentricity: 0.7,
  gravity: 10.0,
  massRatio: 1.0,
  timeStep: 0.005
}

config :: Config
config = {
  bodyColor: "#ffffff",
  bodyRadius: 7.0,
  pixelRatio: 2.0,
  scale: 350.0
}

main :: Effect Unit
main = do
  maybeCanvas <- getCanvasElementById "canvas"
  case maybeCanvas of
    Nothing -> log "Canvas not found"
    Just canv -> do
      ctx <- getContext2D canv
      wind <- window
      width <- innerWidth wind
      height <- innerHeight wind
      scaleCanvas wind canv ctx
      let
        { eccentricity: e, massRatio } = consts
        state = {
          pos: { x: 1.0, y: 0.0 },
          vel: { x: 0.0, y: initialVelocity 1.0 e },
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
        dims = { width: toNumber width, height: toNumber height }

      stateRef <- new state
      void $ requestAnimationFrame (update wind dims ctx stateRef) wind

scaleCanvas :: Window -> CanvasElement -> Context2D -> Effect Unit
scaleCanvas wind canv ctx = do
  width <- innerWidth wind
  height <- innerHeight wind
  let { pixelRatio: ratio } = config
  setCanvasWidth canv (ratio * (toNumber width))
  setCanvasHeight canv (ratio * (toNumber height))
  scale ctx { scaleX: ratio, scaleY: ratio }

update :: Window -> Dimensions -> Context2D -> Ref State -> Effect Unit
update wind dims ctx stateRef = do
  { pos, vel, masses, positions } <- read stateRef
  let
    dt = consts.timeStep

    radius = hypotenuse pos
    unitVect = { x: pos.x / radius, y: pos.y / radius }
    currAccel = accel masses radius unitVect
    newPos = verletPos pos vel currAccel dt

    newRadius = hypotenuse newPos
    newUnitVect = { x: newPos.x / newRadius, y: newPos.y / newRadius }
    newAccel = accel masses newRadius newUnitVect
    newVel = verletVel vel currAccel newAccel dt

    a1 = (masses.m1 / masses.total)
    a2 = (masses.m2 / masses.total)

    newState = {
      pos: newPos,
      vel: newVel,
      masses,
      positions: [
        { x: a1 * newPos.x, y: a1 * newPos.y },
        { x: -a2 * newPos.x, y: -a2 * newPos.y }
      ]
    }

  write newState stateRef
  render dims ctx stateRef
  void $ requestAnimationFrame (update wind dims ctx stateRef) wind

render :: Dimensions -> Context2D -> Ref State -> Effect Unit
render dims ctx stateRef = do
  { positions } <- read stateRef
  let
    { width, height } = dims
    { bodyColor: color, bodyRadius: radius } = config
    start = 0.0
    end = 2.0 * pi

  clearRect ctx { width, height, x: 0.0, y: 0.0 }
  beginPath ctx

  foreachE positions
    (\pos -> do
      let tpos = translatePos dims pos
      moveTo ctx (tpos.x + radius) tpos.y
      arc ctx { x: tpos.x, y: tpos.y, radius, start, end }
    )

  setFillStyle ctx color
  fill ctx

hypotenuse :: Vector -> Number
hypotenuse { x, y } = sqrt (x ** 2.0 + y ** 2.0)

accel :: Masses -> Number -> Vector -> Vector
accel { m1, m2 } radius { x, y } = do
  let
    scalar = -(consts.gravity * m1 * m2) / radius ** 2.0
    accelX = scalar * x
    accelY = scalar * y

  { x: accelX, y: accelY }

verletPos :: Vector -> Vector -> Vector -> Number -> Vector
verletPos pos vel currAccel dt = {
  x: pos.x + vel.x * dt + 0.5 * currAccel.x * dt ** 2.0,
  y: pos.y + vel.y * dt + 0.5 * currAccel.y * dt ** 2.0
}

verletVel :: Vector -> Vector -> Vector -> Number -> Vector
verletVel vel currAccel nextAccel dt = {
  x: vel.x + 0.5 * (nextAccel.x + currAccel.x) * dt,
  y: vel.y + 0.5 * (nextAccel.y + currAccel.y) * dt
}

initialVelocity :: Number -> Number -> Number
initialVelocity ratio ecc =
  sqrt (1.0 + ratio) * (1.0 + ecc)

translatePos :: Dimensions -> Vector -> Vector
translatePos { width, height } { x, y } = do
  let
    { scale } = config
    middleX = floor (width / 2.0)
    middleY = floor (height / 2.0)
    centerX = x * scale + middleX
    centerY = y * scale + middleY

  { x: centerX, y: centerY }

