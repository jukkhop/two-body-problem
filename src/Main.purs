module Main where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect, foreachE)
import Graphics.Canvas (CanvasElement, Context2D, Dimensions, arc, beginPath, clearRect, fill, getCanvasElementById, getContext2D, moveTo, scale, setCanvasHeight, setCanvasWidth, setFillStyle)
import Math (floor, pi, pow, sqrt)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)
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
  gravity: 12.5,
  massRatio: 1.0,
  timeStep: 0.005
}

config :: Config
config = {
  bodyColor: "#ffffff",
  bodyRadius: 8.0,
  pixelRatio: 2.0,
  scale: 300.0
}

main :: Effect Unit
main = do
  canvas <- unsafePartial getCanvas
  ctx <- getContext2D canvas
  wind <- window
  width <- innerWidth wind
  height <- innerHeight wind
  scaleCanvas wind canvas ctx

  let
    { eccentricity, massRatio } = consts
    state = initialState massRatio eccentricity
    dims = { width: toNumber width, height: toNumber height }

  void $ requestAnimationFrame (simulate wind dims ctx state) wind

getCanvas :: Partial => Effect CanvasElement
getCanvas = do
  maybeCanvas <- getCanvasElementById "canvas"
  case maybeCanvas of
    Nothing -> crashWith "Canvas not found"
    Just canvas -> pure canvas

scaleCanvas :: Window -> CanvasElement -> Context2D -> Effect Unit
scaleCanvas wind canv ctx = do
  width <- innerWidth wind
  height <- innerHeight wind
  let { pixelRatio: ratio } = config
  setCanvasWidth canv (ratio * (toNumber width))
  setCanvasHeight canv (ratio * (toNumber height))
  scale ctx { scaleX: ratio, scaleY: ratio }

simulate :: Window -> Dimensions -> Context2D -> State -> Effect Unit
simulate wind dims ctx state = do
  let newState = updateState state consts.timeStep
  render dims ctx newState
  void $ requestAnimationFrame (simulate wind dims ctx newState) wind

render :: Dimensions -> Context2D -> State -> Effect Unit
render dims ctx state = do
  let
    { width, height } = dims
    { positions } = state
    { bodyColor: color, bodyRadius: radius } = config
    start = 0.0
    end = 2.0 * pi

  clearRect ctx { width, height, x: 0.0, y: 0.0 }
  beginPath ctx

  foreachE (positions <#> translatePos dims)
    \{ x, y } -> do
      moveTo ctx (x + radius) y
      arc ctx { x, y, radius, start, end }

  setFillStyle ctx color
  fill ctx

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
    unitVect = { x: pos.x / radius, y: pos.y / radius }
    currAccel = accel masses radius unitVect
    newPos = verletPos pos vel currAccel deltaTime

    newRadius = hypotenuse newPos
    newUnitVect = { x: newPos.x / newRadius, y: newPos.y / newRadius }
    newAccel = accel masses newRadius newUnitVect
    newVel = verletVel vel currAccel newAccel deltaTime

    a1 = masses.m2 / masses.total
    a2 = masses.m1 / masses.total
  in
    {
      pos: newPos,
      vel: newVel,
      masses,
      positions: [
        { x: a1 * newPos.x, y: a1 * newPos.y },
        { x: -a2 * newPos.x, y: -a2 * newPos.y }
      ]
    }

accel :: Masses -> Number -> Vector -> Vector
accel { m1, m2 } radius unitVect =
  let
    scalar = -(consts.gravity * m1 * m2) / radius ** 2.0
    accelX = scalar * unitVect.x
    accelY = scalar * unitVect.y
  in
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

hypotenuse :: Vector -> Number
hypotenuse { x, y } = sqrt (x ** 2.0 + y ** 2.0)

initialVelocity :: Number -> Number -> Number
initialVelocity ratio ecc =
  sqrt (1.0 + ratio) * (1.0 + ecc)

translatePos :: Dimensions -> Vector -> Vector
translatePos { width, height } { x, y } =
  let
    { scale } = config
    middleX = floor (width / 2.0)
    middleY = floor (height / 2.0)
    centerX = x * scale + middleX
    centerY = y * scale + middleY
  in
    { x: centerX, y: centerY }
