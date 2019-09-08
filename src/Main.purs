module Main where

import Prelude

import Data.Array ((!!))
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref, new, read, write)
import Graphics.Canvas (CanvasElement, Context2D, Dimensions, arc, beginPath, clearRect, fill, getCanvasElementById, getContext2D, moveTo, scale, setCanvasHeight, setCanvasWidth, setFillStyle)
import Math (floor, pi, pow, sqrt)
import Web.HTML (Window, window)
import Web.HTML.Window (innerHeight, innerWidth, requestAnimationFrame)

type Constants = {
  eccentricity :: Number,
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

type State = {
  x :: Number,
  y :: Number,
  vx :: Number,
  vy :: Number,
  masses :: {
    m1 :: Number,
    m2 :: Number,
    ratio :: Number,
    total :: Number
  },
  positions :: Array Vector
}

consts :: Constants
consts = {
  eccentricity: 0.7,
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
          x: 1.0,
          y: 0.0,
          vx: 0.0,
          vy: initialVelocity 1.0 e,
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

initialVelocity :: Number -> Number -> Number
initialVelocity ratio ecc =
  sqrt (1.0 + ratio) * (1.0 + ecc)

verletPos :: Number -> Number -> Number -> Number -> Number
verletPos pos vel dt currAccel =
  pos + vel * dt + 0.5 * currAccel * dt `pow` 2.0

verletVel :: Number -> Number -> Number -> Number -> Number
verletVel vel dt currAccel nextAccel =
  vel + 0.5 * (nextAccel + currAccel) * dt

update :: Window -> Dimensions -> Context2D -> Ref State -> Effect Unit
update wind dims ctx stateRef = do
  { x, y, vx, vy, masses, positions } <- read stateRef
  let
    dt = consts.timeStep
    radius = sqrt (x `pow` 2.0 + y `pow` 2.0)
    currAccel = accel radius (x / radius) (y / radius)
    newX = verletPos x vx dt currAccel.x
    newY = verletPos y vy dt currAccel.y
    newRadius = sqrt (newX `pow` 2.0 + newY `pow` 2.0)
    newAccel = accel newRadius (newX / newRadius) (newY / newRadius)
    newVx = verletVel vx dt currAccel.x newAccel.x
    newVy = verletVel vy dt currAccel.y newAccel.y

    a1 = (masses.m2 / masses.total)
    a2 = (masses.m1 / masses.total)

    newState = {
      x: newX,
      y: newY,
      vx: newVx,
      vy: newVy,
      masses,
      positions: [
        { x: -a2 * newX, y: -a2 * newY },
        { x: a1 * newX, y: a1 * newY }
      ]
    }

  write newState stateRef
  render dims ctx stateRef
  void $ requestAnimationFrame (update wind dims ctx stateRef) wind

accel :: Number -> Number -> Number -> Vector
accel radius unitX unitY = do
  let
    scalar = -(10.0 + consts.massRatio) / radius `pow` 2.0
    accelX = scalar * unitX
    accelY = scalar * unitY

  { x: accelX, y: accelY }

render :: Dimensions -> Context2D -> Ref State -> Effect Unit
render { width, height } ctx stateRef = do
  { x, y, masses, positions } <- read stateRef

  let
    { m1, m2, ratio, total } = masses
    { bodyColor: color, bodyRadius: radius } = config
    start = 0.0
    end = 2.0 * pi
    def = { x: 0.0, y: 0.0 }
    pos1 = fromMaybe def (positions !! 0)
    pos2 = fromMaybe def (positions !! 1)
    tpos1 = translatePos { width, height } { x: pos1.x, y: pos1.y }
    tpos2 = translatePos { width, height } { x: pos2.x, y: pos2.y }

  clearRect ctx { width, height, x: 0.0, y: 0.0 }
  beginPath ctx

  moveTo ctx (tpos1.x + radius) tpos1.y
  arc ctx { x: tpos1.x, y: tpos1.y, radius, start, end }

  moveTo ctx (tpos2.x + radius) tpos2.y
  arc ctx { x: tpos2.x, y: tpos2.y, radius, start, end }

  setFillStyle ctx color
  fill ctx

translatePos :: Dimensions -> Vector -> Vector
translatePos { width, height } { x, y } = do
  let
    { scale } = config
    middleX = floor (width / 2.0)
    middleY = floor (height / 2.0)
    centerX = x * scale + middleX
    centerY = y * scale + middleY

  { x: centerX, y: centerY }

scaleCanvas :: Window -> CanvasElement -> Context2D -> Effect Unit
scaleCanvas wind canv ctx = do
  width <- innerWidth wind
  height <- innerHeight wind
  let { pixelRatio: ratio } = config
  setCanvasWidth canv (ratio * (toNumber width))
  setCanvasHeight canv (ratio * (toNumber height))
  scale ctx { scaleX: ratio, scaleY: ratio }
