module Main where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref (Ref, new, read, write)
import Graphics.Canvas (CanvasElement, Context2D, Dimensions, arc, beginPath, clearRect, fill, getCanvasElementById, getContext2D, moveTo, scale, setCanvasHeight, setCanvasWidth, setFillStyle)
import Math (pi, pow, sqrt)
import Web.HTML (Window, window)
import Web.HTML.Window (innerHeight, innerWidth, requestAnimationFrame)

type Constants = {
  bodyColor :: String,
  bodyRadius :: Number,
  eccentricity :: Number,
  gravity :: Number,
  pixelRatio :: Number,
  timeStep :: Number
}

consts :: Constants
consts = {
  bodyColor: "#ffffff",
  bodyRadius: 7.0,
  eccentricity: 0.75,
  gravity: 1.0,
  pixelRatio: 2.0,
  timeStep: 1.0
}

type Body
  = { mass :: Number
    , x :: Number
    , y :: Number
    , vx :: Number
    , vy :: Number
    }

type State = { body1 :: Body, body2 :: Body }
type Vector = { x :: Number, y :: Number }

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
        { eccentricity: e } = consts
        state =
          { body1:
            { mass: 21.5
            , x: 400.0
            , y: 400.0
            , vx: 0.0
            , vy: -(initialVelocity 1.0 e)
            }
          , body2:
            { mass: 21.5
            , x: 450.0
            , y: 400.0
            , vx: 0.0
            , vy: initialVelocity 1.0 e
            }
          }

        dims = { width: toNumber width, height: toNumber height }

      stateRef <- new state
      void $ requestAnimationFrame (update wind dims ctx stateRef) wind

initialVelocity :: Number -> Number -> Number
initialVelocity ratio ecc = sqrt (1.0 + ratio) * (1.0 + ecc)

update :: Window -> Dimensions -> Context2D -> Ref State -> Effect Unit
update wind dims ctx stateRef = do
  { body1, body2 } <- read stateRef
  let
    distX = body2.x - body1.x
    distY = body2.y - body1.y
    radius = sqrt (distX `pow` 2.0 + distY `pow` 2.0)

    unitVect = { x: distX / radius, y: distY / radius }
    unitVectNeg = { x: -unitVect.x, y: -unitVect.y }

    accel1 = accel body1.mass body2.mass radius unitVect
    accel2 = accel body2.mass body1.mass radius unitVectNeg

    dt = consts.timeStep

    newState =
      { body1:
        { mass: body1.mass
        , x: body1.x + body1.vx * dt
        , y: body1.y + body1.vy * dt
        , vx: body1.vx + accel1.x * dt
        , vy: body1.vy + accel1.y * dt
        }
      , body2:
        { mass: body2.mass
        , x: body2.x + body2.vx * dt
        , y: body2.y + body2.vy * dt
        , vx: body2.vx + accel2.x * dt
        , vy: body2.vy + accel2.y * dt
        }
      }
      
  write newState stateRef
  render dims ctx stateRef
  void $ requestAnimationFrame (update wind dims ctx stateRef) wind

accel :: Number -> Number -> Number -> Vector -> Vector
accel m1 m2 radius unitVector = do
  let
    scalar = (consts.gravity * m1 * m2) / radius `pow` 2.0
    accelX = scalar * unitVector.x
    accelY = scalar * unitVector.y

  { x: accelX, y: accelY }

render :: Dimensions -> Context2D -> Ref State -> Effect Unit
render { width, height } ctx stateRef = do
  { body1, body2 } <- read stateRef
  clearRect ctx { width, height, x: 0.0, y: 0.0 }
  beginPath ctx
  
  let
    { bodyColor: color, bodyRadius: radius } = consts
    start = 0.0
    end = 2.0 * pi
  
  moveTo ctx (body1.x + radius) body1.y
  arc ctx { x: body1.x, y: body1.y, radius, start, end }

  moveTo ctx (body2.x + radius) body2.y
  arc ctx { x: body2.x, y: body2.y, radius, start, end }

  setFillStyle ctx color
  fill ctx

scaleCanvas :: Window -> CanvasElement -> Context2D -> Effect Unit
scaleCanvas wind canv ctx = do
  width <- innerWidth wind
  height <- innerHeight wind
  let { pixelRatio: ratio } = consts
  setCanvasWidth canv (ratio * (toNumber width))
  setCanvasHeight canv (ratio * (toNumber height))
  scale ctx { scaleX: ratio, scaleY: ratio }
