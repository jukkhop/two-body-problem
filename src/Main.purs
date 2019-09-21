module Main where

import Prelude

import Data.Int (toNumber)
import Effect (Effect, foreachE)
import Graphics.Canvas (Context2D, Dimensions, arc, beginPath, clearRect, fill, getContext2D, moveTo, setFillStyle)
import Math (pi)
import Partial.Unsafe (unsafePartial)
import Types (Config, Constants, State)
import Utils (getCanvas, scaleCanvas, translatePos)
import Web.HTML (Window, window)
import Web.HTML.Window (innerHeight, innerWidth, requestAnimationFrame)
import World (initialState, updateState)

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

  let
    state = initialState consts
    dims = { width: toNumber width, height: toNumber height }

  scaleCanvas wind canvas ctx config.pixelRatio
  void $ requestAnimationFrame (simulate wind dims ctx state) wind

simulate :: Window -> Dimensions -> Context2D -> State -> Effect Unit
simulate wind dims ctx state = do
  let newState = updateState consts state
  render dims ctx newState
  void $ requestAnimationFrame (simulate wind dims ctx newState) wind

render :: Dimensions -> Context2D -> State -> Effect Unit
render dims ctx state = do
  let
    { width, height } = dims
    { positions } = state
    { bodyColor: color, bodyRadius: radius, scale } = config
    start = 0.0
    end = 2.0 * pi

  clearRect ctx { width, height, x: 0.0, y: 0.0 }
  beginPath ctx

  foreachE (positions <#> translatePos dims scale)
    \{ x, y } -> do
      moveTo ctx (x + radius) y
      arc ctx { x, y, radius, start, end }

  setFillStyle ctx color
  fill ctx
