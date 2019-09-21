module Main where

import Prelude

import Canvas (getCanvas, render, scaleCanvas)
import Data.Int (toNumber)
import Effect (Effect)
import Graphics.Canvas (Context2D, Dimensions, getContext2D)
import Partial.Unsafe (unsafePartial)
import Types (Config, State, Constants)
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
  scaleCanvas wind canvas ctx config.pixelRatio

  let
    { eccentricity, massRatio } = consts
    state = initialState massRatio eccentricity
    dims = { width: toNumber width, height: toNumber height }

  void $ requestAnimationFrame (simulate wind dims ctx state) wind

simulate :: Window -> Dimensions -> Context2D -> State -> Effect Unit
simulate wind dims ctx state = do
  let newState = updateState state consts.timeStep
  render config dims ctx newState
  void $ requestAnimationFrame (simulate wind dims ctx newState) wind
