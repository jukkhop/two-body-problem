module Canvas where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect, foreachE)
import Graphics.Canvas (CanvasElement, Context2D, Dimensions, arc, beginPath, clearRect, fill, getCanvasElementById, moveTo, scale, setCanvasHeight, setCanvasWidth, setFillStyle)
import Math (floor, pi)
import Partial (crashWith)
import Types (Config, State, Vector)
import Web.HTML (Window)
import Web.HTML.Window (innerHeight, innerWidth)

getCanvas :: Partial => Effect CanvasElement
getCanvas = do
  maybeCanvas <- getCanvasElementById "canvas"
  case maybeCanvas of
    Nothing -> crashWith "Canvas not found"
    Just canvas -> pure canvas

render :: Config -> Dimensions -> Context2D -> State -> Effect Unit
render config dims ctx state = do
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

scaleCanvas :: Window -> CanvasElement -> Context2D -> Number -> Effect Unit
scaleCanvas wind canv ctx ratio = do
  width <- innerWidth wind
  height <- innerHeight wind
  setCanvasWidth canv $ ratio * toNumber width
  setCanvasHeight canv $ ratio * toNumber height
  scale ctx { scaleX: ratio, scaleY: ratio }

translatePos :: Dimensions -> Number -> Vector -> Vector
translatePos { width, height } scale { x, y } =
  let
    middleX = floor (width / 2.0)
    middleY = floor (height / 2.0)
    centerX = x * scale + middleX
    centerY = y * scale + middleY
  in
    { x: centerX, y: centerY }
