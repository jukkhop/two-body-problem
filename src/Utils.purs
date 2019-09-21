module Utils where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Graphics.Canvas (CanvasElement, Context2D, Dimensions, getCanvasElementById, scale, setCanvasHeight, setCanvasWidth)
import Math (floor)
import Partial (crashWith)
import Types (Vector)
import Web.HTML (Window)
import Web.HTML.Window (innerHeight, innerWidth)

getCanvas :: Partial => Effect CanvasElement
getCanvas = do
  maybeCanvas <- getCanvasElementById "canvas"
  case maybeCanvas of
    Nothing -> crashWith "Canvas not found"
    Just canvas -> pure canvas

scaleCanvas :: Window -> CanvasElement -> Context2D -> Number -> Effect Unit
scaleCanvas wind canv ctx ratio = do
  width <- innerWidth wind
  height <- innerHeight wind
  setCanvasWidth canv (ratio * toNumber width)
  setCanvasHeight canv (ratio * toNumber height)
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
