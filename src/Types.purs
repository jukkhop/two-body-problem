module Types where

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

type Masses = {
  m1 :: Number,
  m2 :: Number,
  ratio :: Number,
  total :: Number
}

type Vector = { x :: Number, y :: Number }

type State = {
  pos :: Vector,
  vel :: Vector,
  masses :: Masses,
  positions :: Array Vector
}
