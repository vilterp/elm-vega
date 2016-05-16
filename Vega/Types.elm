module Vega.Types exposing (..)
-- where

import Color exposing (Color)

import Diagrams.Geom as Geom

-- I'm going to have to expose constructors for these,
-- but not the actual types... or something...

{-
Really what we're doing here is building a function from record
to diagram that's mapped over the data to give us all our marks.

The function also has to give us the world space coordinate it extracts,
so we can establish the range.
-}
type Mark record
  = Point
      { x : FloatVal record
      , y : FloatVal record
      , radius : FloatVal record
      , color : ColorVal record
      }
  | Rect
      { x : FloatVal record
      , y : FloatVal record
      , width : Dimension record
      , height : Dimension record
      , color : ColorVal record
      }


type Dimension r
  = ToVal (FloatVal r)
  | ConstantLength Float


type alias FloatVal record =
  { scale : FloatScale record
  , source : FloatValSource
  }


type FloatValSource
  = FromData
  | ConstantFS Float


type FloatScale record
  = QuantitativeFV
      { extract : record -> Float
      , map : FloatMap
      }
  | OrdinalFV
      { extract : record -> String
      , map : OrdinalMap
      }


type ColorVal record
  = ConstantColor Color
  | ColorRamp
      { extract : record -> Float
      , map : ColorRamp
      }
  | ColorPalette
      { extract : record -> String
      , colors : List Color -- TODO: allow you to specify key-color pairs
      }


type alias ComputedPoint =
  { x : Float
  , y : Float
  , radius : Float
  , color : Color
  }


type alias ComputedRect =
  { x : Float
  , y : Float
  , width : Float
  , height : Float
  , color : Color
  }


{-| first arg is world-space min & max -}
type alias ColorRamp =
  (Float, Float) -> Float -> Color


{-| first arg is world-space min & max -}
type alias FloatMap =
  (Float, Float) -> Geom.Dims -> Float -> Float


type alias OrdinalMap =
  List String -> Geom.Dims -> String -> Float


type Range
  = ExplicitRange (Float, Float)
  | Width
  | Height


type WorldSpaceInterval
  = OrdinalInterval (List String)
  | QuantitativeInterval (Float, Float)

