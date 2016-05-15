module Test exposing (..)
-- where

import Collage
import Element
import Html exposing (Html)
import Color
import Html.App as App

import Vega exposing (..)
import Diagrams.Core as Diagrams
import Diagrams.Geom as Geom
import Diagrams.Type exposing (..)
import Diagrams.Debug

import Vega.SampleData.Iris as Iris


points =
  -- range should be Width by default, or something
  Point
    { x = QuantitativeFV { extract = .sepalWidth, map = linear Width }
    , y = QuantitativeFV { extract = .petalWidth, map = linear Height }
    , radius =
        QuantitativeFV
          { extract = .petalLength
          , map = linear (ExplicitRange (1, 10))
          }
    , color =
        ColorPalette
          { extract = .species
          , colors = [Color.blue, Color.orange, Color.green]
          }
    }


rects =
  let
    yScale =
      linear Height
  in
    Rect
      { x = OrdinalFV { extract = .species, map = ordinalMap Width }
      , y = QuantitativeFV { extract = .avg, map = yScale }
      , width = ConstantLength 30
      , height = ToVal (scaledConstantFloat (linear Height) 0 )
      --, height = ConstantLength 10
      , color = constantColor Color.blue
      }


diagram =
  render rects dims (Iris.avgSpeciesAttr .sepalWidth)
  --render points dims Iris.table


dims =
  { width = 500
  , height = 500
  }


main =
  App.beginnerProgram
    { model = ()
    , view =
        diagram
        |> Diagrams.toHtml dims
        |> always
    , update = \_ _ -> ()
    }
