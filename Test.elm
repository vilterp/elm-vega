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
    { x =
        { scale = QuantitativeFV { extract = .sepalWidth, map = linear Width }
        , source = FromData
        }
    , y =
        { scale = QuantitativeFV { extract = .petalWidth, map = linear Height }
        , source = FromData
        }
    , radius =
        { scale =
            QuantitativeFV
              { extract = .petalLength
              , map = linear (ExplicitRange (1, 10))
              }
        , source = FromData
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
      QuantitativeFV { extract = .avg, map = linear Height }
  in
    Rect
      { x =
          { scale = OrdinalFV { extract = .species, map = ordinalMap Width }
          , source = FromData
          }
      , y = { scale = yScale, source = FromData }
      , width = ConstantLength 30 -- TODO: make this dependent on dims!!
      , height = ToVal { scale = yScale, source = ConstantFS 0 }
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
