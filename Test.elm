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


mark =
  -- range should be Width by default, or something
  Point
    { x = { extract = .sepalWidth, map = linear Width }
    , y = { extract = .petalWidth, map = linear Height }
    , radius =
        { extract = .petalLength
        , map = linear (ExplicitRange (1, 10))
        }
    , color =
        ColorMap
          { extract = .species
          , map = ColorPalette [Color.blue, Color.orange, Color.green]
          }
    }


dims =
  { width = 500
  , height = 500
  }


main =
  App.beginnerProgram
    { model = ()
    , view =
        render mark dims Iris.table
        |> Diagrams.toHtml dims
        |> always
    , update = \_ _ -> ()
    }
