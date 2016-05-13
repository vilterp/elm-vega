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
    , radius = { extract = .petalLength, map = linear (ExplicitRange (1, 10)) }
    , color = ColorMap { extract = .species, map = ColorPalette [Color.blue, Color.orange] }
    }


-- TODO: move this into diagrams lib
renderDia : Geom.Dims -> Diagram t a -> Html x
renderDia dims dia =
  dia
  |> Diagrams.Debug.showBBox
  |> Diagrams.Debug.showOrigin
  |> Diagrams.render
  |> (\x -> [x])
  |> Collage.collage (truncate dims.width) (truncate dims.height)
  |> Element.toHtml


dims =
  { width = 500
  , height = 500
  }


view : () -> Html x
view model =
  render mark dims Iris.table
  |> renderDia dims


main =
  App.beginnerProgram
    { model = ()
    , view = view
    , update = \_ _ -> ()
    }
