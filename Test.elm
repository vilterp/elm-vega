module Test exposing (..)
-- where

import Collage
import Element
import Html exposing (Html)
import Html.App as App

import Vega exposing (..)
import Diagrams.Core as Diagrams
import Diagrams.Geom as Geom
import Diagrams.Type exposing (..)
import Diagrams.Debug

import Vega.SampleData.Iris as Iris


mark =
  Point
    { x = { scale = Linear, extract = .sepalWidth }
    , y = { scale = Linear, extract = .petalWidth }
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
