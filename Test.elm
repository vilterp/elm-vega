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


mark =
  Point
    { x = { scale = Linear, extract = .temperature }
    , y = { scale = Linear, extract = .humidity }
    }


data =
  let
    temperature =
      [50..100]

    humidity =
      [40..90]
  in
    List.map2 (,) temperature humidity
    |> List.map (\(t, h) -> { temperature = t, humidity = h })


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
  render mark dims data
  |> renderDia dims


main =
  App.beginnerProgram
    { model = ()
    , view = view
    , update = \_ _ -> ()
    }
