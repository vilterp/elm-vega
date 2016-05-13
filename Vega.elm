module Vega exposing (..)
-- where

import Color
import Collage
import Text

import Diagrams.Type exposing (Diagram)
import Diagrams.Core as Diagrams
import Diagrams.Geom as Geom
import Diagrams.Align as Align exposing (above)
import Diagrams.FillStroke as FillStroke


type Mark r
  = Point
      { x : ScaledVal r
      , y : ScaledVal r
      }


type Scale
  = Linear


type alias ScaleWithData =
  { interval : (Float, Float)
  , scale : Scale
  }


doScale : Scale
      -> Float
      -> List Float
      -> { interval : (Float, Float), values : List Float }
doScale scale targetLength data =
  let
    (dataMin, dataMax) =
      minMax data
      |> Maybe.withDefault (0, 0)

    interpolate =
      case scale of
        Linear ->
          Geom.lerp (0, targetLength) (dataMin, dataMax)
  in
    { values = data |> List.map interpolate
    , interval = (dataMin, dataMax)
    }


-- TODO: generalize past Float
type alias ScaledVal r =
  { extract : r -> Float
  , scale : Scale
  }


render : Mark r -> Geom.Dims -> List r -> Diagram t a
render mark dims data =
  case mark of
    Point attrs ->
      let
        xData =
          data
          |> List.map attrs.x.extract
          |> doScale attrs.x.scale (dims.width - 30)

        yData =
          data
          |> List.map attrs.y.extract
          |> doScale attrs.y.scale (dims.height - 30)

        xys =
          List.map2 (,) xData.values yData.values

        fillStroke =
          Color.blue
          |> FillStroke.Solid
          |> FillStroke.justFill

        makePoint coords =
          Diagrams.circle 5 fillStroke
          |> Diagrams.move coords

        xAxis =
          renderAxis attrs.x.scale xData.interval dims.width

        points = 
          xys
          |> List.map makePoint
          |> Diagrams.group
      in
        (points |> Align.alignCenter) `above` (xAxis |> Align.alignCenter)
        |> Align.alignCenter


renderAxis : Scale -> (Float, Float) -> Float -> Diagram t a
renderAxis scale (theMin, theMax) targetLength =
  let
    numTicks =
      10

    tickToView =
      case scale of
        Linear ->
          Geom.lerp (0, targetLength) (1, numTicks)

    tickIndices =
      [1..numTicks]

    tickPositions =
      tickIndices
      |> List.map tickToView

    tickHeight =
      5 -- TODO configurable

    tickMark =
      Diagrams.vline 5 Collage.defaultLine -- TODO configurable

    tickToWorld =
      case scale of
        Linear ->
          Geom.lerp (theMin, theMax) (1, numTicks)

    tickLabel idx =
      tickToWorld idx
      |> truncate -- TODO something other than this
      |> toString
      |> Diagrams.text Text.defaultStyle -- TODO configurable

    makeTick idx pos =
      tickMark `above` (tickLabel idx)
      |> Diagrams.moveX pos

    ticks =
      List.map2 makeTick tickIndices tickPositions
      |> Diagrams.group
      |> Align.alignCenter
    
    line =
      Diagrams.hline targetLength Collage.defaultLine
  in
    line `above` ticks

-- utils

minMax : List comparable -> Maybe (comparable, comparable)
minMax values =
  let
    step x accum =
      case accum of
        Nothing ->
          Just (x, x)

        Just (theMin, theMax) ->
          Just (min x theMin, max x theMax)
  in
    List.foldl step Nothing values
