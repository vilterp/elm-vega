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
      , radius : ScaledVal r
      }


type alias ComputedPoint =
  { x : Float
  , y : Float
  , radius : Float
  }


type Scale
  = Linear


type alias ScaleWithData =
  { interval : (Float, Float)
  , scale : Scale
  }


doScale : Scale
      -> Range
      -> Geom.Dims
      -> List Float
      -> { bounds : (Float, Float), values : List Float }
doScale scale range dims data =
  let
    worldSpaceBounds =
      minMax data
      |> Maybe.withDefault (0, 0)

    margin =
      30

    viewspaceBounds =
      case range of
        ExplicitRange bounds ->
          bounds

        Width ->
          (margin, dims.width - margin)

        Height ->
          (margin, dims.height - margin)

    interpolate =
      case scale of
        Linear ->
          Geom.lerp viewspaceBounds worldSpaceBounds
  in
    { values = data |> List.map interpolate
    , bounds = worldSpaceBounds
    }


-- TODO: generalize past Float
type alias ScaledVal r =
  { domain : r -> Float
  , scale : Scale
  , range : Range
  }


type Range
  = ExplicitRange (Float, Float)
  | Width
  | Height


render : Mark r -> Geom.Dims -> List r -> Diagram t a
render mark dims data =
  case mark of
    Point attrs ->
      let
        xData =
          data
          |> List.map attrs.x.domain
          |> doScale attrs.x.scale attrs.x.range dims

        yData =
          data
          |> List.map attrs.y.domain
          |> doScale attrs.y.scale attrs.y.range dims

        radiusData =
          data
          |> List.map attrs.radius.domain
          |> doScale attrs.radius.scale attrs.radius.range dims

        pointData =
          List.map3 ComputedPoint xData.values yData.values radiusData.values

        fillStroke =
          Color.blue
          |> FillStroke.Solid
          |> FillStroke.justFill

        makePoint point =
          Diagrams.circle point.radius fillStroke
          |> Diagrams.move (point.x, point.y)

        xAxis =
          renderAxis attrs.x.scale xData.bounds dims.width

        points = 
          pointData
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
