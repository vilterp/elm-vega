module Vega.Axis exposing (..)
-- where

import Color
import Text
import Collage

import Diagrams.Geom as Geom exposing (Direction(..))
import Diagrams.Core as Diagrams
import Diagrams.Type exposing (Diagram)
import Diagrams.Align as Align exposing (beside, above)
import Diagrams.FillStroke as FillStroke

import Vega.Types exposing (..)

-- TODO: make configurable
margin =
  30


renderQuantitativeAxis : Direction -> Geom.Dims -> WorldSpaceInterval -> FloatScale r -> Diagram t a
renderQuantitativeAxis dir dims worldSpaceInterval scale =
  let
    vertLine =
      Diagrams.vline dims.height Collage.defaultLine -- TODO: make configurable

    horizLine =
      Diagrams.hline dims.width Collage.defaultLine

    tickLength =
      5 -- TODO: make configurable

    vertTick =
      Diagrams.vline tickLength Collage.defaultLine

    horizTick =
      Diagrams.hline tickLength Collage.defaultLine

    makeTickAndLabel label =
      Diagrams.text Text.defaultStyle label
      |> positionAroundCenter dir vertTick horizTick

    numTicks =
      10 -- TODO: make configurable

    ticksToWorld =
      case worldSpaceInterval of
        QuantitativeInterval interval ->
          Geom.lerp interval (1, numTicks)

        OrdinalInterval strings ->
          Debug.crash "no support for rendering ordinal axes yet :("

    -- TODO: only works for linear!
    worldSpacePositions =
      [1..numTicks] |> List.map ticksToWorld

    -- TODO: DRY up. ugh.
    viewSpaceLength =
      case dir of
        Up ->
          (dims.height, \x -> (x, 0))

        Down ->
          (dims.height, \x -> (x, 0))

        Left ->
          (dims.width, \y -> (0, y))

        Right ->
          (dims.width, \y -> (0, y))

    ticksToView =
      Geom.lerp (margin, fst viewSpaceLength - margin) (1, numTicks)

    viewSpacePositions =
      [1..numTicks] |> List.map ticksToView

    ticksAndLabels =
      List.map2
        (\wsPos vsPos ->
          wsPos
          |> truncate -- TODO: not this
          |> toString
          |> makeTickAndLabel
          |> Diagrams.move (vsPos |> snd viewSpaceLength))
          -- TODO: align left, right, up, or down. fuck.
        worldSpacePositions
        viewSpacePositions
      |> Diagrams.group
      |> Align.alignCenter
  in
    positionAroundCenter dir horizLine vertLine ticksAndLabels


positionAroundCenter : Direction -> Diagram t a -> Diagram t a -> Diagram t a -> Diagram t a
positionAroundCenter dir vertThing horizThing outerThing =
  case dir of
    Up ->
      outerThing `above` vertThing

    Down ->
      vertThing `above` outerThing

    Left ->
      outerThing `beside` horizThing

    Right ->
      horizThing `beside` outerThing
