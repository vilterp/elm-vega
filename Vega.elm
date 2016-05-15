module Vega exposing (..)
-- where

import Color exposing (Color)
import Collage
import Text

import Diagrams.Type exposing (Diagram)
import Diagrams.Core as Diagrams
import Diagrams.Geom as Geom
import Diagrams.Align as Align exposing (above)
import Diagrams.FillStroke as FillStroke

import Util


type Mark record item
  = Point
      { x : FloatVal record
      , y : FloatVal record
      , radius : FloatVal record
      , color : ColorVal record item
      }


type alias FloatVal record =
  { extract : record -> Float
  , map : FloatMap
  }


constantFloat : Float -> FloatVal record
constantFloat val =
  { extract = always val
  , map = constantFloatMap
  }


type ColorVal record item
  = ConstantColor Color
  | ColorMap
      { extract : record -> item
      , map : ColorMap
      }


constantColor : Color -> ColorVal r i
constantColor color =
  ConstantColor color


type ColorMap
  = ColorRamp ColorRamp
  | ColorPalette (List Color)


type alias ComputedPoint =
  { x : Float
  , y : Float
  , radius : Float
  , color : Color
  }


{-| first arg is world-space min & max -}
type alias ColorRamp =
  (Float, Float) -> Float -> Color


{-| first arg is world-space min & max -}
type alias FloatMap =
  (Float, Float) -> Geom.Dims -> Float -> Float


type alias OrdinalMap a =
  List a -> Geom.Dims -> a -> Float


constantFloatMap : FloatMap
constantFloatMap _ _ val =
  val


-- TODO: allow peeps to pass this in
margin =
  30


linear : Range -> FloatMap
linear range =
  case range of
    ExplicitRange outInterval ->
      \inInterval dims val ->
        Geom.lerp outInterval inInterval val

    Width ->
      \inInterval dims val ->
        Geom.lerp (margin, dims.width - margin) inInterval val

    Height ->
      \inInterval dims val ->
        Geom.lerp (margin, dims.height - margin) inInterval val


colorRamp : Color -> Color -> ColorMap
colorRamp fromColor toColor =
  Debug.crash "TODO"


ordinalMap : Range -> OrdinalMap a
ordinalMap range =
  \items dims item ->
    let
      viewspaceInterval =
        case range of
          ExplicitRange interval ->
            interval

          Width ->
            (margin, dims.width - margin)

          Height ->
            (margin, dims.height - margin)

      itemIdx =
        Util.listFind item items |> Maybe.withDefault 0
    in
      Geom.lerp viewspaceInterval (0, toFloat (List.length items)) (toFloat itemIdx)


type Range
  = ExplicitRange (Float, Float)
  | Width
  | Height


getAllFloatVals : FloatVal record
               -> Geom.Dims
               -> List record
               -> { values : List Float, interval : (Float, Float) }
getAllFloatVals val dims data =
  let
    floatVals =
      data |> List.map val.extract

    worldSpaceInterval =
      minMax floatVals |> Maybe.withDefault (0, 0)
  in
    { values = floatVals |> List.map (val.map worldSpaceInterval dims)
    , interval = worldSpaceInterval
    }


getColorMap : ColorMap -> List item -> item -> Color
getColorMap map items item =
  case map of
    ColorRamp ramp ->
      Debug.crash "TODO"

    ColorPalette colors ->
      let
        itemIdx =
          Util.listFind item items |> Maybe.withDefault 0

        moddedIdx =
          itemIdx % (List.length colors)

        d = 
          Debug.log "itemIdx" (item, itemIdx, moddedIdx, colors)
      in
        colors
        |> Util.listGet moddedIdx
        |> Maybe.withDefault Color.blue


getAllColorVals : ColorVal record item -> List record -> List Color
getAllColorVals val data =
  case val of
    ConstantColor color ->
      data |> List.map (always color)

    ColorMap mapAttrs ->
      let
        items =
          data |> List.map mapAttrs.extract

        uniqueItems =
          items |> Util.listUnique
      in
        items |> List.map (getColorMap mapAttrs.map uniqueItems)


render : Mark r i -> Geom.Dims -> List r -> Diagram t a
render mark dims data =
  case mark of
    Point attrs ->
      let
        xData =
          data |> getAllFloatVals attrs.x dims

        yData =
          data |> getAllFloatVals attrs.y dims

        radiusData =
          data |> getAllFloatVals attrs.radius dims

        colorData =
          data |> getAllColorVals attrs.color

        pointData =
          List.map4
            ComputedPoint
            xData.values
            yData.values
            radiusData.values
            colorData

        fillStroke color =
          color
          |> FillStroke.Solid
          |> FillStroke.justFill

        makePoint point =
          Diagrams.circle point.radius (fillStroke point.color)
          |> Diagrams.move (point.x, point.y)

        --xAxis =
        --  renderAxis attrs.x.scale xData.bounds dims.width

        points = 
          pointData
          |> List.map makePoint
          |> Diagrams.group
      in
        points |> Align.alignCenter
        --(points |> Align.alignCenter) `above` (xAxis |> Align.alignCenter)
        --|> Align.alignCenter


{-
I have
  world space => view space

I want
  tick space => world space

...fuck.
-}

-- TODO: legends for sizes

{-
renderAxis : FloatMap -> (Float, Float) -> Geom.Dims -> Diagram t a
renderAxis floatMap worldSpaceInterval dims =
  let
    numTicks =
      10

    tickIndices =
      [1..numTicks]

    tickPositions =
      tickIndices
      |> List.map floatMap

    tickHeight =
      5 -- TODO configurable

    tickMark =
      Diagrams.vline 5 Collage.defaultLine -- TODO configurable

    tickToWorld =
      floatMap (0, numTicks) { width = numTicks, height = numTicks }

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
      Diagrams.hline viewSpaceLength Collage.defaultLine
  in
    line `above` ticks
-}

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
