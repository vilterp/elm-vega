-- TODO: maybe rename to Core?
module Vega exposing (..)
-- where

import Color exposing (Color)
import Collage
import Text

import Diagrams.Type exposing (Diagram)
import Diagrams.Core as Diagrams
import Diagrams.Geom as Geom
import Diagrams.Align as Align exposing (beside, above)
import Diagrams.FillStroke as FillStroke
import Diagrams.Debug as DDebug

import Util
import Vega.Types exposing (..)
import Vega.Axis as Axis


-- TODO: square this terminology up w/ vega...
-- scales, not maps...
scaledConstantFloat : FloatScale record -> Float -> FloatVal record
scaledConstantFloat scale val =
  { scale = scale
  , source = ConstantFS val
  }


constantColor : Color -> ColorVal r
constantColor color =
  ConstantColor color


constantFloatMap : FloatMap
constantFloatMap _ _ val =
  val


linear : Range -> FloatMap
linear range =
  case range of
    ExplicitRange outInterval ->
      \inInterval dims val ->
        Geom.lerp outInterval inInterval val

    Width ->
      \inInterval dims val ->
        Geom.lerp (0, dims.width) inInterval val

    Height ->
      \inInterval dims val ->
        Geom.lerp (0, dims.height) inInterval val


ordinalMap : Range -> OrdinalMap
ordinalMap range =
  \items dims item ->
    let
      viewspaceInterval =
        case range of
          ExplicitRange interval ->
            interval

          Width ->
            (0, dims.width)

          Height ->
            (0, dims.height)

      itemIdx =
        Util.listFind item items |> Maybe.withDefault 0
    in
      Geom.lerp viewspaceInterval (0, toFloat (List.length items)) (toFloat itemIdx)


colorRamp : Color -> Color -> ColorRamp
colorRamp fromColor toColor =
  Debug.crash "TODO"


-- TODO: change name of this to compute or something
-- TODO: dry up the source case expression
getAllFloatVals : FloatVal record
              -> Geom.Dims
              -> List record
              -> { values : List Float, interval : WorldSpaceInterval }
getAllFloatVals val dims data =
  case val.scale of
    QuantitativeFV attrs ->
      let
        floatVals =
          data |> List.map attrs.extract

        worldSpaceInterval =
          minMax floatVals |> Maybe.withDefault (0, 0)

        toFeed =
          case val.source of
            ConstantFS constant ->
              -- TODO: less dumb way of doing this
              floatVals |> List.map (always constant)

            FromData ->
              floatVals
      in
        { values = toFeed |> List.map (attrs.map worldSpaceInterval dims)
        , interval = QuantitativeInterval worldSpaceInterval
        }

    OrdinalFV attrs ->
      let
        column =
          data |> List.map attrs.extract

        uniqueItems =
          column |> Util.listUnique

        toFeed =
          case val.source of
            ConstantFS constant ->
              -- TODO: I guess you should be able to pass in a string though?
              Debug.crash "can't give a float constant to an ordinal scale"

            FromData ->
              column
      in
        { values = column |> List.map (attrs.map uniqueItems dims)
        , interval = OrdinalInterval uniqueItems
        }


getAllForDimension : Dimension record
                  -> Geom.Dims
                  -> WorldSpaceInterval
                  -> List (Float, record)
                  -> { values : List Float, interval : WorldSpaceInterval }
getAllForDimension dimension dims worldSpaceInterval data =
  case dimension of
    ToVal floatVal ->
      { values =
          getAllFloatVals floatVal dims (List.map snd data)
          |> .values
          |> List.map2 (\(relatedVal, _) toVal -> toVal - relatedVal) data
      , interval = worldSpaceInterval
      }

    ConstantLength length ->
      Debug.log "ConstantLength"
        { values = data |> List.map (always length)
        , interval = worldSpaceInterval
        }


getAllColorVals : ColorVal record -> List record -> List Color
getAllColorVals val data =
  case val of
    ConstantColor color ->
      data |> List.map (always color)

    ColorRamp attrs ->
      Debug.crash "TODO"

    ColorPalette attrs ->
      let
        items =
          data |> List.map attrs.extract

        uniqueItems =
          items |> Util.listUnique

        colorForItem item =
          let
            itemIdx =
              Util.listFind item uniqueItems |> Maybe.withDefault 0

            moddedIdx =
              itemIdx % (List.length attrs.colors)
          in
            attrs.colors
            |> Util.listGet moddedIdx
            |> Maybe.withDefault Color.blue
      in
        items |> List.map colorForItem


-- TODO: DRY up
render : Mark r -> Geom.Dims -> List r -> Diagram t a
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

        xAxis =
          Axis.renderQuantitativeAxis Geom.Down dims xData.interval attrs.x.scale

        yAxis =
          Axis.renderQuantitativeAxis Geom.Left dims yData.interval attrs.y.scale

        points = 
          pointData
          |> List.map makePoint
          |> Diagrams.group
          |> Align.alignCenter -- TODO: reduce use of this... linear in # points
      in
        (yAxis `beside` points |> Align.alignRight) `above` (xAxis |> Align.alignRight)
        |> Align.alignCenter -- I think this traverses all the points again :(

    Rect attrs ->
      let
        xData =
          data |> getAllFloatVals attrs.x dims

        yData =
          data |> getAllFloatVals attrs.y dims

        colorData =
          data |> getAllColorVals attrs.color

        widthData =
          data
          |> List.map2 (,) xData.values
          |> getAllForDimension attrs.width dims xData.interval

        heightData =
          data
          |> List.map2 (,) yData.values
          |> getAllForDimension attrs.height dims yData.interval

        rects =
          List.map5
            ComputedRect
            xData.values
            yData.values
            widthData.values
            heightData.values
            colorData

        fillStroke color =
          color
          |> FillStroke.Solid
          |> FillStroke.justFill

        makeRect rect =
          Diagrams.rect rect.width rect.height (fillStroke rect.color)
          |> Align.alignTop
          |> Align.alignLeft
          |> Diagrams.move (rect.x, rect.y)
      in
        rects
        |> Debug.log "rects"
        |> List.map makeRect
        |> Diagrams.group
        |> Align.alignCenter


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
