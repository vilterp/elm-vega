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


type Mark record
  = Point
      { x : FloatVal record
      , y : FloatVal record
      , radius : FloatVal record
      , color : ColorVal record
      }
  | Rect
      { x : FloatVal record
      , y : FloatVal record
      , width : Dimension record
      , height : Dimension record
      , color : ColorVal record
      }


type Dimension r
  = ToVal (FloatVal r)
  | ConstantLength Float


{- welp, extract can't always be record to float. do I really need this separation
between the "value", which has its extractor, and the map?  -}
type FloatVal record
  = QuantitativeFV
      { extract : record -> Float
      , map : FloatMap
      }
  | OrdinalFV
      { extract : record -> String
      , map : OrdinalMap
      }


constantFloat : Float -> FloatVal record
constantFloat val =
  QuantitativeFV
    { extract = always val
    , map = constantFloatMap
    }


-- TODO: square this terminology up w/ vega...
-- scales, not maps...
scaledConstantFloat : FloatMap -> Float -> FloatVal record
scaledConstantFloat map val =
  QuantitativeFV
    { extract = always val
    , map = map
    }


type ColorVal record
  = ConstantColor Color
  | ColorRamp
      { extract : record -> Float
      , map : ColorRamp
      }
  | ColorPalette
      { extract : record -> String
      , colors : List Color -- TODO: allow you to specify key-color pairs
      }


constantColor : Color -> ColorVal r
constantColor color =
  ConstantColor color


type alias ComputedPoint =
  { x : Float
  , y : Float
  , radius : Float
  , color : Color
  }


type alias ComputedRect =
  { x : Float
  , y : Float
  , width : Float
  , height : Float
  , color : Color
  }


{-| first arg is world-space min & max -}
type alias ColorRamp =
  (Float, Float) -> Float -> Color


{-| first arg is world-space min & max -}
-- TODO: fuck it, you have to return a string for ordinal items.
-- we can't have type vars leaking everywhere. it has to be stringified
-- sooner or later before it's rendered; I guess this is sooner.
type alias FloatMap =
  (Float, Float) -> Geom.Dims -> Float -> Float


type alias OrdinalMap =
  List String -> Geom.Dims -> String -> Float


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
        let
          d = 
            Debug.log "height, interval, val" (dims.height, inInterval, val)

          res =
            Geom.lerp (margin, dims.height - margin) inInterval val
        in
          res


ordinalMap : Range -> OrdinalMap
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


colorRamp : Color -> Color -> ColorRamp
colorRamp fromColor toColor =
  Debug.crash "TODO"


type Range
  = ExplicitRange (Float, Float)
  | Width
  | Height


-- TODO: change name of this to compute or something
getAllFloatVals : FloatVal record
              -> Geom.Dims
              -> List record
              -> { values : List Float, interval : WorldSpaceInterval }
getAllFloatVals val dims data =
  case val of
    QuantitativeFV attrs ->
      let
        floatVals =
          data |> List.map attrs.extract

        worldSpaceInterval =
          minMax floatVals |> Maybe.withDefault (0, 0)
      in
        { values = floatVals |> List.map (attrs.map worldSpaceInterval dims)
        , interval = QuantitativeInterval worldSpaceInterval
        }

    OrdinalFV attrs ->
      let
        column =
          data |> List.map attrs.extract

        uniqueItems =
          column |> Util.listUnique
      in
        { values = column |> List.map (attrs.map uniqueItems dims)
        , interval = OrdinalInterval uniqueItems
        }


{-
Okay so how is this dimension shit gonna work? You have to compute the x
first and then the width...
-}


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


type WorldSpaceInterval
  = OrdinalInterval (List String)
  | QuantitativeInterval (Float, Float)


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

        --xAxis =
        --  renderAxis attrs.x.scale xData.bounds dims.width

        points = 
          pointData
          |> List.map makePoint
          |> Diagrams.group
      in
        pointData
        |> List.map makePoint
        |> Diagrams.group
        |> Align.alignCenter

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
