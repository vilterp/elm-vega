module Util exposing (..)
-- where

import Set


listFind : a -> List a -> Maybe Int
listFind item list =
  let
    go idx items =
      case items of
        [] ->
          Nothing

        x::xs ->
          if x == item then
            Just idx
          else
            go (idx + 1) xs
  in
    go 0 list


listGet : Int -> List a -> Maybe a
listGet idx list =
  case (idx, list) of
    (0, x::xs) ->
      Just x

    (n, []) ->
      Nothing

    (_, x::xs) ->
      listGet (idx - 1) xs


listUnique : List a -> List a
listUnique items =
  let
    go soFar items =
      case items of
        [] ->
          soFar

        (x::xs) ->
          case listFind x soFar of
            Just _ ->
              go soFar xs

            Nothing ->
              go (x::soFar) xs
  in
    go [] items
