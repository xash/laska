module Ai where

import Dict

import open Types
import open Board

enemyClick : Game -> Point
enemyClick (state, b) =
  let pnts = b |> Dict.keys
               |> filter (\pnt -> owner b pnt == Just Enemy)
  in case state of
    Turn (Enemy, MustMove) ->
      head <| filter (canMove b) pnts
    Turn (Enemy, FromMove from) ->
      head <| filter (canMoveTo b from) <| directions b from
    Turn (Enemy, MustJump) ->
      head <| filter (canJump b []) pnts
    Turn (Enemy, FromJump jumped from) ->
      head <| filter (canJumpTo b jumped from) <| directions b from
    otherwise -> (0,0)
