module Ai where

import Dict

import open Types
import open Board

enemyClick : Game -> Point
enemyClick (state, board) =
  let pnts = Dict.toList board |>
             map fst |>
             filter (\pnt -> owner board pnt == Just Enemy)
  in case state of
    Turn (Enemy, MustMove) ->
      head <| filter (canMove board) pnts
    Turn (Enemy, FromMove from) ->
      head <| filter (canMoveTo board from) <| directions board from
    Turn (Enemy, MustJump) ->
      head <| filter (canJump board []) pnts
    Turn (Enemy, FromJump over from) ->
      head <| filter (canJumpTo board over from) <| directions board from
    Won _ -> (0,0)

