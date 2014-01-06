import Graphics.Input as Input
import Mouse
import Dict
import Window

import open Types
import open Ai
import open Board
import open Draw
import open Signals

-- Main part
main = lift2 draw Window.dimensions gameLoop

gameLoop : Signal Game
gameLoop = foldp update (Turn (Own, MustMove), startField) clickedOn

update : Maybe Point -> Game -> Game
update click (state, board) =
  let old = (state, board)
  in case (click, state) of
    (_, Won player) -> old
    (_, Turn (Enemy, _)) -> nextStep (enemyClick old) old
    (Nothing, _) -> old    
    (Just click, Turn (Own, _)) -> nextStep click old

nextStep : Point -> Game -> Game
nextStep click (state, board) =
  let old = (state, board)    
  in case state of
    Turn (player, MustMove) -> 
      if owner board click == Just player && canMove board click
      then (Turn (player, FromMove click), board) else old
    Turn (player, FromMove from) ->
      if canMoveTo board from click 
      then newGame moved old from click else old    
    Turn (player, MustJump) ->
      if owner board click == Just player && canJump board [] click
      then (Turn (player, FromJump [] click), board) else old
    Turn (player, FromJump over from) ->
      if canJumpTo board over from click
      then newGame jumped old from click else old

newGame f (state, board) from to =
  let newBoard = f board from to |> makeGenerals
  in (newState (state, newBoard) to, newBoard)

newState : Game -> Point -> State
newState (Turn (player, action), board) click =
  let otherPnts = Dict.toList board |>
                     filter (\(pnt, pillar) -> owner board pnt == Just (other player)) |>
                     map (\(pnt, pillar) -> pnt)
      otherMove = if | any (canJump board []) otherPnts -> Turn (other player, MustJump)
                     | any (canMove board) otherPnts -> Turn (other player, MustMove)
                     | otherwise -> Won player
  in case action of
    FromJump over fromJump ->
      if newPlace click fromJump |> canJump board (click::over)
      then Turn (player, FromJump (click::over) (newPlace click fromJump))
      else otherMove
    FromMove _ -> otherMove     
