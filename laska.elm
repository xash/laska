import Graphics.Input as Input
import Mouse
import Dict
import Window

import open Types
import open Ai
import open Board
import open Draw
import open Signals
import open Helper

main = lift2 draw Window.dimensions gameLoop

gameLoop : Signal Game
gameLoop = foldp update (Turn (Own, MustMove), startField) clickedOn

update : Maybe Point -> Game -> Game
update click (state, b) =
  let old = (state, b)
  in case (click, state) of
    (_, Turn (Enemy, _))        -> nextStep (enemyClick old) old
    (Just click, Turn (Own, _)) -> nextStep click old
    otherwise -> old

nextStep : Point -> Game -> Game
nextStep click (state, b) =
  let old = (state, b)    
  in case state of
    Turn (p, MustMove) -> 
      if owner b click == Just p && canMove b click
      then (Turn (p, FromMove click), b) else old
    Turn (p, FromMove from) ->
        if | canMoveTo b from click -> 
             newGame moved old from click 
           | owner b click == Just p && canMove b click ->
             (Turn (p, FromMove click), b)
           | otherwise -> 
             old
    Turn (p, MustJump) ->
      if owner b click == Just p && canJump b [] click
      then (Turn (p, FromJump [] click), b) else old
    Turn (p, FromJump over from) ->
      if | canJumpTo b over from click ->
             newGame jumped old from click
         | isEmpty over && owner b click == Just p && canJump b [] click ->
             (Turn (p, FromMove click), b)
         | otherwise ->
             old

newGame f (state, b) from to =
  let newBoard = f b from to |> makeGenerals
  in (newState (state, newBoard) to, newBoard)

newState : Game -> Point -> State
newState (Turn (p, action), b) click =
  let otherPnts = Dict.keys b |>
                  filter (\pnt -> owner b pnt == Just (other p))
      otherMove = if | any (canJump b []) otherPnts -> Turn (other p, MustJump)
                     | any (canMove b) otherPnts    -> Turn (other p, MustMove)
                     | otherwise                    -> Won p
  in case action of
    FromJump jumped from ->
      if afterJump from click |> canJump b (click::jumped)
      then Turn (p, FromJump (click::jumped) (afterJump from click))
      else otherMove
    FromMove _ -> otherMove     
