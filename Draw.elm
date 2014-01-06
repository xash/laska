module Draw where

import Dict

import open Types
import open Board

boardSize = 400
space = (boardSize `div` 8)
clickSpace = boardSize `div` 7

stoneCircle : Stone -> Form
stoneCircle stone =
  let gb = dashed black
      gborder = circle 17 |> outlined { gb | width <- 5 }
      pb = solid black
      pborder = circle 18 |> outlined { pb | width <- 3 }
  in case stone of
  (Own, Peasant) -> group [ circle 20 |> filled red, pborder ]
  (Enemy, Peasant) -> group [ circle 20 |> filled green, pborder ]
  (Own, General) -> group [ circle 20 |> filled red, gborder ]
  (Enemy, General) -> group [ circle 20 |> filled green, gborder ]

drawPillar : Pillar -> [Form]
drawPillar pillar = map stoneCircle pillar |> reverse |>
                    zipWith (\y -> moveY (toFloat y*10)) [0..30]
drawBoardField : Form
drawBoardField = circle 30 |> filled brown

drawButton : State -> Board -> Point -> [Form]
drawButton state board pnt =
 case state of
   Won player ->
     []
   Turn (player, MustMove) ->
     if owner board pnt == Just player && canMove board pnt
     then [circle 35 |> filled blue] else []
   Turn (player, MustJump) ->
     if owner board pnt == Just player && canJump board [] pnt
     then [circle 35 |> filled red] else []
   Turn (player, FromMove from) ->
     if | canMoveTo board from pnt -> [circle 35 |> filled blue]
        | pnt == from -> [circle 35 |> filled yellow]
        | otherwise -> []
   Turn (player, FromJump over from) ->
     if | canJumpTo board over from pnt -> [circle 35 |> filled red]
        | pnt == from -> [circle 35 |> filled yellow]
        | otherwise -> []

drawField : State -> Board -> Point -> [Form]
drawField state board pnt =
 let pillar = Dict.findWithDefault [] pnt board
 in drawButton state board pnt ++ [drawBoardField] ++ drawPillar pillar

drawBoard : Game -> Element
drawBoard (state, board) =
 let drawPos x = x * space |> absolute
 in boardPoints |> reverse |>
    concatMap (\(x,y) -> drawField state board (x,y) |>
                   map (move (toFloat <| y * space, toFloat <| x * space)) |>
                   map (move (-150, -150))) |>
    collage 400 400

draw : (Int, Int) -> Game -> Element
draw (wx, wy) (state, board) =
 let drawnBoard = drawBoard (state, board)
     textElement = text <| bold <| toText <| case state of
       Won Own -> "You won!"
       Won Enemy -> "You lost!"
       Turn (Own, MustMove) -> "Choose which pillar to move."
       Turn (Own, FromMove _) -> "Choose its destination."
       Turn (Own, MustJump) -> "Choose which pillar shall attack."
       Turn (Own, FromJump _ _) -> "Choose its prey."
       Turn (Enemy, _) -> "Waiting for enemy."
       otherwise -> " "
 in  flow down [drawnBoard, container 400 50 middle textElement] |>
     container wx wy middle


