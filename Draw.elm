module Draw where

import Dict

import open Types
import open Board

boardSize = 400
space = (boardSize `div` 8)

stoneCircle : Stone -> Form
stoneCircle stone =
  let gb = dashed black
      gborder = circle 17 |> outlined { gb | width <- 5 }
      pb = solid black
      pborder = circle 18 |> outlined { pb | width <- 3 }
  in case stone of
  (Own, Peasant)   -> group [ circle 20 |> filled red, pborder ]
  (Enemy, Peasant) -> group [ circle 20 |> filled green, pborder ]
  (Own, General)   -> group [ circle 20 |> filled red, gborder ]
  (Enemy, General) -> group [ circle 20 |> filled green, gborder ]

drawPillar : Pillar -> Form
drawPillar p = p |> map stoneCircle 
                 |> reverse 
                 |> zipWith (\y -> moveY (toFloat y*10)) [0..30]
                 |> group

drawBoardField : Form
drawBoardField = circle 30 |> filled brown

drawButton : State -> Board -> Point -> Form
drawButton state b pnt = group <|
 case state of
   Won player ->
     []
   Turn (player, MustMove) ->
     if owner b pnt == Just player && canMove b pnt
     then [circle 35 |> filled blue] else []
   Turn (player, MustJump) ->
     if owner b pnt == Just player && canJump b [] pnt
     then [circle 35 |> filled red] else []
   Turn (player, FromMove from) ->
     if | canMoveTo b from pnt      -> [circle 35 |> filled blue]
        | pnt == from               -> [circle 35 |> filled yellow]
        | otherwise                 -> []
   Turn (player, FromJump over from) ->
     if | canJumpTo b over from pnt -> [circle 35 |> filled red]
        | pnt == from               -> [circle 35 |> filled yellow]
        | otherwise                 -> []

drawField : State -> Board -> Point -> Form
drawField state b pnt =
  group <| [drawButton state b pnt,
            drawBoardField,
            drawPillar (Dict.findWithDefault [] pnt b)]

drawBoard : Game -> Element
drawBoard (state, b) =
  let drawPnt x = toFloat <| space * x
  in boardPoints |> reverse
                 |> map (\(x,y) -> drawField state b (x,y)
                                |> move (drawPnt y, drawPnt x)
                                |> move (-150, -150))
                 |> collage 400 400

draw (wx, wy) (state, b) =
 let drawnBoard = drawBoard (state, b)
     textElement = text . bold . toText <| case state of
       Won Own                -> "You won!"
       Won Enemy              -> "You lost!"
       Turn (Enemy, _)        -> "Waiting for enemy."
       Turn (_, MustMove)     -> "Choose which pillar to move."
       Turn (_, FromMove _)   -> "Choose its destination."
       Turn (_, MustJump)     -> "Choose which pillar shall attack."
       Turn (_, FromJump _ _) -> "Choose its prey."
       otherwise              -> " "
 in  flow down [drawnBoard, container 400 50 middle textElement] |>
     container wx wy middle


