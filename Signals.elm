module Signals where

import Mouse
import Window

import open Types
import open Draw
import open Board

clicked : Signal (Int, Int)
clicked = sampleOn Mouse.isClicked Mouse.position |> dropRepeats

transform : (Int, Int) -> (Int, Int) -> (Int, Int)
transform (wx, wy) (mx, my) =
  let xBorder = (wx - 400) `div` 2
      yBorder = (wy - 450) `div` 2
  in  (mx - xBorder - 25, my - yBorder + 25)

clickedOn : Signal (Maybe Point)
clickedOn = lift2 transform Window.dimensions clicked |>
            --keepIf (\(x,y) -> x > 0 && y > 0) |>
            lift (\(x,y) -> (7-(y `div` space), (x `div` space))) |>
            lift (\c -> if onBoard c then Just c else Nothing) |>
            lift3 (\no mouse click -> if mouse then click else no)
              (constant Nothing) Mouse.isClicked |>
            lift2 (\second click -> click) (every (2*second))

