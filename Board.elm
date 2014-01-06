module Board where

import Dict

import open Types
import open Helper

boardPoints : [Point]
boardPoints =
  let points = concatMap (\x -> map (\y -> (x,y)) [0..6]) [0..6]
  in filter (\(x,y) -> isEven (x+y)) points

onBoard : Point -> Bool
onBoard pnt = pnt `elem` boardPoints

startField : Board
startField = (map (\pnt -> (pnt, [ (Own, Peasant)])) (take 11 boardPoints) ++
             map (\pnt -> (pnt, [ (Enemy, Peasant)])) (drop 14 boardPoints)) |>
             Dict.fromList

directions : Board -> Point -> [Point]
directions board pnt = map (addTuple pnt) <|
  case Dict.lookup pnt board
  of Nothing -> []
     Just ((Own, Peasant)::s) -> [(1,-1), (1,1)]
     Just ((Enemy, Peasant)::s) -> [(-1,-1),(-1,1)]
     Just ((_, General)::s) -> [(1,-1), (1,1), (-1,-1),(-1,1)]

canMoveTo : Board -> Point -> Point -> Bool
canMoveTo board pnt to = (to `elem` (directions board pnt)) &&
                         onBoard to && (not <| Dict.member to board)

canJumpTo : Board -> [Point] -> Point -> Point -> Bool
canJumpTo board over pnt to =
  let ownPillar = Dict.findWithDefault [] pnt board
  in  (not <| to `elem` over) &&
      (to `elem` (directions board pnt)) &&
      onBoard to && onBoard (newPlace to pnt) &&
      canMoveTo (Dict.insert to ownPillar board) to (newPlace to pnt) &&
      (case (owner board pnt, owner board to) of
        (_, Nothing) -> False
        (Nothing, _) -> False
        (Just Own, Just Enemy) -> True
        (Just Enemy, Just Own) -> True
        otherwise -> False)

canMove : Board -> Point -> Bool
canMove board pnt =
  directions board pnt |> any (canMoveTo board pnt)

canJump : Board -> [Point] -> Point -> Bool
canJump board over pnt =
  directions board pnt |> any (canJumpTo board over pnt)

moved : Board -> Point -> Point -> Board
moved board pnt to =
  case Dict.lookup pnt board of
    Nothing -> board
    Just pillar -> Dict.remove pnt board |> Dict.insert to pillar

jumped : Board -> Point -> Point -> Board
jumped board from over =
  let to = newPlace over from
  in
      case (Dict.lookup from board, Dict.lookup over board)  of
        (_, Nothing) -> board
        (Nothing, _) -> board
        (Just fromPillar, Just overPillar) ->
          Dict.remove from board |>
          Dict.remove over |>
          Dict.insert to (fromPillar ++ [head overPillar]) |>
          (\dict -> if length overPillar > 1
            then Dict.insert over (tail overPillar) dict
            else dict)

makeGenerals : Board -> Board
makeGenerals board =
  Dict.toList board |>
  filter (\(pnt, pillar) ->
    (owner board pnt == Just Own && pnt `elem` [(6,0),(6,2),(6,4),(6,6)]) ||
    (owner board pnt == Just Enemy && pnt `elem` [(0,0),(0,2),(0,4),(0,6)])) |>
  map (\(pnt, (player,_)::ps) -> (pnt,(player,General)::ps)) |>
  foldr (\(pnt, pillar) b ->
         (Dict.insert pnt pillar <| Dict.remove pnt b)) board


