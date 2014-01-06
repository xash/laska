module Board where

import Dict

import open Types
import open Helper

afterJump (x,y) (a,b) = (a+(a-x), b+(b-y))

boardPoints : [Point]
boardPoints = concatMap (\x -> map (\y -> (x,y)) [0..6]) [0..6] |>
              filter (\(x,y) -> isEven (x+y))

onBoard : Point -> Bool
onBoard pnt = pnt `elem` boardPoints

startField : Board
startField = 
  zip (take 11 boardPoints ++ drop 14 boardPoints)
      (repeat 11 [(Own, Peasant)] ++ repeat 11 [(Enemy, Peasant)])
  |> Dict.fromList

owner : Board -> Point -> Maybe Player
owner b pnt =
 case Dict.lookup pnt b of
   Nothing -> Nothing
   Just pillar -> Just (head pillar |> fst)

directions : Board -> Point -> [Point]
directions board pnt = filter onBoard <| map (addTuple pnt) <|
  case Dict.lookup pnt board
  of Just ((Own, Peasant)::s)   -> [(1,-1), (1,1)]
     Just ((Enemy, Peasant)::s) -> [(-1,-1), (-1,1)]
     Just ((_, General)::s)     -> [(1,-1), (1,1), (-1,-1), (-1,1)]
     otherwise -> []

canMoveTo : Board -> Point -> Point -> Bool
canMoveTo b from to = to `elem` directions b from &&
                      not (Dict.member to b)

canJumpTo : Board -> [Point] -> Point -> Point -> Bool
canJumpTo b jumped from to =
  let ownPillar = Dict.findWithDefault [] from b
  in  not (to `elem` jumped) &&
      to `elem` directions b from &&
      onBoard (afterJump from to) &&
      canMoveTo (Dict.insert to ownPillar b) to (afterJump from to) &&
      case (owner b from, owner b to) of
        (Just Own, Just Enemy) -> True
        (Just Enemy, Just Own) -> True
        otherwise -> False

canMove : Board -> Point -> Bool
canMove b from =
  directions b from |> any (canMoveTo b from)

canJump : Board -> [Point] -> Point -> Bool
canJump b jumped from =
  directions b from |> any (canJumpTo b jumped from)

moved : Board -> Point -> Point -> Board
moved b from to =
  case Dict.lookup from b of
    Nothing     -> b
    Just pillar -> b |> Dict.remove from 
                     |> Dict.insert to pillar

jumped : Board -> Point -> Point -> Board
jumped b from over =
  case (Dict.lookup from b, Dict.lookup over b)  of
    (Just fromPillar, Just overPillar) -> b
      |> Dict.remove from
      |> Dict.remove over
      |> Dict.insert (afterJump from over) (fromPillar ++ [head overPillar]) 
      |> if length overPillar > 1
         then Dict.insert over (tail overPillar)
         else id
    otherwise -> b

makeGenerals : Board -> Board
makeGenerals b =
  Dict.foldl (\pnt ((player,_)::ps) -> 
    if (player == Own && pnt `elem` [(6,0),(6,2),(6,4),(6,6)]) ||
       (player == Enemy && pnt `elem` [(0,0),(0,2),(0,4),(0,6)])
    then Dict.insert pnt ((player, General)::ps)
    else id) b b
