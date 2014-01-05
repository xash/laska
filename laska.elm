import Graphics.Input as Input
import Mouse
import Dict

main = lift3 (\(state, board) a b-> flow down [drawBoard (state, board), asText state, asText a, asText b]) currentGame clickedOn (every second)

currentGame : Signal Game
currentGame = foldp update (Turn (Own, MustMove), startField) clickedOn

update : Maybe Point -> Game -> Game
update pnt (state, board) =
  let old = (state, board)
  in case (pnt, state) of         
    (_,  Turn (Enemy, _)) -> makeMove (enemyClick old) old
    (Nothing, _) -> old    
    (Just click, Turn (Own, _)) -> makeMove click old

makeMove : Point -> Game -> Game
makeMove click (state, board) =
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

makeGenerals : Board -> Board
makeGenerals board =
  Dict.toList board |>
  filter (\(pnt, pillar) -> 
    (owner board pnt == Just Own && pnt `elem` [(6,0),(6,2),(6,4),(6,6)]) ||
    (owner board pnt == Just Enemy && pnt `elem` [(0,0),(0,2),(0,4),(0,6)])) |>
  map (\(pnt, p::ps) -> case p of
    (player, Peasant) -> (pnt,(player, General)::ps)
    otherwise -> (pnt,p::ps)) |>
  foldl (\(pnt, pillar) -> Dict.insert pnt pillar) board
  --Dict.fromList
  

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
      if newPlace click fromJump |> canJump board over
      then Turn (player, FromJump (click::over) (newPlace click fromJump))
      else otherMove
    FromMove _ -> otherMove     
    

-- Obvious things

type Game = (State, Board)

data State = Turn (Player, Action) | Won Player
data Action = MustMove | FromMove Point | MustJump | FromJump [Point] Point

type Stone =  (Player, Level)
data Player = Own | Enemy
data Level = Peasant | General

type Point = (Int, Int)
type Pillar =  [Stone]
type Board = Dict.Dict Point Pillar

owner : Board -> Point -> Maybe Player
owner board pnt = 
 case Dict.lookup pnt board of
   Nothing -> Nothing
   Just pillar -> Just (head pillar |> fst)

boardPoints : [Point]
boardPoints = 
  let points = concatMap (\x -> map (\y -> (x,y)) [0..6]) [0..6]
  in filter (\(x,y) -> isEven (x+y)) points
  
onBoard : Point -> Bool
onBoard pnt = pnt `elem` boardPoints

other : Player -> Player
other p = case p of
  Own -> Enemy
  Enemy -> Own

startField : Board
startField = (map (\pnt -> (pnt, [ (Own, Peasant)])) (take 11 boardPoints) ++
             map (\pnt -> (pnt, [ (Enemy, Peasant)])) (drop 14 boardPoints))|>
             Dict.fromList   

-- Helper functions

isEven x = x `mod` 2 == 0
addTuple (x,y) (a,b) = (x+a,y+b)
subTuple (x,y) (a,b) = (x-a,y-b)
elem x a = any (\y -> x == y) a
find : (a -> Bool) -> [a] -> Maybe a
find f a = let filtered = filter f a in if isEmpty filtered then Nothing else Just (head filtered)
newPlace (x,y) (a,b) = (x+(x-a),y+(y-b))
     
-- Draw things
boardSize = 400
space = (boardSize `div` 8)
fSpace = toFloat space

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
drawButton (Turn (player, action)) board pnt =
 case action of
   MustMove -> if owner board pnt == Just player && canMove board pnt
               then [circle 35 |> filled blue] else []
   MustJump -> if owner board pnt == Just player && canJump board [] pnt
               then [circle 35 |> filled red] else []
   FromMove from -> if | canMoveTo board from pnt -> [circle 35 |> filled blue]
                       | pnt == from -> [circle 35 |> filled yellow]
                       | otherwise -> []
   FromJump over from -> if | canJumpTo board over from pnt -> [circle 35 |> filled red]
                            | pnt == from -> [circle 35 |> filled yellow]
                            | otherwise -> []
                    
drawField : State -> Board -> Point -> [Form]
drawField state board pnt =
 let pillar = Dict.findWithDefault [] pnt board
 in drawButton state board pnt ++ [drawBoardField] ++ drawPillar pillar

drawBoard : Game -> Element
drawBoard (state, board) = 
 let drawPos x = x * space |> absolute
 in boardPoints |>
    concatMap (\(x,y) -> drawField state board (x,y) |> 
                   map (move (toFloat <| y * space, toFloat <| x * space)) |>
                   map (move (-155, -155))) |>
    collage 370 370
  
--Calculate ingame things

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
      canMoveTo (Dict.insert to ownPillar board) to (newPlace to pnt) && 
      case (owner board pnt, owner board to) of
        (_, Nothing) -> False
        (Nothing, _) -> False
        (Just Own, Just Enemy) -> True
        (Just Enemy, Just Own) -> True
        otherwise -> False

canMove : Board -> Point -> Bool
canMove board pnt = 
  directions board pnt |> filter (canMoveTo board pnt) |> isEmpty |> not

canJump : Board -> [Point] -> Point -> Bool
canJump board over pnt = 
  directions board pnt |> filter (canJumpTo board over pnt) |> isEmpty |> not

mustJump : Board -> Player -> Bool
mustJump board player =
  Dict.toList board |>
  filter (\(pnt, pillar) -> fst (head pillar) == player && canJump board [] pnt) |>
  isEmpty |> not

mustMove : Board -> Player -> Bool
mustMove board player =
  Dict.toList board |>
  filter (\(pnt, pillar) -> fst (head pillar) == player && canMove board pnt) |>
  isEmpty |> not

moved : Board -> Point -> Point -> Board
moved board pnt to =
  case Dict.lookup pnt board of
    Nothing -> board
    Just pillar -> board |> Dict.insert to pillar |> Dict.remove pnt
  
jumped : Board -> Point -> Point -> Board
jumped board from over =
  let to = newPlace over from
  in
      case (Dict.lookup from board, Dict.lookup over board)  of
        (_, Nothing) -> board
        (Nothing, _) -> board
        (Just fromPillar, Just overPillar) ->
          Dict.insert to (fromPillar ++ [head overPillar]) board |>
          Dict.remove from |>
          Dict.remove over |>
          (\dict -> if length overPillar > 1  
            then Dict.insert over (tail overPillar) dict
            else dict)    

--Signals  
clickedOn : Signal (Maybe Point)
clickedOn = sampleOn Mouse.isClicked Mouse.position |> dropRepeats |>
            lift (\(x,y) -> Just (7-((y+30) `div` 50), x `div` 50)) |>
            keepIf (\a -> case a of Just (x,y) -> onBoard (x,y)) Nothing |>
            lift3 (\no mouse click -> if mouse then click else no) 
              (constant Nothing) Mouse.isClicked |>
            lift2 (\second click -> click) (every second)
            
--AI            
enemyClick : Game -> Point
enemyClick (state, board) =
  let pnts = Dict.toList board |>
             filter (\(pnt, pillar) -> owner board pnt == Just Enemy) |>
             map (\(pnt, pillar) -> pnt)
  in case state of
    Turn (Enemy, MustMove) ->
      head <| filter (canMove board) pnts
    Turn (Enemy, FromMove from) ->
      head <| filter (canMoveTo board from) <| directions board from
    Turn (Enemy, MustJump) ->
      head <| filter (canJump board []) pnts
    Turn (Enemy, FromJump over from) ->
      head <| filter (canJumpTo board over from) <| directions board from
