module Types where

import Dict

type Game = (State, Board)

data State = Turn (Player, Action) | Won Player
data Action = MustMove | FromMove Point | MustJump | FromJump [Point] Point

type Stone =  (Player, Level)
data Player = Own | Enemy
data Level = Peasant | General

type Point = (Int, Int)
type Pillar =  [Stone]
type Board = Dict.Dict Point Pillar

other : Player -> Player
other p = case p of
  Own -> Enemy
  Enemy -> Own

