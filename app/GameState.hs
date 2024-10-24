module GameState where

import Constants
import Foreign.C
import Graph
import Pacman
import SDL hiding (E, Paused)

data GameState
  = Running
      { pacman :: Pacman,
        graph :: Graph Float
      }
  | Paused GameState
  | Menu
  | Quit
  deriving (Eq)

initialGameState :: GameState
initialGameState = Menu

update :: Double -> GameState -> GameState
update dt Menu =
  Running {pacman = initPacman nodeA, graph = graph'}
  where
    nodeA = Node (V2 0 0) Void nodeB nodeC Void
    nodeB = Node (V2 (tileWidth * 2) 0) Void Void Void nodeA
    nodeC = Node (V2 0 (tileHeight * 4)) nodeA Void Void Void
    graph' = Graph [nodeA, nodeB, nodeC]
update dt running@(Running pacman' graph') =
  running {pacman = move dt pacman'}
update dt gamestate = gamestate

processInput :: GameState -> EventPayload -> GameState
processInput _ QuitEvent = Quit
processInput gamestate (KeyboardEvent kbdata) = case kbdata of
  KeyboardEventData _ Pressed _ keysym -> processStateInput (keysymKeycode keysym) gamestate
  _ -> gamestate
processInput gamestate _ = gamestate

processStateInput :: Keycode -> GameState -> GameState
processStateInput keycode gamestate@(Running _ _) = case keycode of
  KeycodeEscape -> Paused gamestate
  KeycodeW -> gamestate {pacman = setTarget (M N) (pacman gamestate)}
  KeycodeD -> gamestate {pacman = setTarget (M E) (pacman gamestate)}
  KeycodeS -> gamestate {pacman = setTarget (M S) (pacman gamestate)}
  KeycodeA -> gamestate {pacman = setTarget (M W) (pacman gamestate)}
  _ -> gamestate
processStateInput keycode gamestate@(Paused _) = case keycode of
  _ -> gamestate
processStateInput keycode gamestate@Menu = case keycode of
  _ -> gamestate
processStateInput _ gamestate = gamestate
