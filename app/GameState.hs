module GameState where

import SDL hiding (Paused)

data GameState
  = Running
      {
      }
  | Paused
  | Menu
  | Quit
  deriving (Eq, Show)

initialGameState :: GameState
initialGameState = Menu

update :: Double -> GameState -> GameState
update dt Menu = Running
update dt gamestate = gamestate

processInput :: GameState -> EventPayload -> GameState
processInput _ QuitEvent = Quit
processInput gamestate (KeyboardEvent kbdata) = case kbdata of
  KeyboardEventData _ Pressed _ keysym -> processStateInput (keysymKeycode keysym) gamestate
  _ -> gamestate
processInput gamestate _ = gamestate

processStateInput :: Keycode -> GameState -> GameState
processStateInput keycode gamestate@Running = case keycode of
  KeycodeEscape -> Paused
  _ -> gamestate
processStateInput keycode gamestate@Paused = case keycode of
  _ -> gamestate
processStateInput keycode gamestate@Menu = case keycode of
  _ -> gamestate
processStateInput _ gamestate = gamestate
