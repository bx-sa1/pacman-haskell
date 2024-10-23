module Main where

import Control.Monad (unless, when)
import Data.String (fromString)
import Data.Word
import GameState
import SDL

frametime :: Double
frametime = 1 / 60

main :: IO ()
main = do
  initializeAll

  window <- createWindow (fromString "Pacman") defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer

  now <- ticks
  gameloop now window renderer initialGameState

gameloop :: Word32 -> Window -> Renderer -> GameState -> IO ()
gameloop lasttime window renderer gamestate = do
  now <- ticks
  let delta = (fromIntegral now - fromIntegral lasttime) / 1000 :: Double

  events <- pollEvents
  let payloads = map eventPayload events

  let newgamestate = update delta $ foldl processInput gamestate payloads
  render renderer newgamestate

  when (delta < frametime) $ delay (truncate $ (frametime - delta) * 1000)

  unless (gamestate == Quit) (gameloop now window renderer newgamestate)

render :: Renderer -> GameState -> IO ()
render renderer gamestate = do
  rendererDrawColor renderer $= V4 0 255 255 255
  clear renderer

  present renderer
