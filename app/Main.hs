module Main where

import Constants
import Control.Monad (unless, when)
import Data.String (fromString)
import Data.Word
import Foreign.C
import GameState
import Graph
import Pacman
import SDL
import SDL.Raw (Rect (Rect))

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

  renderPacman

  present renderer
  where
    renderPacman = do
      let pacman' = pacman gamestate
          graph' = graph gamestate

      renderNode graph'

      rendererDrawColor renderer $= V4 0 0 255 255
      fillRect renderer (Just $ Rectangle (P $ fmap truncate $ pos pacman') (V2 32 32))

    renderNode :: Graph Float -> IO ()
    renderNode (Graph (node : nodes)) = do
      case node of
        Node pos' n e s w -> do
          rendererDrawColor renderer $= V4 0 255 0 255
          fillRect renderer (Just $ Rectangle (P $ CInt . truncate <$> pos') (CInt . truncate <$> V2 tileWidth tileHeight))
          renderEdge pos' n
          renderEdge pos' e
          renderEdge pos' s
          renderEdge pos' w
          renderNode $ Graph nodes
        Void -> return ()
    renderNode (Graph []) = return ()

    renderEdge nodepos neighbour = do
      case neighbour of
        Node pos' _ _ _ _ -> do
          rendererDrawColor renderer $= V4 255 0 0 255
          drawLine renderer (P $ CInt . truncate <$> center nodepos) (P $ CInt . truncate <$> center pos')
        Void -> return ()

    center v = v + V2 (tileWidth / 2) (tileHeight / 2)
