module Pacman where

import Constants
import Foreign.C
import GHC.Float (double2Float, double2Int)
import Graph
import SDL hiding (E)

data MoveDirection = M Direction | Stop deriving (Eq, Show)

getDirection :: MoveDirection -> Direction
getDirection (M d) = d
getDirection Stop = undefined

data Pacman = Pacman
  { -- sprite :: Sprite,
    moveDirection :: MoveDirection,
    targetNode :: Node Float,
    currentNode :: Node Float,
    pos :: V2 Float
  }
  deriving (Eq)

initPacman :: Node Float -> Pacman
initPacman node =
  Pacman
    { moveDirection = Stop,
      targetNode = Void,
      currentNode = node,
      pos = nodePos node
    }

move :: Double -> Pacman -> Pacman
move delta pacman = case moveDirection pacman of
  Stop -> pacman
  _ -> checkShouldStop $ movePacman
  where
    targetNode' = targetNode pacman
    currentNode' = currentNode pacman
    pos' = pos pacman

    directionToVec d = case d of
      M dir -> case dir of
        N -> V2 0 (-1)
        E -> V2 1 0
        S -> V2 0 1
        W -> V2 (-1) 0
      Stop -> V2 0 0

    overshootCheck =
      (>=)
        (distance pos' (nodePos currentNode'))
        (nodeDistance (targetNode', currentNode'))

    checkShouldStop newPos =
      if overshootCheck
        then stop $ pacman {pos = nodePos targetNode'}
        else pacman {pos = newPos}
    movePacman = pos' + (directionToVec (moveDirection pacman)) * (V2 pacmanMoveSpeed pacmanMoveSpeed) * (double2Float <$> V2 delta delta)

stop :: Pacman -> Pacman
stop pacman = pacman {moveDirection = Stop, currentNode = targetNode pacman}

setTarget :: MoveDirection -> Pacman -> Pacman
setTarget Stop pacman = stop pacman
setTarget moveDirection'@(M dir) pacman
  | moveDirection pacman == Stop = resolve dir pacman
  | isOpposite (getDirection $ moveDirection pacman) dir = resolve dir (stop pacman)
  | otherwise = pacman
  where
    resolve dir' pacman' =
      case getAdjacent (currentNode pacman') dir' of
        Void -> pacman'
        node@(Node {}) ->
          pacman' {moveDirection = moveDirection', targetNode = node}
