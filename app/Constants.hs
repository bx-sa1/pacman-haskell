module Constants where

import Foreign.C

data Direction = N | E | S | W deriving (Eq, Show)

isOpposite :: Direction -> Direction -> Bool
isOpposite N S = True
isOpposite S N = True
isOpposite E W = True
isOpposite W E = True
isOpposite _ _ = False

cint2cdouble :: CInt -> CDouble
cint2cdouble = fromRational . toRational

tileWidth = 32 :: Float

tileHeight = 32 :: Float

pacmanMoveSpeed :: Float
pacmanMoveSpeed = 10
