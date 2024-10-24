module Spritesheet where

import Foreign.C
import GHC.IO.FD (openFile)
import SDL
import System.Posix.Internals (fileType)

data Spritesheet = Spritesheet
  { texture :: Texture,
    textureInfo :: TextureInfo,
    spriteSize :: V2 CInt
  }

newtype Sprite = Sprite (Rectangle CInt)

loadSpritesheet :: V2 CInt -> Renderer -> String -> IO Spritesheet
loadSpritesheet spriteSize' renderer file = do
  surf <- loadBMP file
  texture' <- createTextureFromSurface renderer surf
  textureInfo' <- queryTexture texture'
  return $ Spritesheet texture' textureInfo' spriteSize'

getSprite :: [CInt] -> Spritesheet -> Sprite
getSprite is spritesheet =
  let info = textureInfo spritesheet

      textureSize = V2 (textureWidth info) (textureHeight info)
      spriteSize' = spriteSize spritesheet

      first = head is
      range = last is - head is

      xy = P $ textureSize `div` spriteSize' * (V2 first first)
      wh = spriteSize' * V2 range range
   in (Sprite $ Rectangle xy wh)
