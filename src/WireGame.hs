{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module WireGame where

import           Prelude hiding ((.), id, until)
import           Urza as U hiding (fill, stroke)
import           Urza.Color as Color
import           FRP.Netwire hiding (app, when)
import           Control.Monad
import           Control.Monad.State
import           System.Directory
import           System.FilePath ((</>))
import           Types
import           Codec.Picture
import           Codec.Picture.Types
import           Render
import           Linear
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap, imageHeight, Position)
import           Jake

goGame :: IO ()
goGame = do
    urza       <- initUrza (0, 0) (400, 400) "Jake"
    shdr       <- makeShaderProgram
    bmp        <- spriteBitmap

    let jr   = jakeRenderer bmp shdr
        wini = def :: WindowIteration ()
        sprites = tileSprites bmp
        iter = wini { _iRender = (renderAppWindow shdr) (renderGame jr sprites)
                    , _iWire   = windowWire gameWire
                    , _iProcessEv = processInputEnv
                    }
    loopUrza urza iter $ Right $ app bmp


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Renders
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Wires
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Other

