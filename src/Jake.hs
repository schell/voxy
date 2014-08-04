{-# LANGUAGE OverloadedStrings #-}
module Jake (
    module J,
    jakeMain
) where

import Jake.Game as J
import Jake.Rendering as J
import Jake.Types as J
import Jake.State as J

import           Urza as U hiding (fill, stroke)
import           Types
import           Core

jakeMain :: IO ()
jakeMain = do
    wvar <- initUrza (0, 0) (600, 600) "Jake"
    shdr <- makeShaderProgram
    bmp  <- spriteBitmap

    let jr          = jakeRenderer bmp shdr
        sprites     = tileSprites bmp
        renderGame' = renderGame jr sprites
        renderApp   = renderGame'
        ienv        = def :: InputEnv
        aenv        = AppEnv { aeTime = 0
                             , aeDelta = 1/60
                             , aeDeltaAcc = 0
                             }
        env         = Env { inputEnv = ienv
                          , appEnv = aenv
                          }

    mainLoop wvar renderApp gameState env game




