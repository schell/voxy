{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Jake (
    module J,
    jakeMain
) where

import Jake.Game as J
import Jake.Rendering as J
import Jake.Types as J
import Jake.Wires as J

import           Prelude hiding ((.), id, until)
import           Urza as U hiding (fill, stroke)
import           Control.Concurrent
import           Control.Monad as M
import           Control.Monad.Reader
import           Control.Wire hiding (loop)
import           Control.Wire.Core
import           System.Exit
import           Render

jakeMain :: IO ()
jakeMain = do
    wvar <- initUrza (0, 0) (600, 600) "Jake"
    shdr <- makeShaderProgram
    bmp  <- spriteBitmap

    let jr          = jakeRenderer bmp shdr
        sprites     = tileSprites bmp
        renderGame' = renderGame jr sprites
        renderApp   = renderGame'
        env         = def :: InputEnv

    mainLoop wvar renderApp env gameWire clockTiming game

mainLoop :: WindowVar
          -> Render Game
          -> InputEnv
          -> GameWire () Game
          -> Timing IO
          -> Game
          -> IO ()
mainLoop wvar render inputEnv wire timing edata = do
    -- Let GLFW-b load up our input events
    pollEvents
    (events, window) <- takeMVar wvar
    putMVar wvar ([], window)

    -- Make the rendering context current.
    makeContextCurrent $ Just window
    -- Process an event into the environment.
    let inputEnv' = foldl foldInput inputEnv events
    (dt, timing') <- stepTiming timing

    let Output edata' wire' = runReader (runReaderT (stepWire wire dt ()) inputEnv') edata
    edata'' <- render edata'

    -- GLFW-b housekeeping.
    swapBuffers window
    shouldClose <- windowShouldClose window
    M.when shouldClose exitSuccess

    -- Clean out the events so they don't just build up forever.
    let inputEnv'' = inputEnv'{ _ienvEvents = [] }
    mainLoop wvar render inputEnv'' wire' timing' edata''

