{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Jake (
    module J,
    jakeMain
) where

import Projection.Types

import           Prelude hiding ((.), id, until)
import           Urza as U hiding (fill, stroke)
import           Control.Concurrent
import           Control.Monad as M
import           Control.Wire hiding (loop)
import           System.Exit
import           Render
import           Text.Printf

ProjectionMain :: IO ()
ProjectionMain = do
    wvar <- initUrza (0, 0) (600, 600) "Projection"
    shdr <- makeShaderProgram
    bmp  <- spriteBitmap

    let renderGame' = renderGame jr sprites
        ienv        = def :: InputEnv
        genv        = GameEnv { geTime = 0
                              , geDelta = 1/60
                              , geDeltaAcc = 0
                              }
        env         = Env { inputEnv = ienv
                          , gameEnv = genv
                          }

    mainLoop wvar renderApp clockTiming gameState env game

mainLoop :: WindowVar
         -> Render Game
         -> Timing IO
         -> App ()
         -> Env
         -> Game
         -> IO ()
mainLoop wvar render timing app' env g = do
    -- Let GLFW-b load up our input events
    pollEvents
    (events, window) <- takeMVar wvar
    putMVar wvar ([], window)

    -- Make the rendering context current.
    makeContextCurrent $ Just window
    -- Process an event into the environment.
    let inputEnv' = foldl foldInput (inputEnv env) events
    (dt', timing') <- stepTiming timing
    let genv      = gameEnv env
        acc       = dt' + (geDeltaAcc genv)
        delta     = geDelta genv
        gameEnv'  = genv{ geDeltaAcc = acc'
                        , geTime = dt' + (geTime genv)
                        }
        env'      = Env { gameEnv = gameEnv', inputEnv = inputEnv' }
        Consumption g' env'' acc' = consume app' g env' acc delta

    g'' <- render g'

    -- GLFW-b housekeeping.
    swapBuffers window
    shouldClose <- windowShouldClose window
    M.when shouldClose exitSuccess

    mainLoop wvar render timing' app' env'' g''


consume :: App () -> Game -> Env -> Double -> Double -> Consumption
consume app' g e acc d =
    let g'   = execApp app' e g
        acc' = acc - d
        i    = inputEnv e
        e'   = e{ inputEnv = i{ _ienvEvents = [] }}
    in if (acc >= d)
         then consume app' g' e' acc' d
         else Consumption g e acc
