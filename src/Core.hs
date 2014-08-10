module Core where

import           Urza as U hiding (fill, stroke)
import           Types

import           Control.Wire hiding (loop)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Identity
import           Control.Monad as M
import           Control.Concurrent
import           System.Exit


-- | The main GLFW-b loop.
mainLoop :: WindowVar
         -> Render a
         -> App a ()
         -> Env
         -> a
         -> IO ()
mainLoop = mainLoop' clockTiming

mainLoop' :: Timing IO
          -> WindowVar
          -> Render a
          -> App a ()
          -> Env
          -> a
          -> IO ()
mainLoop' timing wvar render app' env g = do
    -- Let GLFW-b load up our input events
    pollEvents
    (events, window) <- takeMVar wvar
    putMVar wvar ([], window)

    -- Make the rendering context current.
    makeContextCurrent $ Just window
    -- Process an event into the environment.
    let inputEnv' = foldl foldInput (inputEnv env) events
    (dt', timing') <- stepTiming timing
    let aenv      = appEnv env
        acc       = dt' + (aeDeltaAcc aenv)
        aenv'     = aenv{ aeDeltaAcc = acc + dt' }
        env'      = env{ inputEnv = inputEnv', appEnv = aenv' }
        Consumption g' env'' = consume app' g env'

    g'' <- render g'

    -- GLFW-b housekeeping.
    swapBuffers window
    shouldClose <- windowShouldClose window
    M.when shouldClose exitSuccess

    mainLoop' timing' wvar render app' env'' g''

-- | Consumes time from an environment and iterates an App a b until no
-- more time steps can be made.
-- Inspired by http://gafferongames.com/game-physics/fix-your-timestep/
consume :: App a () -> a -> Env -> Consumption a
consume app' g e =
    let g'   = execApp app' e g
        a    = appEnv e
        acc  = aeDeltaAcc a
        t    = aeTime a
        dt   = aeDelta a
        acc' = acc - dt
        i    = (inputEnv e){ _ienvEvents = [] }
        a'   = a `withAETime` (dt + t) `withAEDeltaAcc` acc'
        e'   = e `withInputEnv` i `withAppEnv` a'
    in if (acc >= dt)
         -- Go another round.
         then consume app' g' e'
         -- Return the final state of the app.
         else Consumption g e

-- | Executes an app's state using an environment and app object.
execApp :: App a b -> Env -> a -> a
execApp app' env a =
    let Identity (_,a') = runApp app' env a
    in a'

-- | Unwraps the App monad stack to iterate an App a b.
runApp :: App a b -> Env -> a -> Identity (b, a)
runApp app' env a = runStateT (runReaderT (runA app') env) a


