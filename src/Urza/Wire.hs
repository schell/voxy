{-# LANGUAGE FlexibleContexts #-}
module Urza.Wire where

import           Prelude hiding ((.), id, until)
import           Types
import           Urza
import           FRP.Netwire
import           Control.Wire
import           Control.Wire.Unsafe.Event
import           Control.Monad.Reader hiding (when)
import           Data.Maybe
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
import           Control.Lens hiding ((#), at)


-- | Processes individual events into the input environment.
processEnv :: Maybe InputEvent -> Env -> Env
processEnv mE@(Just (CursorMoveEvent x y)) env =
    env & envLastCursorPos .~ (x,y) & envEvent .~ mE
processEnv mE@(Just (CursorEnterEvent cs)) env =
    env & envCursorOnScreen .~ (cs == CursorState'InWindow) & envEvent .~ mE
processEnv mE env = env & envEvent .~ mE

step :: (Monoid s) => Session IO s -> UrzaWire s b -> Env -> IO (Session IO s, UrzaWire s b, Either () b)
step session wire env
    | isJust $ env^.envEvent = do
        (session', wire', _) <- stepEvent session wire env
        stepTime session' wire' env
    | otherwise = stepTime session wire env

stepTime :: Session IO s -> UrzaWire s b -> Env -> IO (Session IO s, UrzaWire s b, Either () b)
stepTime session wire env = do
    (ds, session')  <- stepSession session
    (mx, wire') <- runReaderT (stepWire wire ds $ Right ()) env
    return (session', wire', mx)

stepEvent :: (Monoid s) => t -> UrzaWire s b -> Env -> IO (t, UrzaWire s b, Either () b)
stepEvent session wire env = do
    print env
    (mx, wire') <- runReaderT (stepWire wire mempty $ Right ()) $ env
    return (session, wire', mx)

keyEvent :: MonadReader Env m => Key -> KeyState -> Wire s e m a (Event a)
keyEvent key kstate = mkGen_ $ \a -> do
    mEv <- asks _envEvent
    return $ Right $ case mEv of
        Just (KeyEvent k _ ks _) -> if k == key && ks == kstate then Event a else NoEvent
        _ -> NoEvent

cursorMoveEvent :: MonadReader Env m => Wire s e m a (Event (Double, Double))
cursorMoveEvent = mkGen_ $ \_ -> do
    mEv <- asks _envEvent
    return $ Right $ case mEv of
        Just (CursorMoveEvent x y) -> Event (x, y)
        _ -> NoEvent

mouseButtonEvent :: MonadReader Env m => MouseButton -> MouseButtonState -> Wire s e m a (Event (Double, Double))
mouseButtonEvent mbutton mstate = mkGen_ $ \_ -> do
    mEv   <- asks _envEvent
    (x,y) <- asks _envLastCursorPos
    return $ Right $ case mEv of
        Just (MouseButtonEvent mbutton' mstate' _) ->
            if mbutton == mbutton' && mstate == mstate'
              then Event (x,y)
              else NoEvent
        _ -> NoEvent

cursorEnterEvent :: MonadReader Env m => CursorState -> Wire s e m a (Event a)
cursorEnterEvent cState = mkGen_ $ \a -> do
    mEv <- asks _envEvent
    return $ Right $ case mEv of
        Just (CursorEnterEvent s) -> if cState == s then Event a else NoEvent
        _ -> NoEvent

-- | Produces `a` when the cursor is on the screen, otherwise it inhibits.
whenCursorIsOnScreen :: (Monoid e) => MonadReader Env m => Wire s e m a a
whenCursorIsOnScreen = mkGen_ $ \a -> do
    cOs <- asks _envCursorOnScreen
    return $ if cOs then Right a else Left mempty

cursorPositionStartingWith :: (MonadReader Env m, HasTime t s, Monoid e, Fractional t) => Position -> Wire s e m a Position
cursorPositionStartingWith pos = cursor2Pos . asSoonAs . cursorMoveEvent <|> (pure pos)
    where cursor2Pos = arr $ \(x, y) -> Position (round x) (round y)


