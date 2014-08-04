module Jake.State where

import Jake.Game
import Jake.Types
import Types
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import Urza.Input.Event
import Urza.Input.Types
import Linear hiding (trace)
import Debug.Trace

gameState :: JakeApp ()
gameState = do
    on windowResizeEvent $ \(WindowSizeEvent w h) ->
        modify $ \g -> g{ _gWindow = V2 w h }

    let moveLeft  _ = updateJakeMovement $ (id, (+ V2 (-80) 0), id)
        moveRight _ = updateJakeMovement $ (id, (+ V2 80 0), id)
        restart   _ = modify $ \g -> g{ _gJake = jakeToon }

    on (keyDownEvent Key'Right) moveRight
    on (keyUpEvent Key'Right) moveLeft

    on (keyDownEvent Key'Left) moveLeft
    on (keyUpEvent Key'Left) moveRight

    on (keyDownEvent Key'R) restart

    progressJake
    collideJake


collideJake :: JakeApp ()
collideJake = do
    correction <- gets correctJakesMovement
    updateJakeMovement correction


progressJake :: JakeApp ()
progressJake = do
    jake <- gets _gJake
    dt   <- timeDelta
    let (av, vv, _) = _jMovement jake
        av' = gravity
        vv' = (dt *) <$> av
        pv' = (dt *) <$> vv
    updateJakeMovement ((+ av'), (+ vv'), (+ pv'))

gravity :: V2 Double
gravity = V2 0 9

timeDelta :: JakeApp Double
timeDelta = asks $ aeDelta . appEnv

updateJakeMovement :: MovementCorrection -> JakeApp ()
updateJakeMovement (a,v,p) = do
    jake <- gets _gJake
    let (av, vv, pv) = _jMovement jake
        mv           = (a av, v vv, p pv)
    modify $ \g -> g{ _gJake = jake{ _jMovement = mv }}

getJakesMovement :: JakeApp Movement
getJakesMovement = gets (_jMovement . _gJake)

keyDownEvent :: Key -> [InputEvent] -> Maybe InputEvent
keyDownEvent = keyEvent KeyState'Pressed

keyUpEvent :: Key -> [InputEvent] -> Maybe InputEvent
keyUpEvent = keyEvent KeyState'Released

keyEvent :: KeyState -> Key -> [InputEvent] -> Maybe InputEvent
keyEvent state key evs = do
    ev <- getKeyEvent evs
    case ev of
        (KeyEvent key' _ state' _) -> if key' == key && state == state'
                                             then return ev
                                             else fail ""
        _ -> fail ""


windowResizeEvent :: [InputEvent] -> Maybe InputEvent
windowResizeEvent = getWindowSizeEvent

cursorMoveEvent :: [InputEvent] -> Maybe InputEvent
cursorMoveEvent = getCursorMoveEvent

on :: ([InputEvent] -> Maybe InputEvent) -> (InputEvent -> JakeApp ()) -> JakeApp ()
on f g = do
    mEv <- asks $ (f . _ienvEvents . inputEnv)
    case mEv of
        Just ev -> g ev
        Nothing -> return ()

