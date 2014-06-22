module QuadTreeApp.Render where

import           Types
import           Prelude hiding ((.), id, until)
import           Urza as U
import           Urza.Data.QTree as QT
import           FRP.Netwire hiding (app)
import           Control.Lens hiding ((#), at)
import           Control.Monad.Reader hiding (when)
import qualified Control.Monad as M
import qualified Urza.Color as Color
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)

renderApp :: Renderer -> App -> IO ()
renderApp r (App cur qt _ cols) = do
    renderSelectionRect r cur
    renderQTree r qt
    forM_ (leaves qt) $ \(QLeaf b (_, c)) -> do
        fillPath_ r $ do
            setColor $ alpha c 0.5
            uncurryRectangle rectangleAt b
        strokePath_ r $ do
            setColor c
            uncurryRectangle rectangleAt b
        r^.shader.setTextColor $ Color4 0 0 0 0.8
        drawTextAt' r (Position (round $ U.left b) (round $ U.top b)) $ show b
    forM_ cols $ \(b, _) -> do
        strokePath_ r $ do
            setColor Color.red
            uncurryRectangle rectangleAt b

renderSelectionRect :: Renderer -> BoundingBox -> IO ()
renderSelectionRect r bb = do
    M.when (areaOf bb > 0) $ fillPath_ r $ do
        setColor $ Color.white
        uncurryRectangle rectangleAt bb

renderQTree :: Renderer -> QTree a -> IO ()
renderQTree r (QTree bb mbs _) = do
    fillPath_ r $ do
        setColor $ Color4 1 1 1 0.3
        uncurryRectangle rectangleAt bb
    strokePath_ r $ do
        setColor $ Color4 0 0 0 1
        uncurryRectangle rectangleAt bb
    case mbs of
        Nothing -> return ()
        Just (a,b,c,d) -> do
            forM_ [a,b,c,d] $ renderQTree r

