module QuadTreeApp.Render where

import           Prelude hiding ((.), id, until)
import           Urza as U
import           Urza.Data.QTree as QT
--import           Control.Lens hiding ((#), at)
import           Control.Monad.Reader hiding (when)
--import qualified Control.Monad as M
--import qualified Urza.Color as Color
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
{-
renderApp :: Renderer -> App -> IO ()
renderApp r (App cur qt _ cols) = do
    renderSelectionRect r cur
    renderQTree r qt
    forM_ (leaves qt) $ \(QLeaf b (_, c)) -> do
        fillPath_ r $ do
            setColor $ alpha c 0.5
            uncurryRectangle rectangle b
        strokePath_ r $ do
            setColor c
            uncurryRectangle rectangle b
        r^.shader.setTextColor $ Color4 0 0 0 0.8
        drawTextAt' r (Position (round $ U.left b) (round $ U.top b)) $ show b
    forM_ cols $ \(b, _) -> do
        strokePath_ r $ do
            setColor Color.red
            uncurryRectangle rectangle b

renderSelectionRect :: Renderer -> BoundingBox -> IO ()
renderSelectionRect r bb = do
    M.when (areaOf bb > 0) $ fillPath_ r $ do
        setColor $ Color.white
        uncurryRectangle rectangle bb
-}

renderQTree :: ShaderProgram -> QTree a -> IO ()
renderQTree s (QTree bb mbs lvs) = do
    fillPath_ s $ do
        setColor $ Color4 1 1 1 0.3
        rectangle bb
    strokePath_ s $ do
        setColor $ Color4 0 0 0 1
        rectangle bb
    case mbs of
        Nothing -> return ()
        Just (a,b,c,d) -> do
            forM_ [a,b,c,d] $ renderQTree s
    forM_ lvs $ \leaf -> strokePath_ s $ do
        setColor $ Color4 0 1 0 1
        rectangle $ _qlBounds leaf

