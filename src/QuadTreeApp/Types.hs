module QuadTreeApp.Types where

import           Prelude hiding ((.), id, until)
import           System.Random
import           Urza as U
import           Urza.Data.QTree as QT
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)


randomColors :: IO [Color4 Double]
randomColors = do
    a <- newStdGen
    b <- newStdGen
    c <- newStdGen
    let rs = randomRs (0, 1) a
        gs = randomRs (0, 1) b
        bs = randomRs (0, 1) c
    return $ zipWith3 (\r g b' -> Color4 r g b' 1) rs gs bs

newQTree :: AppTree
newQTree = QT.empty $ Rectangle 0 0 400 400


newApp :: [Color4 Double] -> App
newApp colors = App { appInputRect = zeroRect
                    , appTree = newQTree
                    , appColors = colors
                    , appCollisions = []
                    }

data App = App { appInputRect :: BoundingBox
               , appTree    :: QTree (BoundingBox, Color4 Double)
               , appColors  :: [Color4 Double]
               , appCollisions :: [(BoundingBox, Color4 Double)]
               } deriving (Show)

type AppTree = QTree (BoundingBox, Color4 Double)


