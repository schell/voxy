{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graph where

import Urza
import Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
import Data.Maybe

class Default a where
    def :: a

instance Show (IO Bitmap) where
    show _ = "IO Bitmap"

data Graph = Graph { _graphPosition  :: Position
                   , _graphSize      :: Size
                   , _graphScale     :: Scale
                   , _graphRotation  :: Rotation
                   , _graphChildren  :: [Graph]
                   , _graphCache     :: Maybe Bitmap
                   , _graphRasterize :: IO Bitmap
                   } deriving (Show)


instance Default Graph where
    def = Graph (Position 0 0) (Size 0 0) (Scale 1 1) (Rotation 0) [] Nothing emptyBitmap

graphFromFile :: FilePath -> IO (Maybe Graph)
graphFromFile fp = do
   mBmp <- loadBitmap fp
   return $ case mBmp of
       Nothing -> Nothing
       Just b@(Bitmap _ s) -> Just $ def { _graphSize = s
                                         , _graphCache = Just b
                                         , _graphRasterize = fmap fromJust $ loadBitmap fp
                                         }


drawGraph :: Renderer -> Graph -> IO ()
drawGraph r (Graph pos sz scl rot _ cache raster) = do
    bmp <- case cache of
        Just bmp -> return bmp
        Nothing  -> raster
    drawBitmap r bmp pos sz scl rot


infixl 8 #
-- | Postfix function application, for conveniently applying
--   attributes.  Unlike @($)@, @(#)@ has a high precedence (8), so @d
--   \# foo \# bar@ can be combined with other things using operators
--   like @(|||)@ or @(\<\>)@ without needing parentheses.
(#) :: a -> (a -> b) -> b
(#) = flip ($)
