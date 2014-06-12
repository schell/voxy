module Types where

import           Prelude hiding ((.), id, until)
import           Urza as U
import qualified Urza.Color as Color
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)
import           QTree


newtype ColorButton = ColorButton (Color4 Double) deriving (Eq)

newtype SizeButton = SizeButton Int deriving (Eq)

data Stage = Stage { _sColors :: [ColorButton]
                   , _sSizes  :: [SizeButton]
                   }

data AppWindow a = AppWindow { _awSize            :: Size
                             , _awBackgroundColor :: Color4 GLfloat
                             , _awApp             :: a
                             }

data App = App { appInputRect :: BoundingBox
               , appTree    :: QTree (String, Color4 Double)
               , appColors  :: [Color4 Double]
               } deriving (Show)

type Render a = Renderer -> Either () a -> IO (Either () a)


stage :: Stage
stage = Stage { _sColors = [ ColorButton Color.black
                           , ColorButton Color.white
                           , ColorButton Color.red
                           , ColorButton Color.green
                           , ColorButton Color.blue
                           , ColorButton Color.yellow
                           ]
              , _sSizes  = [ SizeButton 1
                           , SizeButton 2
                           , SizeButton 4
                           , SizeButton 8
                           , SizeButton 16
                           ]
              }

