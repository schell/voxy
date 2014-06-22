module Types where

import           Prelude hiding ((.), id, until)
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap)


data AppWindow a = AppWindow { _awSize            :: Size
                             , _awBackgroundColor :: Color4 GLfloat
                             , _awApp             :: a
                             }

type Render a = Either () a -> IO (Either () a)

