module Projection.Rendering where

import Projection.Types
import Render


renderApp :: ShaderProgram -> Window -> Render Window
renderApp shdr w@(Window (V2 w h)) = do
    renderViewport shdr w h white
    return w
