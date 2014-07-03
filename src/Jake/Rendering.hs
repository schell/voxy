module Jake.Rendering where

import           Prelude hiding ((.), id, until)
import           Urza as U hiding (fill, stroke)
import           Urza.Data.QTree
import           Control.Monad
import           Control.Monad.State
import           Linear
import           Jake.Types
import           Jake.Game
import           Render
import           QuadTreeApp.Render


renderGame :: JakeRenderer -> [Sprite] -> Render Game
renderGame jr sprites g@(Game jake tiles (V2 w h)) = do
    renderViewport (_jrShader jr) w h white

    let shdr      = _jrShader jr
        tree      = makeQuadTree tiles
        (_,_,p)   = _jMovement jake
        cols      = queryPoint p tree 

    void $ renderTileMap shdr sprites tiles

    _setModelview shdr eye4
    renderQTree shdr tree

    forM_ cols $ \(b,_) -> strokePath_ shdr $ do
                               setColor red 
                               uncurryRectangle rectangleAt b

    void $ renderJake jr jake
    return g


renderTileMap :: ShaderProgram -> [Sprite] -> TileMap -> IO TileMap
renderTileMap shdr sprites tm = do
    let V2 tw th  = tileSize
    forM_ (concatStamp tm) $ \(x', y', tile) ->
        case tile of
            Empty  -> return ()
            Tile i -> do let s  = sprites !! i
                             t  = def{ _t2Size = tileSize
                                     , _t2Position = V2 (x'*tw) (y'*th)
                                     }
                             s' = spriteSetTfrm s t
                         void $ renderSprite shdr s'
    return tm

renderJake :: JakeRenderer -> Jake -> IO Jake
renderJake jr (Jake action avp@(V2 _ _, V2 _ _, pos)) = do
    let setPos p = do t <- gets _spriteTransform
                      modify $ \s -> s{ _spriteTransform = t{ _t2Position = p}}
        --setFrame _ = modify $ \s -> s{ _spriteFrame = 0 }
        getSprite f = execState (setPos pos) (f jr)
        running  = getSprite _jrSpriteRunning
        chopping = getSprite _jrSpriteChopping
        talking  = getSprite _jrSpriteTalking
        shdr     = _jrShader jr
    case action of
        JakeRunning _   -> void $ renderSprite shdr running
        JakeChopping _  -> void $ renderSprite shdr chopping
        JakeTalking _ _ -> void $ renderSprite shdr talking

    return $ Jake action avp


renderSprite :: ShaderProgram -> Sprite -> IO Sprite
renderSprite shdr s@(SpriteAnimation bmps tfrm frame) = do
    when (frame < length bmps) $ do
        drawTexture shdr (_bitmapTexture $ bmps !! frame) tfrm
    return s
renderSprite shdr s@(SpriteSheet bmp boxes tfrm frame) = do
    when (frame < length boxes) $ do
        let bb = boxes !! frame
        drawBitmapPixels shdr bmp bb tfrm
    return s




metersToPixels :: Double -> Double
metersToPixels = (*40)


