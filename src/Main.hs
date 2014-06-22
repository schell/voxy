{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Prelude hiding ((.), id, until)
import           Urza as U hiding (fill, stroke)
import           FRP.Netwire hiding (app, when)
import           Control.Monad
import           Control.Wire hiding (when)
import qualified Urza.Color as Color
import           Graphics.Rendering.OpenGL hiding (Matrix, renderer, get, drawPixels, Bitmap, imageHeight)
import           System.Directory
import           System.FilePath ((</>))
import           System.Exit
import           Types
import           Render
import           Codec.Picture
import           Codec.Picture.Types
import           Graphics.Rasterific
import           Graphics.Rasterific.Texture
import           Graphics.Text.TrueType
import           Data.Maybe
import qualified Data.Text as T

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type Frame = Int

data Sprite = SpriteAnimation [Bitmap] Transform2d Frame
            | SpriteSheet Bitmap [Rectangle Int] Transform2d Frame
            deriving (Show, Eq)

data Tile = Empty | Tile Int deriving (Eq, Ord, Show)

data TileMap = TileMap { _tmIndices  :: [[Tile]]
                       , _tmSprites  :: [Sprite]
                       , _tmViewport :: BoundingBox
                       , _tmTileSize :: (Double, Double)
                       } deriving (Show, Eq)

data GameEnv = GameEnv { _gameMap :: Double }


-------------------------------------------------------------------------------
-- Renders
-------------------------------------------------------------------------------

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

renderTileMap :: ShaderProgram -> TileMap -> IO TileMap
renderTileMap shdr tm@(TileMap rows sprites (Rectangle x y w h) (tw,th)) = do
    let unseenRows = floor (y/th)
        seenRows   = ceiling (h/th)
        unseenCols = floor (x/tw)
        seenCols   = ceiling (w/tw)
        offsety    = (y/th) - fromIntegral unseenRows
        offsetx    = (x/tw) - fromIntegral unseenCols
        rows'      = take seenRows $ drop unseenRows rows
        tiles'     = map (take seenCols . drop unseenCols) rows'
    forM_ (zip [0..fromIntegral $ length tiles' -1] tiles') $ \(y', row) ->
        forM_ (zip [0..fromIntegral $ length row -1] row) $ \(x', tile) -> case tile of
            Empty  -> return ()
            Tile i -> do let s  = sprites !! i
                             t  = def{ _t2Size = (tw,th)
                                     , _t2Position = (x'*tw - offsetx, y'*th - offsety)
                                     }
                             s' = spriteSetTfrm s t
                         void $ renderSprite shdr s'
    return tm



-------------------------------------------------------------------------------
-- Wires
-------------------------------------------------------------------------------

windowWire :: (InputWire a a) -> InputWire (AppWindow a) (AppWindow a)
windowWire wire = appWire' <|> pass
    where appWire' = proc (AppWindow _ bg app) -> do
                         s'   <- windowSize -< ()
                         app' <- wire -< app
                         returnA -< AppWindow s' bg app'

withSPF :: (Fractional t, RealFrac t) => t -> InputWire Sprite Sprite
withSPF fps = proc s -> do
    t <- timeF -< ()
    returnA -< spriteSetFrame s $ floor (t / fps) `mod` spriteFramesLength s

-------------------------------------------------------------------------------
-- Other
-------------------------------------------------------------------------------

spriteSetFrame :: Sprite -> Frame -> Sprite
spriteSetFrame (SpriteAnimation bmps t _) f = SpriteAnimation bmps t f
spriteSetFrame (SpriteSheet bmp boxs t _) f = SpriteSheet bmp boxs t f

spriteSetTfrm :: Sprite -> Transform2d -> Sprite
spriteSetTfrm (SpriteAnimation bmps _ f) t = SpriteAnimation bmps t f
spriteSetTfrm (SpriteSheet bmp boxs _ f) t = SpriteSheet bmp boxs t f

spriteFramesLength :: Sprite -> Int
spriteFramesLength (SpriteAnimation bmps _ _) = length bmps
spriteFramesLength (SpriteSheet _ boxs _ _) = length boxs

bitmapTfrm :: Bitmap -> Transform2d
bitmapTfrm (Bitmap _ (w,h)) = def{ _t2Size = s }
    where s = (fromIntegral w, fromIntegral h)

withSize :: (Double, Double) -> Transform2d -> Transform2d
withSize s tfrm = tfrm{ _t2Size = s }

staticRGBA8 :: DynamicImage -> Maybe (Image PixelRGBA8)
staticRGBA8 (ImageY8 img)     = Just $ promoteImage img
staticRGBA8 (ImageYA8 img)    = Just $ promoteImage img
staticRGBA8 (ImageRGB8 img)   = Just $ promoteImage img
staticRGBA8 (ImageRGBA8 img)  = Just img
staticRGBA8 (ImageYCbCr8 img) = Just $ promoteImage (convertImage img :: Image PixelRGB8)
staticRGBA8 _                 = Nothing

spriteBitmap :: IO Bitmap
spriteBitmap = do
    assetDir <- fmap (</> "assets") getCurrentDirectory
    Just bmp <- loadBitmap $ assetDir </> "img" </> "jake-forest.png"
    return bmp

sprite :: IO Sprite
sprite = do
    bmp <- spriteBitmap
    let tfrm = def{ _t2Size = (22, 38) }
        f1    = Rectangle 10 42 22 38
        f2    = Rectangle 50 42 22 38
    return $ SpriteSheet bmp [f1,f2] tfrm 0

tilemap :: IO TileMap
tilemap = do
    bmp <- spriteBitmap
    let ground = replicate 10 (Tile 0)
        dirt   = SpriteSheet bmp [Rectangle 320 40 40 40] (withSize (40,40) def) 0
    return $ TileMap { _tmIndices  = (replicate 9 $ replicate 10 Empty) ++ [ground]
                     , _tmSprites  = [dirt]
                     , _tmViewport = Rectangle 0 0 400 400
                     , _tmTileSize = (40, 40)
                     }

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    --cache      <- buildCache
    --let fonts = enumerateFonts cache
    --forM_ fonts $ \(FontDescriptor fname (FontStyle bold ital)) ->
    --    putStrLn $ unwords [ T.unpack fname
    --                       , if bold then " bold" else ""
    --                       , if ital then " italics" else ""
    --                       ]
    urza       <- initUrza (0, 0) (400, 400) "FontyFruity"
    shdr       <- makeShaderProgram
    --let mFontp = findFontInCache cache $ FontDescriptor "Ubuntu Mono" $ FontStyle False False
    --when (isNothing mFontp) exitFailure

    --eFont <- loadFontFile $ fromJust mFontp
    --font <- case eFont of
    --    Left err   -> putStrLn err >> exitFailure
    --    Right font -> return font

    --s <- sprite
    t <- tilemap

    let wini = def :: WindowIteration ()
        iter = wini { _iRender = (renderAppWindow shdr) (renderTileMap shdr)
                    , _iWire   = windowWire pass
                    , _iProcessEv = processInputEnv
                    , _iData = Right $ AppWindow { _awSize = Size 400 400
                                                 , _awBackgroundColor = Color.white
                                                 , _awApp = t
                                                 }
                    }
    loopUrza urza iter

