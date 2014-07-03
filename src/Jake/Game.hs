module Jake.Game where

import           Urza as U hiding (fill, stroke)
import           Urza.Data.QTree as Q
import           Data.List as L
--import           Control.Wire
import           System.Directory
import           System.FilePath ((</>))
import           Linear hiding (trace)
import           Jake.Types
import           Debug.Trace


getCollision :: Game -> Maybe (V2 Double, Tile)
getCollision g =
    case queryPoint jp qt of
        []   -> trace "no collision" Nothing
        cols -> trace (show $ collision cols) collision cols
        where collision = Just . head . {-sortBy smaller . -}map (toSep jp)
              qt = makeQuadTree $ _gTileMap g
              toSep :: V2 Double -> (BoundingBox, a) -> (V2 Double, a)
              toSep p (bb,t) = (separatingAxis p bb, t)
              (jp,_,_) = _jMovement $ _gJake g
              smaller (v1,_) (v2,_) = magnitude v1 `compare` magnitude v2


separatingAxis :: V2 Double -> BoundingBox -> V2 Double
separatingAxis (V2 px py) r =
    let x = px - U.left r
        y = py - U.top r
    in if abs x < abs y then V2 x 0 else V2 0 y


magnitude :: V2 Double -> Double
magnitude (V2 x y) = sqrt (x*x) + (y*y)


makeQuadTree :: TileMap -> QTree (BoundingBox, Tile)
makeQuadTree = foldl accQTree (Q.empty bnds) . filter notEmpty . concatStamp
    where accQTree qt (x,y,t) = Q.insert (bb x y) (bb x y, t) qt
          bb x y = makeBox $ V2 (fromIntegral x) (fromIntegral y)
          bnds = Rectangle 0 0 500 500 :: BoundingBox
          notEmpty :: (a, a, Tile) -> Bool
          notEmpty (_,_,Empty) = False
          notEmpty _           = True


makeBox :: V2 Double -> BoundingBox
makeBox (V2 x y) = Rectangle (x*tw) (y*th) tw th
    where V2 tw th = tileSize


distanceTo :: (Double, Double) -> (Double, Double) -> Double
distanceTo (x,y) (z,w) = sqrt $ dx * dx + dy * dy
    where dx = z - x
          dy = w - y

-- | Concats a 2d array, 'stamping' each item with its (x,y) index.
concatStamp :: Num i => [[a]] -> [(i,i,a)]
concatStamp = snd . foldl foldy (0,[])
    where foldx j (i,xs) x = (i+1, xs ++ [(i,j,x)])
          foldy (i,ys) y = (i+1, ys ++ snd (foldl (foldx i) (0,[]) y))

-- | Returns the tilemap's tiles that lie within the viewport, marked with
-- their (x,y) indices - along with the offset from the viewport's (x,y) to
-- the upper left tile's (x,y).
tilesInViewport :: TileMap -> BoundingBox -> ([(Double, Double, Tile)], (Double,Double))
tilesInViewport rows (Rectangle x y w h) =
    let unseenRows = floor (y/th)
        seenRows   = ceiling (h/th)
        unseenCols = floor (x/tw)
        seenCols   = ceiling (w/tw)
        offsety    = (y/th) - fromIntegral unseenRows
        offsetx    = (x/tw) - fromIntegral unseenCols
        rows'      = take seenRows $ drop unseenRows rows
        tiles'     = map (take seenCols . drop unseenCols) rows'
        V2 th tw   = tileSize
    in (concatStamp tiles', (offsetx,offsety))

spriteSetFrame :: Sprite -> Frame -> Sprite
spriteSetFrame (SpriteAnimation bmps t _) f = SpriteAnimation bmps t f
spriteSetFrame (SpriteSheet bmp boxs t _) f = SpriteSheet bmp boxs t f

spriteSetTfrm :: Sprite -> Transform2d Double -> Sprite
spriteSetTfrm (SpriteAnimation bmps _ f) t = SpriteAnimation bmps t f
spriteSetTfrm (SpriteSheet bmp boxs _ f) t = SpriteSheet bmp boxs t f

spriteFramesLength :: Sprite -> Int
spriteFramesLength (SpriteAnimation bmps _ _) = length bmps
spriteFramesLength (SpriteSheet _ boxs _ _) = length boxs

bitmapTfrm :: Bitmap -> Transform2d Double
bitmapTfrm (Bitmap _ (w,h)) = def{ _t2Size = s }
    where s = V2 (fromIntegral w) (fromIntegral h)

withSize :: V2 Double -> Transform2d Double -> Transform2d Double
withSize s tfrm = tfrm{ _t2Size = s }

tileSizedTfrm :: Transform2d Double
tileSizedTfrm = withSize (V2 40 40) def

spriteBitmap :: IO Bitmap
spriteBitmap = do
    assetDir <- fmap (</> "assets") getCurrentDirectory
    Just bmp <- loadBitmap $ assetDir </> "img" </> "jake-forest.png"
    return bmp

tilemap :: TileMap
tilemap = air ++ [ground]
    where air    = replicate 9 $ replicate 10 Empty
          ground = replicate 10 $ Tile 0

tileSize :: V2 Double
tileSize = V2 40 40

tileSprites :: Bitmap -> [Sprite]
tileSprites bmp =
    let grass  = SpriteSheet bmp [Rectangle 320 0 40 40] (withSize (V2 40 40) def) 0
        dirt   = SpriteSheet bmp [Rectangle 320 40 40 40] (withSize (V2 40 40) def) 0
    in [grass, dirt]

jakeRunningSprite :: Bitmap -> Sprite
jakeRunningSprite bmp = SpriteSheet bmp [f1,f2] tfrm 0
    where tfrm = withSize (V2 40 40) def
          f1 = Rectangle 0 0 40 40
          f2 = Rectangle 40 0 40 40

jakeChoppingSprite :: Bitmap -> Sprite
jakeChoppingSprite bmp = SpriteSheet bmp [f1,f2] tileSizedTfrm 0
    where f1 = Rectangle 80 0 40 40
          f2 = Rectangle 160 0 40 40

jakeTalkingSprite :: Bitmap -> Sprite
jakeTalkingSprite bmp = SpriteSheet bmp [f1,f2] tileSizedTfrm 0
    where f1 = Rectangle 0 0 40 40
          f2 = Rectangle 0 40 40 40

jakeToon :: Jake
jakeToon = Jake (JakeRunning 0) (V2 0 0, V2 0 0, V2 0 0)

jakeRenderer :: Bitmap -> ShaderProgram -> JakeRenderer
jakeRenderer bmp shdr =
    JakeRenderer { _jrShader = shdr
                 , _jrSpriteRunning  = jakeRunningSprite bmp
                 , _jrSpriteChopping = jakeChoppingSprite bmp
                 , _jrSpriteTalking  = jakeTalkingSprite bmp
                 }

game :: Game
game = Game jakeToon tilemap (V2 0 0)

