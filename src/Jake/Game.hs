module Jake.Game where

import           Urza as U hiding (fill, stroke)
import           Urza.Data.QTree as Q
--import           Control.Wire
import           System.Directory
import           System.FilePath ((</>))
import           Linear hiding (trace)
import           Jake.Types
import           Debug.Trace
import           Data.Maybe
import           Data.List as L

-- | Given the current state of the game, find the current correct position
-- for Jake.
correctJakesMovement :: Game -> MovementCorrection
correctJakesMovement g = do
    let jp = getJakesPosition g
        tm = _gTileMap g
        mCol = getCollision jp tm
    case mCol of
        Nothing    -> (id,id,id)
        Just (b,_) -> do
            let b' = expandRect b $ V2 0.1 0.1
                vs = map (jp -) $ sortByMagnitude $ projectionVectors jp $ b'
            case findNonCollisionPoints vs tm of
                []  -> (id,id,id)
                p:_ -> jp `correctionTo` p

correctionTo :: Position -> Position -> MovementCorrection
correctionTo p p' =
    let dp@(V2 dx dy) = p - p'
        ux  = if abs dx > 0 then 0 else 1
        uy  = if abs dy > 0 then 0 else 1
        u   = V2 ux uy
    in ((* u), (* u), (+ (-1 * dp)))

findNonCollisionPoints :: [V2 Double] -> TileMap -> [V2 Double]
findNonCollisionPoints vs tm =
    let cs = map (`getCollision` tm) vs
        zs = zip cs vs
        fs = map snd $ filter (isNothing . fst) zs
    in fs

getCollision :: V2 Double -> TileMap -> Maybe (BoundingBox, Tile)
getCollision p = getCollisionWith (pointOnRect p)

getCollisionWith :: (BoundingBox -> Bool) -> TileMap -> Maybe (BoundingBox, Tile)
getCollisionWith f = listToMaybe . queryWith f . makeQuadTree

getJakesPosition :: Game -> V2 Double
getJakesPosition g = p
    where (_,_,p) = _jMovement $ _gJake g

jakesPosition :: Game -> V2 Double
jakesPosition = getJakesPosition


separate :: V2 Double -> BoundingBox -> V2 Double
separate p r = p - smallestProjectionVector p r


smallestProjectionVector :: V2 Double -> BoundingBox -> V2 Double
smallestProjectionVector v r =
    let v':_ = sortByMagnitude $ projectionVectors v r
    in v'

-- | Sorts a list of vectors by magnitude in ascending order.
sortByMagnitude :: [V2 Double] -> [V2 Double]
sortByMagnitude vs = sortBy (\a b' -> magnitude a `compare` magnitude b') vs

-- | Gives a list of all four possible projections along each separating axis.
projectionVectors :: V2 Double -> BoundingBox -> [V2 Double]
projectionVectors (V2 x y) r =
    let t  = V2 0             (y - top r)
        l  = V2 (x - left r)  0
        b  = V2 0             (y - bottom r)
        r' = V2 (x - right r) 0
    in [t,l,b,r']



distanceTo :: (Double, Double) -> (Double, Double) -> Double
distanceTo (x,y) (z,w) = sqrt $ dx * dx + dy * dy
    where dx = z - x
          dy = w - y


magnitude :: V2 Double -> Double
magnitude (V2 x y) = sqrt ((x*x) + (y*y))


makeQuadTree :: TileMap -> QTree (BoundingBox, Tile)
makeQuadTree = foldl accQTree (Q.empty bnds) . filter notEmpty . concatStamp
    where accQTree qt (x,y,t) = Q.insert (bb x y) (bb x y, t) qt
          bb x y = makeBox (V2 x y)
          bnds = Rectangle 0 0 500 500 :: BoundingBox
          notEmpty :: (a, a, Tile) -> Bool
          notEmpty (_,_,Empty) = False
          notEmpty _           = True


makeBox :: V2 Double -> BoundingBox
makeBox (V2 x y) = Rectangle (x*tw) (y*th) tw th
    where V2 tw th = tileSize


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
tilemap = air ++ [ground, uground]
    where air     = replicate 8 $ replicate 10 Empty
          ground  = [Tile 0] ++ replicate 8 Empty ++ [Tile 0]
          uground = replicate 10 $ Tile 0

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
jakeToon = Jake (JakeRunning 0) (V2 0 0, V2 0 0, V2 100 100)

jakeRenderer :: Bitmap -> ShaderProgram -> JakeRenderer
jakeRenderer bmp shdr =
    JakeRenderer { _jrShader = shdr
                 , _jrSpriteRunning  = jakeRunningSprite bmp
                 , _jrSpriteChopping = jakeChoppingSprite bmp
                 , _jrSpriteTalking  = jakeTalkingSprite bmp
                 }

game :: Game
game = Game jakeToon tilemap (V2 0 0)

