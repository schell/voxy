module Jake.Types where

import           Prelude hiding ((.), id, until)
import           Urza as U hiding (fill, stroke)
import           Linear
import           Control.Wire
import           Control.Monad.Reader
import           Control.Monad.Identity


type GameEnv = ReaderT InputEnv (ReaderT Game Identity)

type GameWire a b = Wire GameEnv a b

asksGame :: (MonadTrans t, MonadReader r m) => (r -> a) -> t m a
asksGame = lift . asks

runGame :: Reader r1 (Reader r a) -> r1 -> r -> a
runGame f inputEnv game = runReader (runReader f inputEnv) game

type Frame = Int

data Sprite = SpriteAnimation { _spriteBitmaps   :: [Bitmap]
                              , _spriteTransform :: Transform2d Double
                              , _spriteFrame     :: Frame
                              }
            | SpriteSheet { _spriteBitmap        :: Bitmap
                          , _spriteBoundingBoxes :: [Rectangle Int]
                          , _spriteTransform     :: Transform2d Double
                          , _spriteFrame         :: Frame
                          } deriving (Show, Eq)

data Tile = Empty | Tile Int deriving (Eq, Ord, Show)

type TileMap = [[Tile]]

-- | Jake's many states of being.
data JakeAction = JakeRunning Double
                | JakeChopping Double
                | JakeTalking String Double
                deriving (Show, Eq, Ord)

data Jake = Jake { _jAction   :: JakeAction
                 , _jMovement :: Movement
                 } deriving (Show)

type Movement = (Acceleration, Velocity, Position)

data JakeRenderer = JakeRenderer { _jrShader         :: ShaderProgram
                                 , _jrSpriteRunning  :: Sprite
                                 , _jrSpriteChopping :: Sprite
                                 , _jrSpriteTalking  :: Sprite
                                 }

data Game = Game { _gJake    :: Jake
                 , _gTileMap :: TileMap
                 , _gWindow  :: V2 Int
                 } deriving (Show)

type Acceleration = V2 Double
type Velocity = V2 Double
type Position = V2 Double


