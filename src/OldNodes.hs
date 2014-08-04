{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Nodes where

import Prelude hiding (id, (.))
import Urza
import Data.Maybe
import System.Directory
import Control.Monad.State
import FRP.Netwire hiding (when)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

mainNodes :: IO ()
mainNodes = do
    --urza <- initUrza (0,0) (400, 400) "Jake"
    --shdr <- makeShaderProgram
    let node = runNode root
    print node
-------------------------------------------------------------------------------
-- Nodes
-------------------------------------------------------------------------------

root :: Node MyData ()
root = do

    -- Getting user data.
    mData <- getData
    -- Setting data.
    when (isNothing mData) $ setData $ Just $ MyData "schell"
    -- Working with the user data without a State interface you supply
    -- modifyData with a function that modifies some user data. The user
    -- data is only modified if it exists, and the result of the
    -- modification is returned wrapped in Maybe.
    _ <- modifyData $ \(MyData name) -> MyData $ name ++ "san"

    -- Declaring the transform of the node.
    withTransform $ do
        setPosition 120 120
        setScale 1 1
        setRotation $ pi/3
    addChild $ childNode "child0"
    addChildren [childNode "child1", childNode "child2"]

getData :: Node a (Maybe a)
getData = fromData id

fromData :: (a -> b) -> Node a (Maybe b)
fromData f = gets (fmap f . _nodeData)

modifyData :: (a -> a) -> Node a (Maybe a)
modifyData f = do
    ma <- fromData f
    case ma of
        Just a  -> setData $ Just a
        Nothing -> return ()
    return ma

withData :: State a () -> Node a (Maybe a)
withData s = do
    ma <- fromData id
    case ma of
        Just a  -> let a' = Just (execState s a) in setData a' >> return a'
        Nothing -> return Nothing

setData :: Maybe a -> Node a ()
setData a = modify $ \n -> n{ _nodeData = a }

addChild :: Node a () -> Node a ()
addChild = addChildren . (:[])

addChildren :: [Node a ()] -> Node a ()
addChildren nodes = do
    children <- gets _nodeChildren
    modify $ \n -> n{ _nodeChildren = children ++ map runNode nodes }

childNode :: String -> Node MyData ()
childNode str = do
    withTransform $ do
        setPosition 50 50
    setData $ Just $ MyData str

setTransform :: Transform2d -> Node a ()
setTransform t = modify $ \n -> n{ _nodeTransform = t }

withTransform :: State Transform2d () -> Node a ()
withTransform st = do
   t <- gets _nodeTransform
   setTransform $ execState st t

setPosition :: Double -> Double -> State Transform2d ()
setPosition x y = modify $ \t -> t{ _t2Position = (x,y) }

setRotation :: Double -> State Transform2d ()
setRotation theta = modify $ \t -> t{ _t2Rotation = theta }

setScale :: Double -> Double -> State Transform2d ()
setScale x y = modify $ \t -> t{ _t2Scale = (x,y) }

runNode :: Node a () -> InternalNode a
runNode = flip execState emptyInternalNode

emptyInternalNode :: InternalNode a
emptyInternalNode =
   InternalNode { _nodeChildren = []
                , _nodeTransform = def
                , _nodeData = Nothing
                }

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type Node a = State (InternalNode a)

data InternalNode a =
    InternalNode { _nodeChildren  :: [InternalNode a]
                 , _nodeTransform :: Transform2d
                 , _nodeData      :: Maybe a
                 } deriving (Show)

data NodeWire = W

data MyData = MyData { myName :: String } deriving (Show)


