{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Prelude
import Urza
import Data.Maybe
import System.Directory
import Control.Monad.State
--import FRP.Netwire hiding (when)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    --urza <- initUrza (0,0) (400, 400) "Jake"
    --shdr <- makeShaderProgram

    print "boom"
-------------------------------------------------------------------------------
-- Nodes
-------------------------------------------------------------------------------

updateGraph :: e -> Graph e a -> Graph e a
updateGraph e (n, s) = (updateNode e n, s)

runGraph :: Graph e a -> Graph e a
runGraph (n, s) = (runNode n s, s)

runNode :: INode e a -> NodeState e a () -> INode e a
runNode n s = (execState s n){ nodeChildren = map (\(n',s') -> (runNode n' s', s')) (nodeChildren n) }

updateNode :: e -> INode e a -> INode e a
updateNode e n = (execState (nodeUpdate n $ e) n)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type Graph e a = (INode e a, NodeState e a ())

type NodeState e a = State (INode e a)

data INode e a = INode { nodeChildren :: [Graph e a]
                       , nodeData     :: Maybe a
                       , nodeUpdate   :: e -> NodeState e a ()
                       }
