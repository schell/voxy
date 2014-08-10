{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Time.Clock
import Control.Applicative
import Control.Monad.Identity
import System.Exit
import System.Environment
import Wire.Core

main :: IO ()
main = do
    s:_  <- getArgs
    t    <- getCurrentTime
    let timeVaryingInt = (constWire 45 ~> at 3) `startingWith` 666
        timeVaryingStr = (constWire "." ~> at 4) `startingWith` "Running..."
    loop (read s) t $ (,) <$> timeVaryingInt <*> timeVaryingStr

loop :: Time -> UTCTime -> Wire Identity () (Int, String) -> IO ()
loop tleft t w = do
    t' <- getCurrentTime
    let dt = realToFrac $ diffUTCTime t' t
        Identity (Output val w') = stepWire w dt ()
    print val
    let tleft' = tleft - dt
    if tleft' <= 0
      then exitSuccess
      else loop tleft' t' w'

