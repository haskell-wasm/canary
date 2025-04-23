module Control.Concurrent.Async.Extra
  ( concurrentlyR,
  )
where

import Control.Concurrent.Async

concurrentlyR :: IO a -> IO b -> IO b
concurrentlyR l r = do
  (_, b) <- concurrently l r
  pure b
