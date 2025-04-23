{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Stream.Monadic.Instances () where

import Control.Exception
import Control.Monad.IO.Class
import Data.Stream.Monadic qualified as VS
import Servant.API
import Servant.Types.SourceT

instance (MonadIO m) => FromSourceIO a (VS.Stream m a) where
  fromSourceIO src = unSourceT src $ pure . VS.Stream go
    where
      go Stop = pure VS.Done
      go (Error err) = liftIO $ fail err
      go (Skip s1) = pure $ VS.Skip s1
      go (Yield x s1) = pure $ VS.Yield x s1
      go (Effect m) = go =<< liftIO m

instance ToSourceIO a (VS.Stream IO a) where
  toSourceIO (VS.Stream step s) = fromStepT $ go s
    where
      go s0 = Effect $ do
        r <- step s0
        evaluate $ case r of
          VS.Yield x s1 -> Yield x $ go s1
          VS.Skip s1 -> Skip $ go s1
          VS.Done -> Stop
