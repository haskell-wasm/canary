module Data.Stream.Monadic.Extra
  ( toLazyList,
    toVector,
  )
where

import Control.Monad
import Control.Monad.Primitive
import Data.Stream.Monadic qualified as VS
import Data.Vector.Fusion.Bundle.Monadic qualified as VB
import Data.Vector.Fusion.Bundle.Size qualified as VB
import Data.Vector.Generic qualified as V
import Data.Vector.Generic.Mutable qualified as MV

toLazyList :: (PrimBase m) => VS.Stream m a -> m [a]
toLazyList (VS.Stream step s) = go s
  where
    go s0 = unsafeInterleave $ do
      r <- step s0
      case r of
        VS.Yield x s1 -> do
          xs <- go s1
          pure $ x : xs
        VS.Skip s1 -> go s1
        VS.Done -> pure []

toVector :: (PrimMonad m, V.Vector v a) => VS.Stream m a -> m (v a)
toVector = V.unsafeFreeze <=< MV.munstream . flip VB.fromStream VB.Unknown
