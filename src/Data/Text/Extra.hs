{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}

module Data.Text.Extra
  ( unsafeCreate,
    unsafeCreateUpToN,
    unsafeUse,
    unsafeFromStrictByteString,
    unsafeToStrictByteString,
  )
where

import Data.Array.Byte
import Data.ByteString.Internal qualified as BS
import Data.Functor
import Data.Primitive
import Data.Text.Internal qualified as T
import Foreign
import GHC.Exts
import GHC.ForeignPtr
import System.IO.Unsafe
import Unsafe.Coerce

unsafeCreate :: Int -> (Ptr Word8 -> IO ()) -> T.Text
unsafeCreate len k = unsafeCreateUpToN len $ \ptr -> k ptr $> len

unsafeCreateUpToN :: Int -> (Ptr Word8 -> IO Int) -> T.Text
unsafeCreateUpToN max_len k
  | max_len <= 0 = T.empty
  | otherwise = unsafePerformIO $ do
      mba <- newPinnedByteArray max_len
      len <- k $ mutableByteArrayContents mba
      ba <- unsafeFreezeByteArray mba
      pure $ T.Text ba 0 len

unsafeUse :: T.Text -> (Ptr Word8 -> Int -> IO a) -> a
unsafeUse (T.Text ba off len) k =
  unsafePerformIO $
    if
      | len <= 0 -> k nullPtr 0
      | isByteArrayPinned ba -> withByteArrayContents ba $
          \ptr -> k (ptr `plusPtr` off) len
      | otherwise -> do
          mba <- newPinnedByteArray len
          withMutableByteArrayContents mba $ \ptr -> do
            copyByteArray mba 0 ba off len
            k ptr len

unsafeFromMutableByteArray# ::
  Addr# -> MutableByteArray# RealWorld -> Int -> T.Text
unsafeFromMutableByteArray# addr# mba# =
  T.Text
    (ByteArray $ unsafeCoerceUnlifted mba#)
    (I# $ addr# `minusAddr#` mutableByteArrayContents# mba#)

unsafeFromStrictByteString :: BS.ByteString -> T.Text
unsafeFromStrictByteString (BS.BS (ForeignPtr addr# (PlainPtr mba#)) len) =
  unsafeFromMutableByteArray# addr# mba# len
unsafeFromStrictByteString (BS.BS (ForeignPtr addr# (MallocPtr mba# _)) len) =
  unsafeFromMutableByteArray# addr# mba# len
unsafeFromStrictByteString (BS.BS fptr len) =
  unsafePerformIO $ withForeignPtr fptr $ \src ->
    pure $ unsafeCreate len $ \dest -> copyBytes dest src len

unsafeToStrictByteString :: T.Text -> BS.ByteString
unsafeToStrictByteString (T.Text ba@(ByteArray ba#) off@(I# off#) len)
  | isByteArrayPinned ba =
      BS.BS
        ( ForeignPtr
            (byteArrayContents# ba# `plusAddr#` off#)
            (PlainPtr $ unsafeCoerceUnlifted ba#)
        )
        len
  | otherwise = unsafePerformIO $ do
      mba@(MutableByteArray mba#) <- newPinnedByteArray len
      copyByteArray mba 0 ba off len
      pure $
        BS.BS
          (ForeignPtr (mutableByteArrayContents# mba#) (PlainPtr mba#))
          len
