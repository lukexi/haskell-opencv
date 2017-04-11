{-# language CPP #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module OpenCV.Internal
  ( objFromPtr
  , peekVector
  , allocVector
  , withArrayPtr
  , withArrayPtrLen
  , deleteMatArray
  , deletePoint2fArray
  , deletePoint3fArray
  ) where

import "base" Control.Exception ( mask_, bracket_ )
import "base" Data.Functor (($>))
import "base" Foreign.Concurrent ( newForeignPtr )
import "base" Foreign.ForeignPtr ( ForeignPtr  )
import "base" Foreign.Ptr ( Ptr )
import "base" Data.Proxy
import "base" Foreign ( Storable, Int32, peek, alloca, peekArray, allocaBytes, plusPtr )
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import qualified "vector" Data.Vector as V
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.C.PlacementNew
import "this" OpenCV.Internal.C.Inline ( openCvCtx )

C.context openCvCtx

C.include "opencv2/core.hpp"
C.using "namespace cv"

objFromPtr :: (ForeignPtr c -> hask) -> (Ptr c -> IO ()) -> IO (Ptr c) -> IO hask
objFromPtr haskCons finalizer mkObjPtr = mask_ $ do
    objPtr <- mkObjPtr
    haskCons <$> newForeignPtr objPtr (finalizer objPtr)

-- | A utility for passing a vector of values along with their c-compatible length
withArrayPtrLen
  :: ( PlacementNew (C a)
     , CSizeOf (C a), WithPtr a
     , Num len
     )
  => V.Vector a
  -> (Ptr (C a) -> len -> IO b)
  -> IO b
withArrayPtrLen array action =
  withArrayPtr array (\arrayPtr -> action arrayPtr arrayLen)
  where arrayLen = fromIntegral $ V.length array

-- | A utility for extracting a vector of values and deleting the vector.
-- Use with allocVector.
peekVector
  :: ( FromPtr item
     , Storable len
     , Integral len
     , arrayPtrPtr ~ Ptr (Ptr (Ptr (C item)))
     )
  => arrayPtrPtr
  -> Ptr len
  -> (arrayPtrPtr -> IO ())
  -> IO (V.Vector item)
peekVector arrayPtrPtr lengthPtr deleteFunc = do
  arrayLen <- fromIntegral <$> peek lengthPtr
  arrayPtr <- peek arrayPtrPtr
  vector   <- V.fromList <$>
    (mapM (fromPtr . pure) =<< peekArray arrayLen arrayPtr)

  deleteFunc arrayPtrPtr

  return vector


allocVector f =
  alloca $ \arrayPtrPtr ->
  alloca $ \(arrayLengthPtr :: Ptr Int32)             ->
   f arrayPtrPtr arrayLengthPtr

-- | Perform an action with a temporary pointer to an array of values
--
-- The input values are placed consecutively in memory using the 'PlacementNew'
-- mechanism.
--
-- This function is intended for types which are not managed by the Haskell
-- runtime, but by a foreign system (such as C).
--
-- The pointer is not guaranteed to be usuable outside the scope of this
-- function. The same warnings apply as for 'withForeignPtr'.
withArrayPtr
    :: forall a b
     . (WithPtr a, CSizeOf (C a), PlacementNew (C a))
    => V.Vector a
    -> (Ptr (C a) -> IO b)
    -> IO b
withArrayPtr arr act =
    allocaBytes arraySize $ \arrPtr ->
      bracket_
        (V.foldM'_ copyNext arrPtr arr)
        (deconstructArray arrPtr )
        (act arrPtr)
  where
    elemSize = cSizeOf (Proxy :: Proxy (C a))
    arraySize = elemSize * V.length arr

    copyNext :: Ptr (C a) -> a -> IO (Ptr (C a))
    copyNext !ptr obj = copyObj ptr obj $> plusPtr ptr elemSize

    copyObj :: Ptr (C a) -> a -> IO ()
    copyObj dstPtr src =
        withPtr src $ \srcPtr ->
          placementNew srcPtr dstPtr

    deconstructArray :: Ptr (C a) -> IO ()
    deconstructArray !begin = deconstructNext begin
      where
        deconstructNext !ptr
            | ptr == end = pure ()
            | otherwise = do placementDelete ptr
                             deconstructNext $ ptr `plusPtr` elemSize

        end :: Ptr (C a)
        end = begin `plusPtr` arraySize




-- Specialized functions to delete arrays for use with peekVector

-- Would love to make these generic, but not sure how to
-- parameterize the $(Type ***)
deleteMatArray :: Ptr (Ptr (Ptr C'Mat)) -> IO ()
deleteMatArray arrayPtrPtr =
  [C.block| void {
      delete [] *$(Mat * * * arrayPtrPtr);
  } |]

deletePoint3fArray :: Ptr (Ptr (Ptr C'Point3f)) -> IO ()
deletePoint3fArray arrayPtrPtr =
  [C.block| void {
      delete [] *$(Point3f * * * arrayPtrPtr);
  } |]

deletePoint2fArray :: Ptr (Ptr (Ptr C'Point2f)) -> IO ()
deletePoint2fArray arrayPtrPtr =
  [C.block| void {
      delete [] *$(Point2f * * * arrayPtrPtr);
  } |]

