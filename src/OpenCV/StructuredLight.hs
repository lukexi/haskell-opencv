{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module OpenCV.StructuredLight
    ( newGrayCodePattern
    , setWhiteThreshold
    , setBlackThreshold
    , generateGrayCodePatterns
    ) where

import "base" Data.Int
import "base" Data.Word
import "base" Data.Bits ( (.|.) )
import "base" Data.List (foldl')
import "base" Foreign.C.Types
import "base" Foreign.Ptr (Ptr)
import "base" Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( peekArray )
import "base" Foreign.Marshal.Utils ( toBool, fromBool )
import "base" Foreign.Storable (peek, Storable)
import "base" System.IO.Unsafe
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c" Language.C.Inline.Unsafe as CU
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Calib3d.Constants
import "this" OpenCV.Features2d
import "this" OpenCV.Core.Types
import "this" OpenCV.Internal
import "this" OpenCV.Internal.Core.Types
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.Internal.Exception
import "this" OpenCV.TypeLevel
import "transformers" Control.Monad.Trans.Except
import qualified "vector" Data.Vector as V

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/structured_light.hpp"
C.include "structured_light.hpp"

C.using "namespace cv"
C.using "namespace structured_light"

newtype GrayCodePattern = GrayCodePattern {unGrayCodePattern :: ForeignPtr (C GrayCodePattern)}

type instance C GrayCodePattern = C'Ptr_GrayCodePattern

instance WithPtr GrayCodePattern where
    withPtr = withForeignPtr . unGrayCodePattern

instance FromPtr GrayCodePattern where
    fromPtr = objFromPtr GrayCodePattern $ \ptr ->
                [CU.block| void {
                  Ptr<GrayCodePattern> * graycodepattern_ptr_ptr =
                    $(Ptr_GrayCodePattern * ptr);
                  graycodepattern_ptr_ptr->release();
                  delete graycodepattern_ptr_ptr;
                }|]

-- |
newGrayCodePattern
    :: ( IsSize projectorSize Int32 )
    => projectorSize Int32 -> IO GrayCodePattern
newGrayCodePattern projectorSize = do
  grayCodePattern <-
    withPtr (toSize projectorSize) $ \projectorSizePtr ->
    fromPtr
      [CU.block| Ptr_GrayCodePattern * {
        Size2i projectorSize = *$(Size2i * projectorSizePtr);
        GrayCodePattern::Params params;
        params.width = projectorSize.width;
        params.height = projectorSize.height;
        Ptr<GrayCodePattern> grayCodePatternPtr = GrayCodePattern::create(params);
        return new Ptr<GrayCodePattern>(grayCodePatternPtr);;
      } |]
  return grayCodePattern

setWhiteThreshold :: GrayCodePattern -> CDouble -> IO ()
setWhiteThreshold grayCodePattern thresh = do
  withPtr grayCodePattern $ \grayCodePatternPtr ->
    [CU.block| void {
      GrayCodePattern * grayCodePattern = *$(Ptr_GrayCodePattern * grayCodePatternPtr);
      grayCodePattern->setWhiteThreshold($(double thresh));
    } |]

setBlackThreshold :: GrayCodePattern -> CDouble -> IO ()
setBlackThreshold grayCodePattern thresh = do
  withPtr grayCodePattern $ \grayCodePatternPtr ->
    [CU.block| void {
      GrayCodePattern * grayCodePattern = *$(Ptr_GrayCodePattern * grayCodePatternPtr);
      grayCodePattern->setBlackThreshold($(double thresh));
    } |]

generateGrayCodePatterns :: GrayCodePattern -> V.Vector (Mat ('S [h, w]) ('S 1) ('S Word8))
generateGrayCodePatterns grayCodePattern = unsafePerformIO $
  withPtr grayCodePattern $ \grayCodePatternPtr ->
  allocVector $ \matsPtrPtr matsLengthPtr -> do
    [C.block| void {
      GrayCodePattern * grayCodePattern = *$(Ptr_GrayCodePattern * grayCodePatternPtr);

      std::vector<Mat> mats;
      grayCodePattern->generate(mats);

      cv::Mat * * * matsPtrPtr = $(Mat * * * matsPtrPtr);
      cv::Mat * * matsPtr = new cv::Mat * [mats.size()];
      *matsPtrPtr = matsPtr;

      *$(int32_t * matsLengthPtr) = mats.size();

      for(int i = 0; i < mats.size(); i++){
        matsPtr[i] = new cv::Mat(mats[i]);
      }

    }|]
    peekVector matsPtrPtr matsLengthPtr deleteMatArray
