{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenCV.Calib3d
    ( FundamentalMatMethod(..)
    , WhichImage(..)
    , findFundamentalMat
    , computeCorrespondEpilines
    , CalibrateCameraFlags(..)
    , calibrateCamera
    , FindCirclesGridFlags(..)
    , findCirclesGrid
    , SolvePnPFlags(..)
    , solvePnP
    , drawChessboardCorners
    ) where

import "base" Data.Int
import "base" Data.Word
import "base" Data.Bits ( (.|.) )
import "base" Data.List (foldl')
import "base" Foreign.C.Types
import "base" Foreign.Ptr (Ptr)
import "base" Foreign.Marshal.Alloc ( alloca )
import "base" Foreign.Marshal.Array ( peekArray )
import "base" Foreign.Marshal.Utils ( toBool, fromBool )
import "base" Foreign.Storable (peek, Storable)
import qualified "inline-c" Language.C.Inline as C
import qualified "inline-c-cpp" Language.C.Inline.Cpp as C
import "this" OpenCV.Internal.C.Inline ( openCvCtx )
import "this" OpenCV.Internal.C.Types
import "this" OpenCV.Internal.Calib3d.Constants
import "this" OpenCV.Features2d
import "this" OpenCV.Core.Types
import "this" OpenCV.Internal.Core.Types
import "this" OpenCV.Internal.Core.Types.Mat
import "this" OpenCV.Internal.Exception
import "this" OpenCV.TypeLevel
import "this" OpenCV.Internal
import "transformers" Control.Monad.Trans.Except
import qualified "vector" Data.Vector as V

--------------------------------------------------------------------------------

C.context openCvCtx

C.include "opencv2/core.hpp"
C.include "opencv2/calib3d.hpp"
C.include "simple_blob_detector.hpp"
C.using "namespace cv"

--------------------------------------------------------------------------------
-- Types

data FundamentalMatMethod
   = FM_7Point
   | FM_8Point
   | FM_Ransac !(Maybe Double) !(Maybe Double)
   | FM_Lmeds  !(Maybe Double)
     deriving (Show, Eq)

marshalFundamentalMatMethod :: FundamentalMatMethod -> (Int32, CDouble, CDouble)
marshalFundamentalMatMethod = \case
    FM_7Point       -> (c'CV_FM_7POINT, 0, 0)
    FM_8Point       -> (c'CV_FM_8POINT, 0, 0)
    FM_Ransac p1 p2 -> (c'CV_FM_RANSAC, maybe 3 realToFrac p1, maybe 0.99 realToFrac p2)
    FM_Lmeds     p2 -> (c'CV_FM_LMEDS, 0, maybe 0.99 realToFrac p2)

data WhichImage = Image1 | Image2 deriving (Show, Eq)

marshalWhichImage :: WhichImage -> Int32
marshalWhichImage = \case
    Image1 -> 1
    Image2 -> 2

--------------------------------------------------------------------------------

data CalibrateCameraFlags
  = CV_CALIB_USE_INTRINSIC_GUESS
  | CV_CALIB_FIX_PRINCIPAL_POINT
  | CV_CALIB_FIX_ASPECT_RATIO
  | CV_CALIB_ZERO_TANGENT_DIST
  | CV_CALIB_FIX_K1
  | CV_CALIB_FIX_K2
  | CV_CALIB_FIX_K3
  | CV_CALIB_FIX_K4
  | CV_CALIB_FIX_K5
  | CV_CALIB_FIX_K6
  | CV_CALIB_RATIONAL_MODEL
  | CALIB_THIN_PRISM_MODEL
  | CALIB_FIX_S1_S2_S3_S4

c'CalibrateCameraFlags :: CalibrateCameraFlags -> Int32
c'CalibrateCameraFlags = \case
  CV_CALIB_USE_INTRINSIC_GUESS -> c'CV_CALIB_USE_INTRINSIC_GUESS
  CV_CALIB_FIX_PRINCIPAL_POINT -> c'CV_CALIB_FIX_PRINCIPAL_POINT
  CV_CALIB_FIX_ASPECT_RATIO    -> c'CV_CALIB_FIX_ASPECT_RATIO
  CV_CALIB_ZERO_TANGENT_DIST   -> c'CV_CALIB_ZERO_TANGENT_DIST
  CV_CALIB_FIX_K1              -> c'CV_CALIB_FIX_K1
  CV_CALIB_FIX_K2              -> c'CV_CALIB_FIX_K2
  CV_CALIB_FIX_K3              -> c'CV_CALIB_FIX_K3
  CV_CALIB_FIX_K4              -> c'CV_CALIB_FIX_K4
  CV_CALIB_FIX_K5              -> c'CV_CALIB_FIX_K5
  CV_CALIB_FIX_K6              -> c'CV_CALIB_FIX_K6
  CV_CALIB_RATIONAL_MODEL      -> c'CV_CALIB_RATIONAL_MODEL
  CALIB_THIN_PRISM_MODEL       -> c'CALIB_THIN_PRISM_MODEL
  CALIB_FIX_S1_S2_S3_S4        -> c'CALIB_FIX_S1_S2_S3_S4

marshalCalibrateCameraFlags :: [CalibrateCameraFlags] -> Int32
marshalCalibrateCameraFlags = foldl' (.|.) 0 . map c'CalibrateCameraFlags

-- {- |
-- <http://docs.opencv.org/3.0-last-rst/modules/calib3d/doc/camera_calibration_and_3d_reconstruction.html#calibratecamera OpenCV Sphinx doc>
-- -}
calibrateCamera
    :: ( IsSize imageSize Int32
       , camMat     ~ Mat (ShapeT [3, 3]) ('S 1) ('S Double)
       , distCoeffs ~ Mat 'D              ('S 1) ('S Double)
       , IsPoint2 point2 CFloat
       , IsPoint3 point3 CFloat
       , rvecs ~ V.Vector Point3f
       , tvecs ~ V.Vector Point3f
       )
    => V.Vector ( V.Vector (point3 CFloat) ) -- ^ Object points
    -> V.Vector ( V.Vector (point2 CFloat) ) -- ^ Image points
    -> imageSize Int32
    -> [CalibrateCameraFlags]
    -> TermCriteria
    -> CvExcept (Double, camMat, distCoeffs, rvecs, tvecs)
calibrateCamera
  objectPoints imagePoints
  imageSize flags
  termCriteria = unsafeWrapException $ do
    newCamMat     <- unsafeCoerceMat <$> newEmptyMat
    newDistCoeffs <- unsafeCoerceMat <$> newEmptyMat
    id $
      withVectorOfVectorOfPoint3f objectPoints $ \objectPointsPtr objectPointsLengths ->
      withVectorOfVectorOfPoint2f imagePoints  $ \imagePointsPtr imagePointsLengths   ->
      withPtr (toSize imageSize)               $ \imageSizePtr                        ->
      withPtr newCamMat                        $ \camMatPtr                           ->
      withPtr newDistCoeffs                    $ \distCoeffsPtr                       ->
      withPtr termCriteria                     $ \termCriteriaPtr                     ->

      allocVector $ \(rvecsPtrPtr :: Ptr (Ptr (Ptr C'Point3f))) rvecsLengthPtr ->
      allocVector $ \(tvecsPtrPtr :: Ptr (Ptr (Ptr C'Point3f))) tvecsLengthPtr ->

      alloca $ \rmsPtr -> flip handleCvException
        [cvExcept|

          // Create a vector of vector of Point3f for objectPoints
          int32_t *objectPointsLengths = $(int32_t * objectPointsLengths);
          int32_t numObjectPoints = $(int32_t c'objectPointsPtrSize);

          std::vector<std::vector<Point3f>> objectPoints;
          objectPoints.reserve(numObjectPoints);
          for (int i = 0; i < numObjectPoints; i++) {
            const Point3f *pointsAtIndex = $(const Point3f * * objectPointsPtr)[i];
            const int32_t numPointsAtIndex = objectPointsLengths[i];
            std::vector<Point3f> objectPointsAtIndexVec(
              pointsAtIndex,
              pointsAtIndex + numPointsAtIndex
              );

            objectPoints.push_back(objectPointsAtIndexVec);
          }

          // Create a vector of vector of Point2f for imagePoints
          int32_t *imagePointsLengths = $(int32_t * imagePointsLengths);
          int32_t numImagePoints = $(int32_t c'imagePointsPtrSize);

          std::vector<std::vector<Point2f>> imagePoints;
          imagePoints.reserve(numImagePoints);
          for (int i = 0; i < numImagePoints; i++) {
            const Point2f *pointsAtIndex = $(const Point2f * * imagePointsPtr)[i];
            const int32_t numPointsAtIndex = imagePointsLengths[i];
            std::vector<Point2f> imagePointsAtIndexVec(
              pointsAtIndex,
              pointsAtIndex + numPointsAtIndex
              );

            imagePoints.push_back(imagePointsAtIndexVec);
          }

          std::vector<cv::Mat> rvecs;
          std::vector<cv::Mat> tvecs;

          *$(double * rmsPtr) = cv::calibrateCamera
            ( objectPoints
            , imagePoints
            , *$(Size2i * imageSizePtr)
            , *$(Mat * camMatPtr)
            , *$(Mat * distCoeffsPtr)
            , rvecs
            , tvecs
            , $(int32_t c'calibrateCameraFlags)
            , *$(TermCriteria * termCriteriaPtr)
            );

          // Copy rvecs into result
          cv::Point3f * * * rvecsPtrPtr = $(Point3f * * * rvecsPtrPtr);
          cv::Point3f * * rvecsPtr = new cv::Point3f * [rvecs.size()];
          *rvecsPtrPtr = rvecsPtr;

          *$(int32_t * rvecsLengthPtr) = rvecs.size();

          for(unsigned int i = 0; i < rvecs.size(); i++){
            rvecsPtr[i] = new cv::Point3f(rvecs[i]);
          }

          // Copy tvecs into result
          cv::Point3f * * * tvecsPtrPtr = $(Point3f * * * tvecsPtrPtr);
          cv::Point3f * * tvecsPtr = new cv::Point3f * [tvecs.size()];
          *tvecsPtrPtr = tvecsPtr;

          *$(int32_t * tvecsLengthPtr) = tvecs.size();

          for(unsigned int i = 0; i < tvecs.size(); i++){
            tvecsPtr[i] = new cv::Point3f(tvecs[i]);
          }
        |] $ do -- when no exception...

          -- Extract the rotation and translation vectors
          rvecs <- peekVector rvecsPtrPtr rvecsLengthPtr deletePoint3fArray
          tvecs <- peekVector tvecsPtrPtr tvecsLengthPtr deletePoint3fArray

          rms <- realToFrac <$> peek rmsPtr
          return (rms, newCamMat, newDistCoeffs, rvecs, tvecs)

  where
    c'objectPointsPtrSize = fromIntegral $ V.length objectPoints
    c'imagePointsPtrSize  = fromIntegral $ V.length imagePoints
    c'calibrateCameraFlags = marshalCalibrateCameraFlags flags



data FindCirclesGridFlags
  = CALIB_CB_SYMMETRIC_GRID
  | CALIB_CB_ASYMMETRIC_GRID
  | CALIB_CB_CLUSTERING

marshalFindCirclesGridFlags :: [FindCirclesGridFlags] -> Int32
marshalFindCirclesGridFlags = foldl' (.|.) 0 . map c'FindCirclesGridFlags
  where
    c'FindCirclesGridFlags :: FindCirclesGridFlags -> Int32
    c'FindCirclesGridFlags = \case
      CALIB_CB_SYMMETRIC_GRID  -> c'CALIB_CB_SYMMETRIC_GRID
      CALIB_CB_ASYMMETRIC_GRID -> c'CALIB_CB_ASYMMETRIC_GRID
      CALIB_CB_CLUSTERING      -> c'CALIB_CB_CLUSTERING

findCirclesGrid
  :: ( IsSize patternSize Int32 )
  => Mat shape channels depth
  -> patternSize Int32
  -> [FindCirclesGridFlags]
  -> SimpleBlobDetector
  -> CvExcept (Bool, V.Vector Point2f)
findCirclesGrid image patternSize flags detector
  = unsafeWrapException $
  withPtr image                $ \imgPtr                                       ->
  withPtr (toSize patternSize) $ \patternSizePtr                               ->
  alloca                       $ \(numCentersPtr :: Ptr Int32)                 ->
  alloca                       $ \(centersPtrPtr :: Ptr (Ptr (Ptr C'Point2f))) ->
  alloca                       $ \(resultPtr     :: Ptr CInt)                  ->
  withPtr detector             $ \detectorPtr ->
  flip handleCvException
  [cvExcept|
    std::vector<cv::Point2f> centers;

    cv::Ptr<cv::SimpleBlobDetector> blobDetector = *$(Ptr_SimpleBlobDetector * detectorPtr);

    bool patternFound = findCirclesGrid
      ( *$(Mat * imgPtr)
      , *$(Size2i * patternSizePtr)
      , centers
      , $(int32_t c'findCirclesGridFlags)
      , blobDetector
      );
    // Copy centers into result
    *$(int32_t * numCentersPtr) = centers.size();
    cv::Point2f * * centersPtr = new cv::Point2f * [centers.size()];
    *$(Point2f * * * centersPtrPtr) = centersPtr;
    for (int i = 0; i < centers.size(); i++) {
      centersPtr[i] = new cv::Point2f(centers[i]);
    }

    *$(bool *resultPtr) = patternFound ? 1 : 0;
  |] $ do
    patternFound <- toBool <$> peek resultPtr
    centers <- peekVector centersPtrPtr numCentersPtr deletePoint2fArray

    return (patternFound, centers)
  where
    c'findCirclesGridFlags = marshalFindCirclesGridFlags flags

drawChessboardCorners
  :: ( IsSize patternSize Int32
     , IsPoint2 point2 CFloat
     )
  => Mat shape channels depth
  -> patternSize Int32
  -> V.Vector (point2 CFloat)
  -> Bool
  -> CvExcept (Mat shape channels depth)
drawChessboardCorners image patternSize corners patternWasFound = do

  unsafeWrapException $ do
    mutImage <- thaw image
    id $
      withPtr mutImage                        $ \imagePtr              ->
      withPtr (toSize patternSize)            $ \patternSizePtr        ->
      withArrayPtrLen (V.map toPoint corners) $ \cornersPtr cornersLen ->
      flip handleCvException
      [cvExcept|

        cv::_InputArray corners = cv::_InputArray
          ( $(Point2f * cornersPtr)
          , $(int32_t   cornersLen));

        drawChessboardCorners
          ( *$(Mat * imagePtr)
          , *$(Size2i * patternSizePtr)
          , corners
          , $(bool c'PatternWasFound)
          );
      |] $ do
        freeze mutImage
  where
    c'PatternWasFound = fromBool patternWasFound

data SolvePnPFlags
 = SOLVEPNP_ITERATIVE
 | SOLVEPNP_EPNP
 | SOLVEPNP_P3P
 | SOLVEPNP_DLS
 | SOLVEPNP_UPNP

marshalSolvePnPFlags :: [SolvePnPFlags] -> Int32
marshalSolvePnPFlags = foldl' (.|.) 0 . map c'SolvePnPFlags
  where
    c'SolvePnPFlags :: SolvePnPFlags -> Int32
    c'SolvePnPFlags = \case
      SOLVEPNP_ITERATIVE -> c'SOLVEPNP_ITERATIVE
      SOLVEPNP_EPNP      -> c'SOLVEPNP_EPNP
      SOLVEPNP_P3P       -> c'SOLVEPNP_P3P
      SOLVEPNP_DLS       -> c'SOLVEPNP_DLS
      SOLVEPNP_UPNP      -> c'SOLVEPNP_UPNP

solvePnP
  :: ( camMat     ~ Mat (ShapeT [3, 3]) ('S 1) ('S Double)
     , distCoeffs ~ Mat shape           ('S 1) ('S Double)
     , IsPoint2 point2 CFloat
     , IsPoint3 point3 CFloat
     )
  => V.Vector (point3 CFloat) -- ^ Object points
  -> V.Vector (point2 CFloat) -- ^ Image points
  -> camMat
  -> distCoeffs
  -> Bool
  -> [SolvePnPFlags]
  -> CvExcept (Point3f, Point3f)
solvePnP
  objectPoints
  imagePoints
  cameraMatrix
  distCoeffs
  useExtrinsicGuess
  flags
  = unsafeWrapException $

    withArrayPtr (V.map toPoint objectPoints) $ \objectPointsPtr ->
    withArrayPtr (V.map toPoint imagePoints)  $ \imagePointsPtr  ->
    withPtr cameraMatrix                      $ \cameraMatrixPtr ->
    withPtr distCoeffs                        $ \distCoeffsPtr   ->
    alloca                                    $ \rvecPtrPtr      ->
    alloca                                    $ \tvecPtrPtr      ->
    flip handleCvException
    [cvExcept|
      cv::_InputArray objectPoints = cv::_InputArray
        ( $(Point3f * objectPointsPtr)
        , $(int32_t c'numObjectPoints));
      cv::_InputArray imagePoints  = cv::_InputArray
        ( $(Point2f * imagePointsPtr)
        , $(int32_t c'numImagePoints));

      cv::Mat rvec(3,1,cv::DataType<float>::type);
      cv::Mat tvec(3,1,cv::DataType<float>::type);
      solvePnP
        ( objectPoints
        , imagePoints
        , *$(Mat *cameraMatrixPtr)
        , *$(Mat *distCoeffsPtr)
        , rvec
        , tvec
        , $(bool c'useExtrinsicGuess)
        , $(int32_t c'solvePnPFlags)
        );

      *$(Point3f * * rvecPtrPtr) = new cv::Point3f(rvec);
      *$(Point3f * * tvecPtrPtr) = new cv::Point3f(tvec);
    |] $ do
      rvecPtr <- peek rvecPtrPtr
      tvecPtr <- peek tvecPtrPtr
      rvec <- fromPtr (pure rvecPtr)
      tvec <- fromPtr (pure tvecPtr)
      return (rvec, tvec)
  where
    c'numObjectPoints   = fromIntegral $ V.length objectPoints
    c'numImagePoints    = fromIntegral $ V.length imagePoints
    c'solvePnPFlags     = marshalSolvePnPFlags flags
    c'useExtrinsicGuess = fromBool useExtrinsicGuess



{- | Calculates a fundamental matrix from the corresponding points in two images

The minimum number of points required depends on the 'FundamentalMatMethod'.

 * 'FM_7Point': @N == 7@
 * 'FM_8Point': @N >= 8@
 * 'FM_Ransac': @N >= 15@
 * 'FM_Lmeds': @N >= 8@

With 7 points the 'FM_7Point' method is used, despite the given method.

With more than 7 points the 'FM_7Point' method will be replaced by the
'FM_8Point' method.

Between 7 and 15 points the 'FM_Ransac' method will be replaced by the
'FM_Lmeds' method.

With the 'FM_7Point' method and with 7 points the result can contain up to 3
matrices, resulting in either 3, 6 or 9 rows. This is why the number of
resulting rows in tagged as 'D'ynamic. For all other methods the result always
contains 3 rows.

<http://docs.opencv.org/3.0-last-rst/modules/calib3d/doc/camera_calibration_and_3d_reconstruction.html#findfundamentalmat OpenCV Sphinx doc>
-}
findFundamentalMat
    :: (IsPoint2 point2 CDouble)
    => V.Vector (point2 CDouble) -- ^ Points from the first image.
    -> V.Vector (point2 CDouble) -- ^ Points from the second image.
    -> FundamentalMatMethod
    -> CvExcept ( Maybe ( Mat ('S '[ 'D, 'S 3 ]) ('S 1) ('S Double)
                        , Mat ('S '[ 'D, 'D   ]) ('S 1) ('S Word8 )
                        )
                )
findFundamentalMat pts1 pts2 method = do
    (fm, pointMask) <- c'findFundamentalMat
    -- If the c++ function can't find a fundamental matrix it will
    -- retrun an empty matrix. We check for this case by trying to
    -- coerce the result to the desired type.
    catchE (Just . (, unsafeCoerceMat pointMask) <$> coerceMat fm)
           (\case CoerceMatError _msgs -> pure Nothing
                  otherError -> throwE otherError
           )
  where
    c'findFundamentalMat = unsafeWrapException $ do
      fm   <- newEmptyMat
      pointMask <- newEmptyMat
      handleCvException (pure (fm, pointMask)) $
        withPtr fm $ \fmPtr ->
        withPtr pointMask $ \pointMaskPtr ->
        withArrayPtr (V.map toPoint pts1) $ \pts1Ptr ->
        withArrayPtr (V.map toPoint pts2) $ \pts2Ptr ->
          [cvExcept|
            cv::_InputArray pts1 = cv::_InputArray($(Point2d * pts1Ptr), $(int32_t c'numPts1));
            cv::_InputArray pts2 = cv::_InputArray($(Point2d * pts2Ptr), $(int32_t c'numPts2));
            *$(Mat * fmPtr) =
              cv::findFundamentalMat
              ( pts1
              , pts2
              , $(int32_t c'method)
              , $(double c'p1)
              , $(double c'p2)
              , *$(Mat * pointMaskPtr)
              );
          |]

    c'numPts1 = fromIntegral $ V.length pts1
    c'numPts2 = fromIntegral $ V.length pts2
    (c'method, c'p1, c'p2) = marshalFundamentalMatMethod method

{- | For points in an image of a stereo pair, computes the corresponding epilines in the other image

<http://docs.opencv.org/3.0-last-rst/modules/calib3d/doc/camera_calibration_and_3d_reconstruction.html#computecorrespondepilines OpenCV Sphinx doc>
-}
computeCorrespondEpilines
    :: (IsPoint2 point2 CDouble)
    => V.Vector (point2 CDouble) -- ^ Points.
    -> WhichImage -- ^ Image which contains the points.
    -> Mat (ShapeT [3, 3]) ('S 1) ('S Double) -- ^ Fundamental matrix.
    -> CvExcept (Mat ('S ['D, 'S 1]) ('S 3) ('S Double))
computeCorrespondEpilines points whichImage fm = unsafeWrapException $ do
    epilines <- newEmptyMat
    handleCvException (pure $ unsafeCoerceMat epilines) $
      withArrayPtr (V.map toPoint points) $ \pointsPtr ->
      withPtr fm       $ \fmPtr       ->
      withPtr epilines $ \epilinesPtr -> do
        -- Destroy type information about the pointsPtr. We wan't to generate
        -- C++ code that works for any type of point. Specifically Point2f and
        -- Point2d.
        [cvExcept|
          cv::_InputArray points =
            cv::_InputArray( $(Point2d * pointsPtr)
                           , $(int32_t c'numPoints)
                           );
          cv::computeCorrespondEpilines
          ( points
          , $(int32_t c'whichImage)
          , *$(Mat * fmPtr)
          , *$(Mat * epilinesPtr)
          );
        |]
  where
    c'numPoints = fromIntegral $ V.length points
    c'whichImage = marshalWhichImage whichImage


