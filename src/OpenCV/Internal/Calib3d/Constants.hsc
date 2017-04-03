{-# language CPP #-}

#ifndef ENABLE_INTERNAL_DOCUMENTATION
{-# OPTIONS_HADDOCK hide #-}
#endif

module OpenCV.Internal.Calib3d.Constants where

#include <bindings.dsl.h>
#include "opencv2/core.hpp"
#include "opencv2/calib3d.hpp"

#include "namespace.hpp"

#num CV_FM_7POINT
#num CV_FM_8POINT
#num CV_FM_RANSAC
#num CV_FM_LMEDS



#num CV_CALIB_USE_INTRINSIC_GUESS
#num CV_CALIB_FIX_PRINCIPAL_POINT
#num CV_CALIB_FIX_ASPECT_RATIO
#num CV_CALIB_ZERO_TANGENT_DIST
#num CV_CALIB_FIX_K1
#num CV_CALIB_FIX_K2
#num CV_CALIB_FIX_K3
#num CV_CALIB_FIX_K4
#num CV_CALIB_FIX_K5
#num CV_CALIB_FIX_K6
#num CV_CALIB_RATIONAL_MODEL
#num CALIB_THIN_PRISM_MODEL
#num CALIB_FIX_S1_S2_S3_S4
