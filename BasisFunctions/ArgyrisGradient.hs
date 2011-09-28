-------------------------------------------------------------------------------
-- ArgyrisGradient.hs
-- Author: David Wells <drwells@vt.edu>
-- Description: Gradients of the Argyris basis functions.
-------------------------------------------------------------------------------
module BasisFunctions.ArgyrisGradient
( allFunctions,
  allFunctionNames
) where

import qualified BasisFunctions.ArgyrisDerivativesX as X
import qualified BasisFunctions.ArgyrisDerivativesY as Y

allFunctions :: [([Double] -> [Double])]
-- each entry : (allArgyrisDerivativesX !! 0) z (allArgyrisDerivativesY !! 0) z
allFunctions =
-- not sure if this works. Try it later.
    zipWith (\ argyrisXX argyrisYY -> (\ z -> [head (argyrisXX z), head (argyrisYY z)]))
    X.allFunctions Y.allFunctions

allFunctionNames :: [String]
allFunctionNames = ["argyrisGradient1", "argyrisGradient2",
                    "argyrisGradient3", "argyrisGradient4",
                    "argyrisGradient5", "argyrisGradient6",
                    "argyrisGradient7", "argyrisGradient8",
                    "argyrisGradient9", "argyrisGradient10",
                    "argyrisGradient11", "argyrisGradient12",
                    "argyrisGradient13", "argyrisGradient14",
                    "argyrisGradient15", "argyrisGradient16",
                    "argyrisGradient17", "argyrisGradient18",
                    "argyrisGradient19", "argyrisGradient20",
                    "argyrisGradient21"]