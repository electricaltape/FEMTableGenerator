-------------------------------------------------------------------------------
-- ArgyrisLaplacian.hs
-- Author: David Wells <drwells@vt.edu>
-- Description: Laplacians of the Argyris basis functions.
-------------------------------------------------------------------------------
module BasisFunctions.ArgyrisLaplacian
( allFunctions,
  allFunctionNames
) where

import qualified BasisFunctions.ArgyrisDerivativesXX as XX
import qualified BasisFunctions.ArgyrisDerivativesYY as YY

allFunctions :: [([Double] -> [Double])]
-- each entry : (allArgyrisDerivativesYY !! 0) z + (allArgyrisDerivativesXX !! 0) z
allFunctions =
    zipWith (\ argyrisXX argyrisYY -> (\ z -> [head (argyrisXX z)  + head (argyrisYY z)]))
    XX.allFunctions YY.allFunctions



allFunctionNames :: [String]
allFunctionNames = ["argyrisLaplacian1", "argyrisLaplacian2",
                    "argyrisLaplacian3", "argyrisLaplacian4",
                    "argyrisLaplacian5", "argyrisLaplacian6",
                    "argyrisLaplacian7", "argyrisLaplacian8",
                    "argyrisLaplacian9", "argyrisLaplacian10",
                    "argyrisLaplacian11", "argyrisLaplacian12",
                    "argyrisLaplacian13", "argyrisLaplacian14",
                    "argyrisLaplacian15", "argyrisLaplacian16",
                    "argyrisLaplacian17", "argyrisLaplacian18",
                    "argyrisLaplacian19", "argyrisLaplacian20",
                    "argyrisLaplacian21"]