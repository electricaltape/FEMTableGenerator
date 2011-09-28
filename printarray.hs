-------------------------------------------------------------------------------
-- File: printarray.hs
-- Author: David Wells <drwells@vt.edu>
-- Description: Create F90 look-up tables given functions and nodes.
-------------------------------------------------------------------------------
-- Different elements.
-- Argyris:
import qualified BasisFunctions.Argyris               as Argyris
import qualified BasisFunctions.ArgyrisDerivativesX   as ArgyrisX
import qualified BasisFunctions.ArgyrisDerivativesXX  as ArgyrisXX
import qualified BasisFunctions.ArgyrisDerivativesY   as ArgyrisY
import qualified BasisFunctions.ArgyrisDerivativesYY  as ArgyrisYY
import qualified BasisFunctions.ArgyrisDerivativesXY  as ArgyrisXY
import qualified BasisFunctions.ArgyrisLaplacian      as ArgyrisLaplacian
import qualified BasisFunctions.ArgyrisGradient       as ArgyrisGradient
-- Quadratics:
import qualified BasisFunctions.Quadratics         as Quads
import qualified BasisFunctions.QuadraticGradients as Grads
-- Different Languages.
import qualified FormatArrays.PrintF90 as F90
import qualified FormatArrays.PrintM   as M
-- import qualified PrintHaskell as Hs
-- import qualified PrintPython  as Py
-- import qualified PrintC       as C

-- datatypes.
data ArrayStyle   = Haskell | Python | Matlab | C | Fortran90
    deriving (Eq, Ord)
type FunctionList = [([Double] -> [Double])]

main :: IO ()
main = putStrLn $
-- (printArrays Matlab Quads.allquadratics Quads.allquadraticsStrings nodes) ++
-- (printArrays Matlab Grads.allquadratics Grads.allquadraticsStrings nodes)
   (printArrays Matlab Argyris.allFunctions Argyris.allFunctionNames nodes6) ++
   (printArrays Matlab ArgyrisX.allFunctions ArgyrisX.allFunctionNames nodes6) ++
   (printArrays Matlab ArgyrisXX.allFunctions ArgyrisXX.allFunctionNames nodes6) ++
   (printArrays Matlab ArgyrisYY.allFunctions ArgyrisYY.allFunctionNames nodes6) ++
   (printArrays Matlab ArgyrisXY.allFunctions ArgyrisXY.allFunctionNames nodes6) ++
   (printArrays Matlab ArgyrisGradient.allFunctions ArgyrisGradient.allFunctionNames nodes6) ++
   (printArrays Matlab ArgyrisLaplacian.allFunctions ArgyrisLaplacian.allFunctionNames nodes6)

-------------------------------------------------------------------------------
-- printArrays - a wrapper to various array printers.
-- inputs:
--      language  - desired output language
--      funcList  - a list of functions
--      funcNames - desired names of said functions
--      nodes     - list of integration points (where we evaluate functions)
-- output:
--     a list of language-specific routines that return the functions
--     evaluated at the nodes
-------------------------------------------------------------------------------
printArrays :: ArrayStyle -> FunctionList -> [String] -> [[Double]] -> String
printArrays language funcList funcNames nodes
  | (language == Fortran90) = F90.printOutput evaluatedFunctions funcNames
--  | (language == Haskell)   =  Hs.printOutput
--  | (language == Python)    =  Py.printOutput
--  | (language == C)         =   C.printOutput
  | (language == Matlab)    =   M.printOutput evaluatedFunctions funcNames
  | otherwise = F90.printOutput evaluatedFunctions funcNames
    where evaluatedFunctions = [map f nodes | f <- funcList]

nodes6 :: [[Double]]
-- Set of 6 Gaussian Quadrature points for a basis triangle: should interpolate
-- order 2 polynomials correctly.
nodes6 = [[0.659027622374092, 0.231933368553031],
          [0.659027622374092, 0.109039009072877],
          [0.231933368553031, 0.659027622374092],
          [0.231933368553031, 0.109039009072877],
          [0.109039009072877, 0.659027622374092],
          [0.109039009072877, 0.231933368553031]]

-- nodes8 :: [[Double]]
-- Set of 8 quadrature points.