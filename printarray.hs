-------------------------------------------------------------------------------
-- File: printarray.hs
-- Author: David Wells <drwells@vt.edu>
-- Description: Create F90 look-up tables given functions and nodes.
-------------------------------------------------------------------------------
import qualified BasisFunctions.Quadratics as Quads
import qualified BasisFunctions.QuadraticGradients as Grads
import qualified FormatArrays.PrintF90     as F90
-- import qualified PrintHaskell as Hs
-- import qualified PrintPython  as Py
-- import qualified PrintC       as C
-- import qualified PrintMatlab  as M
--
-- datatypes.
data ArrayStyle   = Haskell | Python | Matlab | C | Fortran90
    deriving (Eq, Ord)
type FunctionList = [([Double] -> [Double])]

main :: IO ()
main = putStrLn $
        (printArrays Fortran90 Quads.allquadratics Quads.allquadraticsStrings nodes) ++
        (printArrays Fortran90 Grads.allquadratics Grads.allquadraticsStrings nodes)
    where nodes = [[0.659027622374092, 0.231933368553031],
                   [0.659027622374092, 0.109039009072877],
                   [0.231933368553031, 0.659027622374092],
                   [0.231933368553031, 0.109039009072877],
                   [0.109039009072877, 0.659027622374092],
                   [0.109039009072877, 0.231933368553031]]

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
--  | (language == Matlab)    =   M.printOutput
  | otherwise = F90.printOutput evaluatedFunctions funcNames
    where evaluatedFunctions = [map f nodes | f <- funcList]
