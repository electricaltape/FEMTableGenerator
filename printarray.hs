-------------------------------------------------------------------------------
-- File: printarray.hs
-- Author: David Wells <drwells@vt.edu>
-- Description: Create F90 look-up tables given functions and nodes.
-------------------------------------------------------------------------------
import qualified FormatArrays.PrintF90 as F90
import qualified FormatArrays.PrintM   as M
import qualified Polynomial.Polynomials as P
-- import qualified PrintHaskell as Hs
-- import qualified PrintPython  as Py
-- import qualified PrintC       as C

data ArrayStyle   = Haskell | Python | Matlab | C | Fortran90
    deriving (Eq, Ord)
type FunctionList = [([Double] -> [Double])]

-- take polynomial description from the standard input and turn it in to lookup
-- tables.
main :: IO ()
main = getContents >>= (\ polynomials -> putStrLn $
         printArrays Matlab (P.exportGradientsAndPolynomials polynomials) nodes6)

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
printArrays :: ArrayStyle -> ([String], FunctionList) -> [[Double]] -> String
printArrays language (funcNames, funcList) nodes
  | (language == Fortran90) = F90.printOutput evaluatedFunctions funcNames
--  | (language == Haskell)   =  Hs.printOutput
--  | (language == Python)    =  Py.printOutput
--  | (language == C)         =   C.printOutput
  | (language == Matlab)    =   M.printOutput evaluatedFunctions funcNames
  | otherwise = F90.printOutput evaluatedFunctions funcNames
    where evaluatedFunctions = [map f nodes | f <- funcList]

nodes6 :: [[Double]]
-- Set of 6 Gaussian Quadrature points for a basis triangle: should interpolate
-- order 4 polynomials correctly.
nodes6 = [[0.659027622374092, 0.231933368553031],
          [0.659027622374092, 0.109039009072877],
          [0.231933368553031, 0.659027622374092],
          [0.231933368553031, 0.109039009072877],
          [0.109039009072877, 0.659027622374092],
          [0.109039009072877, 0.231933368553031]]

nodes28 :: [[Double]]
-- Set of 28 quadrature points - degree of precision 11. ACM TOMS 612.
-- Taken from Burkardt.
nodes28 = [[0.33333333333333333,  0.333333333333333333],
           [0.9480217181434233,   0.02598914092828833],
           [0.02598914092828833,  0.9480217181434233],
           [0.02598914092828833,  0.02598914092828833],
           [0.8114249947041546,   0.09428750264792270],
           [0.09428750264792270,  0.8114249947041546],
           [0.09428750264792270,  0.09428750264792270],
           [0.01072644996557060,  0.4946367750172147],
           [0.4946367750172147,   0.01072644996557060],
           [0.4946367750172147,   0.4946367750172147],
           [0.5853132347709715,   0.2073433826145142],
           [0.2073433826145142,   0.5853132347709715],
           [0.2073433826145142,   0.2073433826145142],
           [0.1221843885990187,   0.4389078057004907],
           [0.4389078057004907,   0.1221843885990187],
           [0.4389078057004907,   0.4389078057004907],
           [0.6779376548825902,   0.04484167758913055],
           [0.6779376548825902,   0.27722066752827925],
           [0.04484167758913055,  0.6779376548825902],
           [0.04484167758913055,  0.27722066752827925],
           [0.27722066752827925,  0.6779376548825902],
           [0.27722066752827925,  0.04484167758913055],
           [0.8588702812826364,   0.00000000000000000],
           [0.8588702812826364,   0.1411297187173636],
           [0.0000000000000000,   0.8588702812826364],
           [0.0000000000000000,   0.1411297187173636],
           [0.1411297187173636,   0.8588702812826364],
           [0.1411297187173636,   0.0000000000000000]]
