-------------------------------------------------------------------------------
-- File: printarray.hs
-- Author: David Wells <drwells@vt.edu>
-- Description: Create F90 look-up tables given functions and nodes.
-------------------------------------------------------------------------------
import qualified BasisFunctions.Quadratics as Quads
import qualified BasisFunctions.QuadraticGradients as Grads

main = putStrLn $
        (printArrays 1 Quads.allquadratics Quads.allquadraticsStrings nodes) ++
        (printArrays 2 Grads.allquadratics Grads.allquadraticsStrings nodes)
    where nodes = [[0.659027622374092, 0.231933368553031],
                  [0.659027622374092, 0.109039009072877],
                  [0.231933368553031, 0.659027622374092],
                  [0.231933368553031, 0.109039009072877],
                  [0.109039009072877, 0.659027622374092],
                  [0.109039009072877, 0.231933368553031]]

-------------------------------------------------------------------------------
-- printArrays - a wrapper around printArraySub
-- inputs:
--      dim       - desired dimensionality of the output array
--      funcList  - a list of functions
--      funcNames - desired names of said functions
--      nodes     - list of integration points (where we evaluate functions)
-- output:
--      a list of Fortran 90 subroutines that return the functions evaluated at
--      the nodes.
-------------------------------------------------------------------------------
printArrays dim funcList funcNames nodes
    | (dim == 1) = unlines $ zipWith3 printArraySub funcNames (repeat 1) funcVals1D
    | (dim == 2) = unlines $ zipWith3 printArraySub funcNames (repeat 2) f90StyleList2D
    -- funcVals1D - looks like [[[1],[2],[3]],[2],[3],[4]], want it to look
    -- like [[1,2,3],[2,3,4]].
    --
    -- Similarly, funcVals2D looks like [[[1,1],[2,2],[3,3]],[2,2],[3,3],[4,4]]
    where funcVals1D = [map head x | x <- zipWith map funcList (repeat nodes)]
          -- for a 2D array in F90 the values must be sorted as [x1,x2,y1,y2]
          funcVals2D = zipWith map funcList (repeat nodes)
          f90StyleList2D = [(map head x) ++ (map last x) | x <- funcVals2D]

printArraySub name dimension array = unlines $
-- Given a basis function name, dimension of the output array (1D, 2D, 3D,
-- etc), and the 'array' itself (since this is Haskell this is just a list)
-- return a string.
--
-- Warning: in Fortran arrays are given in column order. Therefore the passed
-- in 'array' to be printed must be something like [x,x,x,y,y,y]. Put another
-- way, this function is stupid and will just print lists of numbers.
    ["subroutine " ++ name ++ "(" ++ arrayName ++ ")",
     "    implicit none",
     "    double precision :: " ++ arrayName ++ (printDims dim1 dim2),
     "    " ++ arrayName ++ " = " ++ (printF90array array arrayName),
     "end subroutine " ++ name]
        where arrayName = "basisvals"
              dim1 = (length array) `div` dimension - 1 -- numbering from zero
              dim2 = dimension - 1 -- coded as input value; technically not
              -- necessary, but makes coding easier.

-------------------------------------------------------------------------------
-- Print out an array nicely. example :
-- printF90array [1,2,3] myarray = (/ 1, &
--                                    2, &
--                                    3 /)
-- where the arrayName generates the correct number of spaces for it to line
-- up.
-------------------------------------------------------------------------------
printF90array array arrayName = "(/" ++ showFirst ++ showMiddle
                            ++ showLast ++ "/)"
    where showFirst  = (show $ head array) ++ ", &\n"
          showMiddle = concat $ map (\ array -> ((spaces spaceNum)
                              ++ show array ++ ", &\n")) (tail $ init array)
          showLast   = (spaces 18) ++ (show $ last array)
          spaceNum   = (9 + (length arrayName))

-------------------------------------------------------------------------------
-- Print out the dimensions of a matrix, Fortran-90 style. example:
-- printDims 2 3 = (0:2,0:3)
-- printDims 2 0 = (0:2)
-------------------------------------------------------------------------------
printDims dim1 dim2
    | (dim2 == 0) = "(0:" ++ (show dim1) ++ ")"
    | otherwise = "(0:" ++ (show dim1) ++ ",0:" ++ (show dim2) ++ ")"

-------------------------------------------------------------------------------
-- return an integer number of spaces.
-------------------------------------------------------------------------------
spaces x = take x $ repeat ' '
