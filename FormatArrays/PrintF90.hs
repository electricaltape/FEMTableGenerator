--------------------------------------------------------------------------------
-- PrintF90.hs
-- Author: David Wells <drwells@vt.edu>
-- Description: Given lists of doubles, generate a nice string of subroutines
-- callable from F90 that return said lists.
--------------------------------------------------------------------------------

module FormatArrays.PrintF90
( printOutput
) where

printOutput :: [[[Double]]] -> [String] -> String
--------------------------------------------------------------------------------
-- given a look-up list (already computed), return a F90 subroutine
-- representation of each list as an array.
-- look-up list looks like
-- [
-- [[1], [1.5]],
-- [[2], [2.5]],
-- [[3], [3.5]]
-- ] for 1D (functions have scalar output)
--------------------------------------------------------------------------------
printOutput evaluatedFunctions funcNames =
    concat $ zipWith printArraySub funcNames evaluatedFunctions

printArraySub :: String -> [[Double]] -> String
--------------------------------------------------------------------------------
-- Given the name of a subroutine and the values of an array, return the code
-- for a Fortran 90 subroutine that sets some input equal to a precomputed
-- array.
--------------------------------------------------------------------------------
printArraySub name evaluatedPoints = unlines $
    ["subroutine " ++ name ++ "(" ++ arrayName ++ ")",
     "    implicit none",
     "    double precision :: " ++ arrayName ++ (printDims dim1 dim2),
     "    " ++ arrayName ++ " = " ++ (printF90array array arrayName),
     "end subroutine " ++ name]
        where arrayName = "basisvals"
              dimension = getDimension evaluatedPoints
-- These arrays are indexed from zero, so:
              dim1      = (length array) `div` dimension - 1
              dim2      = dimension - 1
              array     = convertToColumnOrder evaluatedPoints

convertToColumnOrder :: [[Double]] -> [Double]
--------------------------------------------------------------------------------
-- converts some list of the form [[1,2],[3,4]] to [1,3,2,4] by mapping
-- 'extract the nth entry from each value of evaluatedPoints' over the indicies.
--------------------------------------------------------------------------------
convertToColumnOrder evaluatedPoints =
    concat $ map (\ n -> map (!! n) evaluatedPoints) [0..maxIndex]
    -- index from 0, not 1
        where maxIndex = length (head evaluatedPoints) - 1

--------------------------------------------------------------------------------
-- Print out an array nicely. example :
-- printF90array [1,2,3,4] myarray = (/ 1, &
--                                      3, &
--                                      2, &
--                                      4 /)
-- where the arrayName generates the correct number of spaces for it to line
-- up.
--------------------------------------------------------------------------------
printF90array :: [Double] -> String -> String
printF90array evaluatedPoints arrayName = "(/" ++ showFirst ++ showMiddle
                                          ++ showLast ++ "/)"
    where showFirst  = (show $ head evaluatedPoints) ++ ", &\n"
          showMiddle = concat $ map (\ evaluatedPoints -> ((spaces spaceNum)
                       ++ show evaluatedPoints ++ ", &\n"))
                       (tail $ init evaluatedPoints)
          showLast   = (spaces 18) ++ (show $ last evaluatedPoints)
          spaceNum   = (9 + (length arrayName))

getDimension :: [[Double]] -> Int
--------------------------------------------------------------------------------
-- get the dimensionality of the data.
--------------------------------------------------------------------------------
getDimension x = length $ (head x)

printDims :: Int -> Int -> String
--------------------------------------------------------------------------------
-- Print out the dimensions of a matrix, Fortran-90 style. example:
-- printDims 2 3 = (0:2,0:3)
-- printDims 2 0 = (0:2)
--------------------------------------------------------------------------------
printDims dim1 dim2
    | (dim2 == 0) = "(0:" ++ (show dim1) ++ ")"
    | otherwise = "(0:" ++ (show dim1) ++ ",0:" ++ (show dim2) ++ ")"

spaces :: Int -> String
--------------------------------------------------------------------------------
-- return an integer number of spaces.
--------------------------------------------------------------------------------
spaces x = take x $ repeat ' '