------------------------------------------------------------------------------
-- PrintM.hs
-- Author: David Wells <drwells@vt.edu>
-- Description: print out arrays as MATLAB data.
-------------------------------------------------------------------------------

module FormatArrays.PrintM
( printOutput,
  printMArray,
  printArrayFunc
) where

import Data.List

printOutput :: [[[Double]]] -> [String] -> String
--------------------------------------------------------------------------------
-- given a look-up list return a single MATLAB array representation.
--------------------------------------------------------------------------------
printOutput evaluatedFunctions funcNames =
  concat $ zipWith printArrayFunc funcNames evaluatedFunctions

printArrayFunc :: String -> [[Double]] -> String
--------------------------------------------------------------------------------
-- Given an array name and array, print out a function that will
-- return said array.
--------------------------------------------------------------------------------
printArrayFunc name evaluatedPoints = unlines $
    ["function [" ++ arrayName ++ "] = " ++ name ++ "()",
    "    " ++ arrayName ++ " = " ++ (printMArray evaluatedPoints arrayName)
           ++ ";\n"]
    where arrayName = "basisPoints"

printMArray :: [[Double]] -> String -> String
--------------------------------------------------------------------------------
-- Take the look-up list generated in Haskell and convert its textual
-- representation to that of a MATLAB array.
--------------------------------------------------------------------------------
printMArray evaluatedPoints arrayName =
    "[" ++ showFirstRow ++ showMiddle ++ showLastRow ++ "]"
  where stringForm = [map show singleEntry | singleEntry <- evaluatedPoints]
        showFirstRow  = (intercalate ", " (head stringForm)) ++ ";\n"
        -- check if there is a middle section to print. Otherwise return ""
        showMiddle = if ((not . null) $ (init $ tail evaluatedPoints)) then
                        concat [spaces spaceNum ++ intercalate ", " row ++ ";\n"
                               | row <- init $ tail stringForm]
                     else "skipped middle."
        showLastRow   = spaces spaceNum ++ (intercalate ", " (last stringForm))
        -- amount to indent data in second to last row. Cosmetic only.
        spaceNum   = length arrayName + 4 + 4

spaces :: Int -> String
--------------------------------------------------------------------------------
-- return an integer number of spaces.
--------------------------------------------------------------------------------
spaces x = take x $ repeat ' '