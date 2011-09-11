-------------------------------------------------------------------------------
-- File: Quadratics.hs
-- Author: David Wells
-- Description: Module containing the quadratic basis functions.
-------------------------------------------------------------------------------

module BasisFunctions.Quadratics
( allquadratics
, allquadraticsStrings
) where

allquadratics :: [([Double] -> [Double])]
allquadratics = [quadratic0, quadratic1, quadratic2, quadratic3,
                 quadratic4, quadratic5]

allquadraticsStrings :: [String]
allquadraticsStrings = ["quadratic0", "quadratic1", "quadratic2",
                        "quadratic3", "quadratic4", "quadratic5"]

quadratic0 :: [Double] -> [Double]
quadratic0 z = [2*(y + x - 1) * (y + x - 0.5)]
    where [x, y] = z

quadratic1 :: [Double] -> [Double]
quadratic1 z = [2*x * (x - 0.5)]
    where [x, y] = z

quadratic2 :: [Double] -> [Double]
quadratic2 z = [2*y* (y - 0.5)]
    where [x, y] = z

quadratic3 :: [Double] -> [Double]
quadratic3 z = [-4*x*(y + x - 1)]
    where [x, y] = z

quadratic4 :: [Double] -> [Double]
quadratic4 z = [4*x*y]
    where [x, y] = z

quadratic5 :: [Double] -> [Double]
quadratic5 z = [-4*y * (y + x - 1)]
    where [x, y] = z
