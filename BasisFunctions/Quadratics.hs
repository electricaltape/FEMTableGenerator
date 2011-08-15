-------------------------------------------------------------------------------
-- File: Quadratics.hs
-- Author: David Wells
-- Description: Module containing the quadratic basis functions.
-------------------------------------------------------------------------------

module BasisFunctions.Quadratics
( allquadratics
, allquadraticsStrings
, quadratic0
, quadratic1
, quadratic2
, quadratic3
, quadratic4
, quadratic5
) where

allquadratics = [quadratic0, quadratic1, quadratic2, quadratic3, 
                 quadratic4, quadratic5]

allquadraticsStrings = ["quadratic0", "quadratic1", "quadratic2", 
                        "quadratic3", "quadratic4", "quadratic5"]

quadratic0 z = [2*(y + x - 1) * (y + x - 0.5)]
    where [x, y] = z

quadratic1 z = [2*x * (x - 0.5)]
    where [x, y] = z

quadratic2 z = [2*y* (y - 0.5)]
    where [x, y] = z

quadratic3 z = [-4*x*(y + x - 1)]
    where [x, y] = z

quadratic4 z = [4*x*y]
    where [x, y] = z

quadratic5 z = [-4*y * (y + x - 1)]
    where [x, y] = z
