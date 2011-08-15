-------------------------------------------------------------------------------
-- File: QuadraticGradients.hs
-- Author: David Wells
-- Description: Gradients of the quadratic basis functions.
-------------------------------------------------------------------------------

module BasisFunctions.QuadraticGradients
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

allquadraticsStrings = ["quadraticGrad0", "quadraticGrad1", "quadraticGrad2", 
                        "quadraticGrad3", "quadraticGrad4", "quadraticGrad5"]

quadratic0 z = [2*(y + x - 1) + 2*(y + x - 0.5),
                     2*(y + x - 1) + 2*(y + x - 0.5)]
    where [x, y] = z

quadratic1 z = [2*x + 2*(x - 0.5), 0]
    where [x, y] = z

quadratic2 z = [0, 2*y + 2*(y - 0.5)]
    where [x, y] = z

quadratic3 z = [-4*(y + x - 1) - 4*x, -4*x]
    where [x, y] = z

quadratic4 z = [4*y, 4*x]
    where [x, y] = z

quadratic5 z = [-4*y, -4*(y + x - 1) - 4*y]
    where [x, y] = z
