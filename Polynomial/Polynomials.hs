-------------------------------------------------------------------------------
-- Polynomials.hs
-- Author: David Wells <drwells@vt.edu>
-- Description: data and manipulating functions for polynomials.
-------------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}

module Polynomial.Polynomials
( exportGradientsAndPolynomials,
  exportGradients,
  exportPolynomials
) where

import Data.List

-- store a 3D polynomial as (coefficient, x-power, y-power, z-power)
type PolynomialTerm = (Double, Int, Int, Int)
type Polynomial = [PolynomialTerm]

exportGradientsAndPolynomials :: String -> ([String], [[Double] -> [Double]])
exportGradientsAndPolynomials poly =
  (polyNames ++ gradNames, polyFunctions ++ gradFunctions)
    where (polyNames, polyFunctions) = exportPolynomials poly
          (gradNames, gradFunctions) = exportGradients poly

-- Read in a list of polynomials and convert them to Haskell functions. Return
-- names as well as functions.
exportPolynomials :: String -> ([String], [[Double] -> [Double]])
exportPolynomials polyList = (functionNames, functions)
    where functions     = map scalarPolynomialToFunction polyListRead
          functionNames =
            map ("basisFunction" ++) $ map show [0..(length polyListRead - 1)]
          polyListRead = read polyList :: [Polynomial]

exportGradients :: String -> ([String], [[Double] -> [Double]])
exportGradients polyList = (functionNames, functions)
    where functions     =
            map (vectorPolynomialToFunction . gradient) polyListRead
          functionNames =
            map ("basisGradient" ++) $ map show [0..(length polyListRead - 1)]
          polyListRead = read polyList :: [Polynomial]

-- instance Show Polynomial where
--   show polynomial = concat $ map showPolynomialTerm polynomial
--     where showPolynomialTerm (a,b,c,d) = intercalate " "
--             [show a, "x^" ++ show b, "y^" ++ show c, "z^" ++ show d] ++ " "

-- Take a Polynomial and convert it to a function.
scalarPolynomialToFunction :: Polynomial -> ([Double] -> [Double])
scalarPolynomialToFunction poly = \ [x,y] ->
  [sum $ map (\ (c, xPower, yPower, _) -> c * x^xPower * y^yPower) poly]

-- Take a Polynomial and convert it to a function.
vectorPolynomialToFunction :: (Polynomial, Polynomial) -> ([Double] -> [Double])
vectorPolynomialToFunction (xpoly, ypoly) = \ [x,y] ->
  map (\ poly -> sum $ map (\ (c, xPower, yPower, _) -> c * x^xPower * y^yPower) poly)
                [xpoly, ypoly]

-- Find the gradient of the polynomial.
gradient :: Polynomial -> (Polynomial, Polynomial)
gradient poly = (xDerivative, yDerivative)
    where xDerivative = clearZeros $ map xDerivativeTerm poly
          yDerivative = clearZeros $ map yDerivativeTerm poly

xDerivativeTerm :: PolynomialTerm -> PolynomialTerm
xDerivativeTerm (a,0,c,d) = (0,0,0,0)
xDerivativeTerm (a,b,c,d) = (a * fromIntegral b, b - 1, c, d)

yDerivativeTerm :: PolynomialTerm -> PolynomialTerm
yDerivativeTerm (a,b,0,d) = (0,0,0,0)
yDerivativeTerm (a,b,c,d) = (a * fromIntegral c,b, c - 1, d)

-- Clear the zero terms from a polynomial.
clearZeros :: Polynomial -> Polynomial
clearZeros = filter (\ (a,_,_,_) -> (a /= 0.0))

-- test case.
linears :: String
linears = "[[((1), 0, 0, 0), ((-1), 1, 0, 0), ((-1), 0, 1, 0)]," ++
          "[((0), 0, 0, 0), ((1), 1, 0, 0),((0), 0, 1, 0)], " ++
          "[((0), 0, 0, 0), ((0), 1, 0, 0), ((1), 0, 1, 0)]]"