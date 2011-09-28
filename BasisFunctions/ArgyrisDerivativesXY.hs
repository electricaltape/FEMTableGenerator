-------------------------------------------------------------------------------
-- ArgyrisDerivativesXY.hs
-- Author: David Wells <drwells@vt.edu>
-- Description: First derivatives of the Argyris functions in the x direction.
-------------------------------------------------------------------------------
module BasisFunctions.ArgyrisDerivativesXY
( allFunctions,
  allFunctionNames
) where

allFunctions :: [([Double] -> [Double])]
allFunctions = [argyrisDerivativeXY1, argyrisDerivativeXY2,
                argyrisDerivativeXY3, argyrisDerivativeXY4,
                argyrisDerivativeXY5, argyrisDerivativeXY6,
                argyrisDerivativeXY7, argyrisDerivativeXY8,
                argyrisDerivativeXY9, argyrisDerivativeXY10,
                argyrisDerivativeXY11, argyrisDerivativeXY12,
                argyrisDerivativeXY13, argyrisDerivativeXY14,
                argyrisDerivativeXY15, argyrisDerivativeXY16,
                argyrisDerivativeXY17, argyrisDerivativeXY18,
                argyrisDerivativeXY19, argyrisDerivativeXY20,
                argyrisDerivativeXY21]

allFunctionNames :: [String]
allFunctionNames = ["argyrisDerivativeXY1", "argyrisDerivativeXY2",
                    "argyrisDerivativeXY3", "argyrisDerivativeXY4",
                    "argyrisDerivativeXY5", "argyrisDerivativeXY6",
                    "argyrisDerivativeXY7", "argyrisDerivativeXY8",
                    "argyrisDerivativeXY9", "argyrisDerivativeXY10",
                    "argyrisDerivativeXY11", "argyrisDerivativeXY12",
                    "argyrisDerivativeXY13", "argyrisDerivativeXY14",
                    "argyrisDerivativeXY15", "argyrisDerivativeXY16",
                    "argyrisDerivativeXY17", "argyrisDerivativeXY18",
                    "argyrisDerivativeXY19", "argyrisDerivativeXY20",
                    "argyrisDerivativeXY21"]

argyrisDerivativeXY1 :: [Double] -> [Double]
argyrisDerivativeXY1 z =[180*x^2*y + 180*x*y^2 - 120*x*y]
                        where [x,y] = z

argyrisDerivativeXY2 :: [Double] -> [Double]
argyrisDerivativeXY2 z =[-90*x^2*y - 90*x*y^2 + 60*x*y]
                        where [x,y] = z

argyrisDerivativeXY3 :: [Double] -> [Double]
argyrisDerivativeXY3 z =[-90*x^2*y - 90*x*y^2 + 60*x*y]
                        where [x,y] = z

argyrisDerivativeXY4 :: [Double] -> [Double]
argyrisDerivativeXY4 z =[6*x^2*y - 60*x*y^2 - 32*y^3 + 40*x*y + 54*y^2 - 22*y]
                        where [x,y] = z

argyrisDerivativeXY5 :: [Double] -> [Double]
argyrisDerivativeXY5 z =[-32*x^3 - 60*x^2*y + 6*x*y^2 + 54*x^2 + 40*x*y - 22*x]
                        where [x,y] = z

argyrisDerivativeXY6 :: [Double] -> [Double]
argyrisDerivativeXY6 z =[21.0*x^2*y + 21.0*x*y^2 - 14.0*x*y]
                        where [x,y] = z

argyrisDerivativeXY7 :: [Double] -> [Double]
argyrisDerivativeXY7 z =[-32*x^3 - 111.0*x^2*y - 81.0*x*y^2 + 42*x^2 + 74.0*x*y - 10*x]
                        where [x,y] = z

argyrisDerivativeXY8 :: [Double] -> [Double]
argyrisDerivativeXY8 z =[-81.0*x^2*y - 111.0*x*y^2 - 32*y^3 + 74.0*x*y + 42*y^2 - 10*y]
                        where [x,y] = z

argyrisDerivativeXY9 :: [Double] -> [Double]
argyrisDerivativeXY9 z =[21.0*x^2*y + 21.0*x*y^2 - 14.0*x*y]
                        where [x,y] = z

argyrisDerivativeXY10 :: [Double] -> [Double]
argyrisDerivativeXY10 z =[9.00*x^2*y + 6*x*y^2 - 6.00*x*y]
                         where [x,y] = z

argyrisDerivativeXY11 :: [Double] -> [Double]
argyrisDerivativeXY11 z =[-8*x^3 - 36*x^2*y - 36*x*y^2 + 15*x^2 - 8*y^3 + 40*x*y + 15*y^2 - 8*x - 8*y + 1]
                         where [x,y] = z

argyrisDerivativeXY12 :: [Double] -> [Double]
argyrisDerivativeXY12 z =[6*x^2*y + 9.00*x*y^2 - 6.00*x*y]
                         where [x,y] = z

argyrisDerivativeXY13 :: [Double] -> [Double]
argyrisDerivativeXY13 z =[-1.50*x^2*y - 1.50*x*y^2 + x*y]
                         where [x,y] = z

argyrisDerivativeXY14 :: [Double] -> [Double]
argyrisDerivativeXY14 z =[8*x^3 + 21.0*x^2*y + 15.0*x*y^2 - 9*x^2 - 14.0*x*y + 2*x]
                         where [x,y] = z

argyrisDerivativeXY15 :: [Double] -> [Double]
argyrisDerivativeXY15 z =[-4.50*x^2*y - 7.50*x*y^2 + 5.00*x*y]
                         where [x,y] = z

argyrisDerivativeXY16 :: [Double] -> [Double]
argyrisDerivativeXY16 z =[-7.50*x^2*y - 4.50*x*y^2 + 5.00*x*y]
                         where [x,y] = z

argyrisDerivativeXY17 :: [Double] -> [Double]
argyrisDerivativeXY17 z =[15.0*x^2*y + 21.0*x*y^2 + 8*y^3 - 14.0*x*y - 9*y^2 + 2*y]
                         where [x,y] = z

argyrisDerivativeXY18 :: [Double] -> [Double]
argyrisDerivativeXY18 z =[-1.50*x^2*y - 1.50*x*y^2 + x*y]
                         where [x,y] = z

argyrisDerivativeXY19 :: [Double] -> [Double]
argyrisDerivativeXY19 z =[16*(3*x^2*y + 3*x*y^2 - 2*x*y)*sqrt(2)]
                         where [x,y] = z

argyrisDerivativeXY20 :: [Double] -> [Double]
argyrisDerivativeXY20 z =[-96*x^2*y - 192*x*y^2 - 64*y^3 + 128*x*y + 96*y^2 - 32*y]
                         where [x,y] = z

argyrisDerivativeXY21 :: [Double] -> [Double]
argyrisDerivativeXY21 z =[-64*x^3 - 192*x^2*y - 96*x*y^2 + 96*x^2 + 128*x*y - 32*x]
                         where [x,y] = z
