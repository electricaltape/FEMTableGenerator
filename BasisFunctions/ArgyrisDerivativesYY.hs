-------------------------------------------------------------------------------
-- ArgyrisDerivativesYY.hs
-- Author: David Wells <drwells@vt.edu>
-- Description: Second derivatives of the Argyris functions in the y direction.
-------------------------------------------------------------------------------
module BasisFunctions.ArgyrisDerivativesYY
( allFunctions,
  allFunctionNames
) where

allFunctions :: [([Double] -> [Double])]
allFunctions = [argyrisDerivativeYY1, argyrisDerivativeYY2,
                argyrisDerivativeYY3, argyrisDerivativeYY4,
                argyrisDerivativeYY5, argyrisDerivativeYY6,
                argyrisDerivativeYY7, argyrisDerivativeYY8,
                argyrisDerivativeYY9, argyrisDerivativeYY10,
                argyrisDerivativeYY11, argyrisDerivativeYY12,
                argyrisDerivativeYY13, argyrisDerivativeYY14,
                argyrisDerivativeYY15, argyrisDerivativeYY16,
                argyrisDerivativeYY17, argyrisDerivativeYY18,
                argyrisDerivativeYY19, argyrisDerivativeYY20,
                argyrisDerivativeYY21]

allFunctionNames :: [String]
allFunctionNames = ["argyrisDerivativeYY1", "argyrisDerivativeYY2",
                   "argyrisDerivativeYY3", "argyrisDerivativeYY4",
                   "argyrisDerivativeYY5", "argyrisDerivativeYY6",
                   "argyrisDerivativeYY7", "argyrisDerivativeYY8",
                   "argyrisDerivativeYY9", "argyrisDerivativeYY10",
                   "argyrisDerivativeYY11", "argyrisDerivativeYY12",
                   "argyrisDerivativeYY13", "argyrisDerivativeYY14",
                   "argyrisDerivativeYY15", "argyrisDerivativeYY16",
                   "argyrisDerivativeYY17", "argyrisDerivativeYY18",
                   "argyrisDerivativeYY19", "argyrisDerivativeYY20",
                   "argyrisDerivativeYY21"]

argyrisDerivativeYY1 :: [Double] -> [Double]
argyrisDerivativeYY1 z =[60*x^3 + 180*x^2*y - 60*x^2 - 120*y^3 + 180*y^2 - 60*y]
                        where [x,y] = z

argyrisDerivativeYY2 :: [Double] -> [Double]
argyrisDerivativeYY2 z =[-30*x^3 - 90*x^2*y + 30*x^2]
                        where [x,y] = z

argyrisDerivativeYY3 :: [Double] -> [Double]
argyrisDerivativeYY3 z =[-30*x^3 - 90*x^2*y + 30*x^2 + 120*y^3 - 180*y^2 + 60*y]
                        where [x,y] = z

argyrisDerivativeYY4 :: [Double] -> [Double]
argyrisDerivativeYY4 z =[2*x^3 - 60*x^2*y - 96*x*y^2 + 20*x^2 + 108*x*y - 22*x]
                        where [x,y] = z

argyrisDerivativeYY5 :: [Double] -> [Double]
argyrisDerivativeYY5 z =[-20*x^3 + 6*x^2*y + 20*x^2 - 60*y^3 + 96*y^2 - 36*y]
                        where [x,y] = z

argyrisDerivativeYY6 :: [Double] -> [Double]
argyrisDerivativeYY6 z =[7.00*x^3 + 21.0*x^2*y - 7.00*x^2]
                        where [x,y] = z

argyrisDerivativeYY7 :: [Double] -> [Double]
argyrisDerivativeYY7 z =[-37.0*x^3 - 81.0*x^2*y + 37.0*x^2]
                        where [x,y] = z

argyrisDerivativeYY8 :: [Double] -> [Double]
argyrisDerivativeYY8 z =[-27.0*x^3 - 111.0*x^2*y - 96*x*y^2 + 37.0*x^2 + 84*x*y - 10*x]
                        where [x,y] = z

argyrisDerivativeYY9 :: [Double] -> [Double]
argyrisDerivativeYY9 z =[7.00*x^3 + 21.0*x^2*y - 7.00*x^2 - 60*y^3 + 84*y^2 - 24*y]
                        where [x,y] = z

argyrisDerivativeYY10 :: [Double] -> [Double]
argyrisDerivativeYY10 z =[3.00*x^3 + 6*x^2*y - 3.00*x^2]
                         where [x,y] = z

argyrisDerivativeYY11 :: [Double] -> [Double]
argyrisDerivativeYY11 z =[-12*x^3 - 36*x^2*y - 24*x*y^2 + 20*x^2 + 30*x*y - 8*x]
                         where [x,y] = z

argyrisDerivativeYY12 :: [Double] -> [Double]
argyrisDerivativeYY12 z =[2*x^3 + 9.00*x^2*y - 3.00*x^2 - 10.0*y^3 + 18.0*y^2 - 9.00*y + 1]
                         where [x,y] = z

argyrisDerivativeYY13 :: [Double] -> [Double]
argyrisDerivativeYY13 z =[-0.500*x^3 - 1.50*x^2*y + 0.500*x^2]
                         where [x,y] = z

argyrisDerivativeYY14 :: [Double] -> [Double]
argyrisDerivativeYY14 z =[7.00*x^3 + 15.0*x^2*y - 7.00*x^2]
                         where [x,y] = z

argyrisDerivativeYY15 :: [Double] -> [Double]
argyrisDerivativeYY15 z =[-1.50*x^3 - 7.50*x^2*y + 2.50*x^2]
                         where [x,y] = z

argyrisDerivativeYY16 :: [Double] -> [Double]
argyrisDerivativeYY16 z =[-2.50*x^3 - 4.50*x^2*y + 2.50*x^2]
                         where [x,y] = z

argyrisDerivativeYY17 :: [Double] -> [Double]
argyrisDerivativeYY17 z =[5.00*x^3 + 21.0*x^2*y + 24*x*y^2 - 7.00*x^2 - 18*x*y + 2*x]
                         where [x,y] = z

argyrisDerivativeYY18 :: [Double] -> [Double]
argyrisDerivativeYY18 z =[-0.500*x^3 - 1.50*x^2*y + 0.500*x^2 + 10.0*y^3 - 12*y^2 + 3.00*y]
                         where [x,y] = z

argyrisDerivativeYY19 :: [Double] -> [Double]
argyrisDerivativeYY19 z =[16*(x^3 + 3*x^2*y - x^2)*(sqrt 2)]
                         where [x,y] = z

argyrisDerivativeYY20 :: [Double] -> [Double]
argyrisDerivativeYY20 z =[-32*x^3 - 192*x^2*y - 192*x*y^2 + 64*x^2 + 192*x*y - 32*x]
                         where [x,y] = z

argyrisDerivativeYY21 :: [Double] -> [Double]
argyrisDerivativeYY21 z =[-64*x^3 - 96*x^2*y + 64*x^2]
                         where [x,y] = z
