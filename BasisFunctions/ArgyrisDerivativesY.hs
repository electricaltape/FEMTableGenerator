-------------------------------------------------------------------------------
-- ArgyrisDerivativesY.hs
-- Author: David Wells <drwells@vt.edu>
-- Description: First derivatives of the Argyris functions in the x direction.
-------------------------------------------------------------------------------
module BasisFunctions.ArgyrisDerivativesY
( allFunctions,
  allFunctionNames
) where


allFunctions :: [([Double] -> [Double])]
allFunctions = [argyrisDerivativeY1, argyrisDerivativeY2,
                argyrisDerivativeY3, argyrisDerivativeY4,
                argyrisDerivativeY5, argyrisDerivativeY6,
                argyrisDerivativeY7, argyrisDerivativeY8,
                argyrisDerivativeY9, argyrisDerivativeY10,
                argyrisDerivativeY11, argyrisDerivativeY12,
                argyrisDerivativeY13, argyrisDerivativeY14,
                argyrisDerivativeY15, argyrisDerivativeY16,
                argyrisDerivativeY17, argyrisDerivativeY18,
                argyrisDerivativeY19, argyrisDerivativeY20,
                argyrisDerivativeY21]

allFunctionNames :: [String]
allFunctionNames = ["argyrisDerivativeY1", "argyrisDerivativeY2",
                    "argyrisDerivativeY3", "argyrisDerivativeY4",
                    "argyrisDerivativeY5", "argyrisDerivativeY6",
                    "argyrisDerivativeY7", "argyrisDerivativeY8",
                    "argyrisDerivativeY9", "argyrisDerivativeY10",
                    "argyrisDerivativeY11", "argyrisDerivativeY12",
                    "argyrisDerivativeY13", "argyrisDerivativeY14",
                    "argyrisDerivativeY15", "argyrisDerivativeY16",
                    "argyrisDerivativeY17", "argyrisDerivativeY18",
                    "argyrisDerivativeY19", "argyrisDerivativeY20",
                    "argyrisDerivativeY21"]


argyrisDerivativeY1 :: [Double] -> [Double]
argyrisDerivativeY1 z = [60*x^3*y + 90*x^2*y^2 - 30*y^4 - 60*x^2*y + 60*y^3 - 30*y^2]
                        where [x,y] = z

argyrisDerivativeY2 :: [Double] -> [Double]
argyrisDerivativeY2 z = [-30*x^3*y - 45*x^2*y^2 + 30*x^2*y]
                        where [x,y] = z

argyrisDerivativeY3 :: [Double] -> [Double]
argyrisDerivativeY3 z = [-30*x^3*y - 45*x^2*y^2 + 30*y^4 + 30*x^2*y - 60*y^3 + 30*y^2]
                        where [x,y] = z

argyrisDerivativeY4 :: [Double] -> [Double]
argyrisDerivativeY4 z = [2*x^3*y - 30*x^2*y^2 - 32*x*y^3 + 20*x^2*y + 54*x*y^2 - 22*x*y]
                        where [x,y] = z

argyrisDerivativeY5 :: [Double] -> [Double]
argyrisDerivativeY5 z = [-8*x^4 - 20*x^3*y + 3*x^2*y^2 + 18*x^3 - 15*y^4 + 20*x^2*y - 11*x^2 + 32*y^3 - 18*y^2 + 1]
                        where [x,y] = z

argyrisDerivativeY6 :: [Double] -> [Double]
argyrisDerivativeY6 z = [7.00*x^3*y + 10.50*x^2*y^2 - 7.00*x^2*y]
                        where [x,y] = z

argyrisDerivativeY7 :: [Double] -> [Double]
argyrisDerivativeY7 z = [-8*x^4 - 37.0*x^3*y - 40.50*x^2*y^2 + 14*x^3 + 37.0*x^2*y - 5*x^2]
                        where [x,y] = z

argyrisDerivativeY8 :: [Double] -> [Double]
argyrisDerivativeY8 z = [-27.0*x^3*y - 55.50*x^2*y^2 - 32*x*y^3 + 37.0*x^2*y + 42*x*y^2 - 10*x*y]
                        where [x,y] = z

argyrisDerivativeY9 :: [Double] -> [Double]
argyrisDerivativeY9 z = [7.00*x^3*y + 10.50*x^2*y^2 - 15*y^4 - 7.00*x^2*y + 28*y^3 - 12*y^2]
                        where [x,y] = z

argyrisDerivativeY10 :: [Double] -> [Double]
argyrisDerivativeY10 z =[3.00*x^3*y + 3*x^2*y^2 - 3.00*x^2*y]
                         where [x,y] = z

argyrisDerivativeY11 :: [Double] -> [Double]
argyrisDerivativeY11 z =[-2*x^4 - 12*x^3*y - 18*x^2*y^2 - 8*x*y^3 + 5*x^3 + 20*x^2*y + 15*x*y^2 - 4*x^2 - 8*x*y + x]
                         where [x,y] = z

argyrisDerivativeY12 :: [Double] -> [Double]
argyrisDerivativeY12 z =[2*x^3*y + 4.50*x^2*y^2 - 2.50*y^4 - 3.00*x^2*y + 6.00*y^3 - 4.50*y^2 + y]
                         where [x,y] = z

argyrisDerivativeY13 :: [Double] -> [Double]
argyrisDerivativeY13 z =[-0.500*x^3*y - 0.750*x^2*y^2 + 0.500*x^2*y]
                         where [x,y] = z

argyrisDerivativeY14 :: [Double] -> [Double]
argyrisDerivativeY14 z =[2*x^4 + 7.00*x^3*y + 7.50*x^2*y^2 - 3*x^3 - 7.00*x^2*y + x^2]
                         where [x,y] = z

argyrisDerivativeY15 :: [Double] -> [Double]
argyrisDerivativeY15 z =[-1.50*x^3*y - 3.750*x^2*y^2 + 2.50*x^2*y]
                         where [x,y] = z

argyrisDerivativeY16 :: [Double] -> [Double]
argyrisDerivativeY16 z =[-2.50*x^3*y - 2.250*x^2*y^2 + 2.50*x^2*y]
                         where [x,y] = z

argyrisDerivativeY17 :: [Double] -> [Double]
argyrisDerivativeY17 z =[5.00*x^3*y + 10.50*x^2*y^2 + 8*x*y^3 - 7.00*x^2*y - 9*x*y^2 + 2*x*y]
                         where [x,y] = z

argyrisDerivativeY18 :: [Double] -> [Double]
argyrisDerivativeY18 z =[-0.500*x^3*y - 0.750*x^2*y^2 + 2.50*y^4 + 0.500*x^2*y - 4*y^3 + 1.50*y^2]
                         where [x,y] = z

argyrisDerivativeY19 :: [Double] -> [Double]
argyrisDerivativeY19 z =[8*(2*x^3*y + 3*x^2*y^2 - 2*x^2*y)*(sqrt 2)]
                         where [x,y] = z

argyrisDerivativeY20 :: [Double] -> [Double]
argyrisDerivativeY20 z =[-32*x^3*y - 96*x^2*y^2 - 64*x*y^3 + 64*x^2*y + 96*x*y^2 - 32*x*y]
                         where [x,y] = z

argyrisDerivativeY21 :: [Double] -> [Double]
argyrisDerivativeY21 z =[-16*x^4 - 64*x^3*y - 48*x^2*y^2 + 32*x^3 + 64*x^2*y - 16*x^2]
                         where [x,y] = z
