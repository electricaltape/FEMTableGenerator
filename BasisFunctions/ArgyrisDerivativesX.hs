-------------------------------------------------------------------------------
-- ArgyrisDerivativesX.hs
-- Author: David Wells <drwells@vt.edu>
-- Description: First derivatives of the Argyris functions in the x direction.
-------------------------------------------------------------------------------
module BasisFunctions.ArgyrisDerivativesX
( allFunctions,
  allFunctionNames
) where


allFunctions :: [([Double] -> [Double])]
allFunctions = [argyrisDerivativeX1, argyrisDerivativeX2,
                argyrisDerivativeX3, argyrisDerivativeX4,
                argyrisDerivativeX5, argyrisDerivativeX6,
                argyrisDerivativeX7, argyrisDerivativeX8,
                argyrisDerivativeX9, argyrisDerivativeX10,
                argyrisDerivativeX11, argyrisDerivativeX12,
                argyrisDerivativeX13, argyrisDerivativeX14,
                argyrisDerivativeX15, argyrisDerivativeX16,
                argyrisDerivativeX17, argyrisDerivativeX18,
                argyrisDerivativeX19, argyrisDerivativeX20,
                argyrisDerivativeX21]

allFunctionNames :: [String]
allFunctionNames = ["argyrisDerivativeX1", "argyrisDerivativeX2",
                    "argyrisDerivativeX3", "argyrisDerivativeX4",
                    "argyrisDerivativeX5", "argyrisDerivativeX6",
                    "argyrisDerivativeX7", "argyrisDerivativeX8",
                    "argyrisDerivativeX9", "argyrisDerivativeX10",
                    "argyrisDerivativeX11", "argyrisDerivativeX12",
                    "argyrisDerivativeX13", "argyrisDerivativeX14",
                    "argyrisDerivativeX15", "argyrisDerivativeX16",
                    "argyrisDerivativeX17", "argyrisDerivativeX18",
                    "argyrisDerivativeX19", "argyrisDerivativeX20",
                    "argyrisDerivativeX21"]


argyrisDerivativeX1 :: [Double] -> [Double]
argyrisDerivativeX1 z = [-30*x^4 + 90*x^2*y^2 + 60*x*y^3 + 60*x^3 - 60*x*y^2 - 30*x^2]
                        where [x,y] = z

argyrisDerivativeX2 :: [Double] -> [Double]
argyrisDerivativeX2 z = [30*x^4 - 45*x^2*y^2 - 30*x*y^3 - 60*x^3 + 30*x*y^2 + 30*x^2]
                        where [x,y] = z

argyrisDerivativeX3 :: [Double] -> [Double]
argyrisDerivativeX3 z = [-45*x^2*y^2 - 30*x*y^3 + 30*x*y^2]
                        where [x,y] = z

argyrisDerivativeX4 :: [Double] -> [Double]
argyrisDerivativeX4 z = [-15*x^4 + 3*x^2*y^2 - 20*x*y^3 + 32*x^3 - 8*y^4 + 20*x*y^2 - 18*x^2 + 18*y^3 - 11*y^2 + 1]
                        where [x,y] = z

argyrisDerivativeX5 :: [Double] -> [Double]
argyrisDerivativeX5 z = [-32*x^3*y - 30*x^2*y^2 + 2*x*y^3 + 54*x^2*y + 20*x*y^2 - 22*x*y]
                        where [x,y] = z

argyrisDerivativeX6 :: [Double] -> [Double]
argyrisDerivativeX6 z = [-15*x^4 + 10.50*x^2*y^2 + 7.00*x*y^3 + 28*x^3 - 7.00*x*y^2 - 12*x^2]
                        where [x,y] = z

argyrisDerivativeX7 :: [Double] -> [Double]
argyrisDerivativeX7 z = [-32*x^3*y - 55.50*x^2*y^2 - 27.0*x*y^3 + 42*x^2*y + 37.0*x*y^2 - 10*x*y]
                        where [x,y] = z

argyrisDerivativeX8 :: [Double] -> [Double]
argyrisDerivativeX8 z = [-40.50*x^2*y^2 - 37.0*x*y^3 - 8*y^4 + 37.0*x*y^2 + 14*y^3 - 5*y^2]
                        where [x,y] = z

argyrisDerivativeX9 :: [Double] -> [Double]
argyrisDerivativeX9 z = [10.50*x^2*y^2 + 7.00*x*y^3 - 7.00*x*y^2]
                        where [x,y] = z

argyrisDerivativeX10 :: [Double] -> [Double]
argyrisDerivativeX10 z = [-2.50*x^4 + 4.50*x^2*y^2 + 2*x*y^3 + 6.00*x^3 - 3.00*x*y^2 - 4.50*x^2 + x]
                         where [x,y] = z

argyrisDerivativeX11 :: [Double] -> [Double]
argyrisDerivativeX11 z = [-8*x^3*y - 18*x^2*y^2 - 12*x*y^3 - 2*y^4 + 15*x^2*y + 20*x*y^2 + 5*y^3 - 8*x*y - 4*y^2 + y]
                         where [x,y] = z

argyrisDerivativeX12 :: [Double] -> [Double]
argyrisDerivativeX12 z = [3*x^2*y^2 + 3.00*x*y^3 - 3.00*x*y^2]
                         where [x,y] = z

argyrisDerivativeX13 :: [Double] -> [Double]
argyrisDerivativeX13 z = [2.50*x^4 - 0.750*x^2*y^2 - 0.500*x*y^3 - 4*x^3 + 0.500*x*y^2 + 1.50*x^2]
                         where [x,y] = z

argyrisDerivativeX14 :: [Double] -> [Double]
argyrisDerivativeX14 z = [8*x^3*y + 10.50*x^2*y^2 + 5.00*x*y^3 - 9*x^2*y - 7.00*x*y^2 + 2*x*y]
                         where [x,y] = z

argyrisDerivativeX15 :: [Double] -> [Double]
argyrisDerivativeX15 z = [-2.250*x^2*y^2 - 2.50*x*y^3 + 2.50*x*y^2]
                         where [x,y] = z

argyrisDerivativeX16 :: [Double] -> [Double]
argyrisDerivativeX16 z = [-3.750*x^2*y^2 - 1.50*x*y^3 + 2.50*x*y^2]
                         where [x,y] = z

argyrisDerivativeX17 :: [Double] -> [Double]
argyrisDerivativeX17 z = [7.50*x^2*y^2 + 7.00*x*y^3 + 2*y^4 - 7.000*x*y^2 - 3*y^3 + y^2]
                         where [x,y] = z

argyrisDerivativeX18 :: [Double] -> [Double]
argyrisDerivativeX18 z = [-0.7500*x^2*y^2 - 0.5000*x*y^3 + 0.5000*x*y^2]
                         where [x,y] = z

argyrisDerivativeX19 :: [Double] -> [Double]
argyrisDerivativeX19 z = [8*(3*x^2*y^2 + 2*x*y^3 - 2*x*y^2)*sqrt(2)]
                         where [x,y] = z

argyrisDerivativeX20 :: [Double] -> [Double]
argyrisDerivativeX20 z = [-48*x^2*y^2 - 64*x*y^3 - 16*y^4 + 64*x*y^2 + 32*y^3 - 16*y^2]
                         where [x,y] = z

argyrisDerivativeX21 :: [Double] -> [Double]
argyrisDerivativeX21 z = [-64*x^3*y - 96*x^2*y^2 - 32*x*y^3 + 96*x^2*y + 64*x*y^2 - 32*x*y]
                         where [x,y] = z
