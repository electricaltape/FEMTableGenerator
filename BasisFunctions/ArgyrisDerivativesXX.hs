-------------------------------------------------------------------------------
-- ArgyrisDerivativesXX.hs
-- Author: David Wells <drwells@vt.edu>
-- Description: Second derivatives of the Argyris functions in the x direction.
-------------------------------------------------------------------------------
module BasisFunctions.ArgyrisDerivativesXX
( allFunctions,
  allFunctionNames
) where

allFunctions :: [([Double] -> [Double])]
allFunctions = [argyrisDerivativeXX1, argyrisDerivativeXX2,
                argyrisDerivativeXX3, argyrisDerivativeXX4,
                argyrisDerivativeXX5, argyrisDerivativeXX6,
                argyrisDerivativeXX7, argyrisDerivativeXX8,
                argyrisDerivativeXX9, argyrisDerivativeXX10,
                argyrisDerivativeXX11, argyrisDerivativeXX12,
                argyrisDerivativeXX13, argyrisDerivativeXX14,
                argyrisDerivativeXX15, argyrisDerivativeXX16,
                argyrisDerivativeXX17, argyrisDerivativeXX18,
                argyrisDerivativeXX19, argyrisDerivativeXX20,
                argyrisDerivativeXX21]

allFunctionNames :: [String]
allFunctionNames = ["argyrisDerivativeXX1", "argyrisDerivativeXX2",
                    "argyrisDerivativeXX3", "argyrisDerivativeXX4",
                    "argyrisDerivativeXX5", "argyrisDerivativeXX6",
                    "argyrisDerivativeXX7", "argyrisDerivativeXX8",
                    "argyrisDerivativeXX9", "argyrisDerivativeXX10",
                    "argyrisDerivativeXX11", "argyrisDerivativeXX12",
                    "argyrisDerivativeXX13", "argyrisDerivativeXX14",
                    "argyrisDerivativeXX15", "argyrisDerivativeXX16",
                    "argyrisDerivativeXX17", "argyrisDerivativeXX18",
                    "argyrisDerivativeXX19", "argyrisDerivativeXX20",
                    "argyrisDerivativeXX21"]


argyrisDerivativeXX1 :: [Double] -> [Double]
argyrisDerivativeXX1 z =[-120*x^3 + 180*x*y^2 + 180*x^2 + 60*y^3 - 60*y^2 - 60*x]
                        where [x,y] = z

argyrisDerivativeXX2 :: [Double] -> [Double]
argyrisDerivativeXX2 z =[120*x^3 - 90*x*y^2 - 180*x^2 - 30*y^3 + 30*y^2 + 60*x]
                        where [x,y] = z

argyrisDerivativeXX3 :: [Double] -> [Double]
argyrisDerivativeXX3 z =[-90*x*y^2 - 30*y^3 + 30*y^2]
                        where [x,y] = z

argyrisDerivativeXX4 :: [Double] -> [Double]
argyrisDerivativeXX4 z =[-60*x^3 + 6*x*y^2 + 96*x^2 - 20*y^3 + 20*y^2 - 36*x]
                        where [x,y] = z

argyrisDerivativeXX5 :: [Double] -> [Double]
argyrisDerivativeXX5 z =[-96*x^2*y - 60*x*y^2 + 2*y^3 + 108*x*y + 20*y^2 - 22*y]
                        where [x,y] = z

argyrisDerivativeXX6 :: [Double] -> [Double]
argyrisDerivativeXX6 z =[-60*x^3 + 21.0*x*y^2 + 84*x^2 + 7.00*y^3 - 7.00*y^2 - 24*x]
                        where [x,y] = z

argyrisDerivativeXX7 :: [Double] -> [Double]
argyrisDerivativeXX7 z =[-96*x^2*y - 111.0*x*y^2 - 27.0*y^3 + 84*x*y + 37.0*y^2 - 10*y]
                        where [x,y] = z

argyrisDerivativeXX8 :: [Double] -> [Double]
argyrisDerivativeXX8 z =[-81.0*x*y^2 - 37.0*y^3 + 37.0*y^2]
                        where [x,y] = z

argyrisDerivativeXX9 :: [Double] -> [Double]
argyrisDerivativeXX9 z =[21.0*x*y^2 + 7.00*y^3 - 7.00*y^2]
                        where [x,y] = z

argyrisDerivativeXX10 :: [Double] -> [Double]
argyrisDerivativeXX10 z =[-10.0*x^3 + 9.00*x*y^2 + 18.0*x^2 + 2*y^3 - 3.00*y^2 - 9.00*x + 1]
                         where [x,y] = z

argyrisDerivativeXX11 :: [Double] -> [Double]
argyrisDerivativeXX11 z =[-24*x^2*y - 36*x*y^2 - 12*y^3 + 30*x*y + 20*y^2 - 8*y]
                         where [x,y] = z

argyrisDerivativeXX12 :: [Double] -> [Double]
argyrisDerivativeXX12 z =[6*x*y^2 + 3.00*y^3 - 3.00*y^2]
                         where [x,y] = z

argyrisDerivativeXX13 :: [Double] -> [Double]
argyrisDerivativeXX13 z =[10.0*x^3 - 1.50*x*y^2 - 12*x^2 - 0.500*y^3 + 0.500*y^2 + 3.00*x]
                         where [x,y] = z

argyrisDerivativeXX14 :: [Double] -> [Double]
argyrisDerivativeXX14 z =[24*x^2*y + 21.0*x*y^2 + 5.00*y^3 - 18*x*y - 7.00*y^2 + 2*y]
                         where [x,y] = z

argyrisDerivativeXX15 :: [Double] -> [Double]
argyrisDerivativeXX15 z =[-4.50*x*y^2 - 2.50*y^3 + 2.50*y^2]
                         where [x,y] = z

argyrisDerivativeXX16 :: [Double] -> [Double]
argyrisDerivativeXX16 z =[-7.50*x*y^2 - 1.50*y^3 + 2.50*y^2]
                         where [x,y] = z

argyrisDerivativeXX17 :: [Double] -> [Double]
argyrisDerivativeXX17 z =[15.0*x*y^2 + 7.00*y^3 - 7.00*y^2]
                         where [x,y] = z

argyrisDerivativeXX18 :: [Double] -> [Double]
argyrisDerivativeXX18 z =[-1.50*x*y^2 - 0.500*y^3 + 0.500*y^2]
                         where [x,y] = z

argyrisDerivativeXX19 :: [Double] -> [Double]
argyrisDerivativeXX19 z =[16*(3*x*y^2 + y^3 - y^2)*sqrt(2)]
                         where [x,y] = z

argyrisDerivativeXX20 :: [Double] -> [Double]
argyrisDerivativeXX20 z =[-96*x*y^2 - 64*y^3 + 64*y^2]
                         where [x,y] = z

argyrisDerivativeXX21 :: [Double] -> [Double]
argyrisDerivativeXX21 z =[-192*x^2*y - 192*x*y^2 - 32*y^3 + 192*x*y + 64*y^2 - 32*y]
                         where [x,y] = z
