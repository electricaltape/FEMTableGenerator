-------------------------------------------------------------------------------
-- ArgyrisFunctions.hs
-- Author: David Wells <drwells@vt.edu>
-- Description: Module containing the Argyris basis functions.
-------------------------------------------------------------------------------
module BasisFunctions.Argyris
( allFunctions,
  allFunctionNames
) where

allFunctions :: [([Double] -> [Double])]
allFunctions = [argyris1, argyris2, argyris3, argyris4, argyris5, argyris6,
              argyris7, argyris8, argyris9, argyris10, argyris11, argyris12,
              argyris13, argyris14, argyris15, argyris16, argyris17, argyris18,
              argyris19, argyris20, argyris21]

allFunctionNames = ["argyris1", "argyris2", "argyris3", "argyris4", "argyris5",
                    "argyris6", "argyris7", "argyris8", "argyris9", "argyris10",
                    "argyris11", "argyris12", "argyris13", "argyris14",
                    "argyris15", "argyris16", "argyris17", "argyris18",
                    "argyris19", "argyris20", "argyris21"]

argyris1 :: [Double] -> [Double]
argyris1 z  = [1 - 10*x^3 - 10*y^3 + 15*x^4 - 30*x^2*y^2 + 15*y^4 - 6*x^5 + 30*x^3*y^2 + 30*x^2*y^3 - 6*y^5]
              where [x,y] = z

argyris2 :: [Double] -> [Double]
argyris2 z  = [10*x^3 - 15*x^4 + 15*x^2*y^2 + 6*x^5 - 15*x^3*y^2 - 15*x^2*y^3]
              where [x,y] = z

argyris3 :: [Double] -> [Double]
argyris3 z  = [10*y^3 + 15*x^2*y^2 - 15*y^4 - 15*x^3*y^2 - 15*x^2*y^3 + 6*y^5]
              where [x,y] = z

argyris4 :: [Double] -> [Double]
argyris4 z  = [x - 6*x^3 - 11*x*y^2 + 8*x^4 + 10*x^2*y^2 + 18*x*y^3 - 3*x^5 + x^3*y^2 - 10*x^2*y^3 - 8*x*y^4]
              where [x,y] = z

argyris5 :: [Double] -> [Double]
argyris5 z  = [y - 11*x^2*y - 6*y^3 + 18*x^3*y + 10*x^2*y^2 + 8*y^4 - 8*x^4*y - 10*x^3*y^2 + x^2*y^3 - 3*y^5]
              where [x,y] = z

argyris6 :: [Double] -> [Double]
argyris6 z  = [-4*x^3 + 7*x^4 - 3.5*x^2*y^2 - 3*x^5 + 3.5*x^3*y^2 + 3.5*x^2*y^3]
              where [x,y] = z

argyris7 :: [Double] -> [Double]
argyris7 z  = [-5*x^2*y + 14*x^3*y + 18.5*x^2*y^2 - 8*x^4*y - 18.5*x^3*y^2 - 13.5*x^2*y^3]
              where [x,y] = z

argyris8 :: [Double] -> [Double]
argyris8 z  = [-5*x*y^2 + 18.5*x^2*y^2 + 14*x*y^3 - 13.5*x^3*y^2 - 18.5*x^2*y^3 - 8*x*y^4]
              where [x,y] = z

argyris9 :: [Double] -> [Double]
argyris9 z  = [-4*y^3 - 3.5*x^2*y^2 + 7*y^4 + 3.5*x^3*y^2 + 3.5*x^2*y^3 - 3*y^5]
              where [x,y] = z

argyris10 :: [Double] -> [Double]
argyris10 z = [0.5*x^2 - 1.5*x^3 + 1.5*x^4 - 1.5*x^2*y^2 - 0.5*x^5 + 1.5*x^3*y^2 + x^2*y^3]
              where [x,y] = z

argyris11 :: [Double] -> [Double]
argyris11 z = [x*y - 4*x^2*y - 4*x*y^2 + 5*x^3*y + 10*x^2*y^2 + 5*x*y^3 - 2*x^4*y - 6*x^3*y^2 - 6*x^2*y^3 - 2*x*y^4]
              where [x,y] = z

argyris12 :: [Double] -> [Double]
argyris12 z = [0.5*y^2 - 1.5*y^3 - 1.5*x^2*y^2 + 1.5*y^4 + x^3*y^2 + 1.5*x^2*y^3 - 0.5*y^5]
              where [x,y] = z

argyris13 :: [Double] -> [Double]
argyris13 z = [0.5*x^3 - x^4 + 0.25*x^2*y^2 + 0.5*x^5 - 0.25*x^3*y^2 - 0.25*x^2*y^3]
              where [x,y] = z

argyris14 :: [Double] -> [Double]
argyris14 z = [x^2*y - 3*x^3*y - 3.5*x^2*y^2 + 2*x^4*y + 3.5*x^3*y^2 + 2.5*x^2*y^3]
              where [x,y] = z

argyris15 :: [Double] -> [Double]
argyris15 z = [1.25*x^2*y^2 - 0.75*x^3*y^2 - 1.25*x^2*y^3]
              where [x,y] = z

argyris16 :: [Double] -> [Double]
argyris16 z = [1.25*x^2*y^2 - 1.25*x^3*y^2 - 0.75*x^2*y^3]
              where [x,y] = z

argyris17 :: [Double] -> [Double]
argyris17 z = [x*y^2 - 3.5*x^2*y^2 - 3*x*y^3 + 2.5*x^3*y^2 + 3.5*x^2*y^3 + 2*x*y^4]
              where [x,y] = z

argyris18 :: [Double] -> [Double]
argyris18 z = [0.5*y^3 + 0.25*x^2*y^2 - y^4 - 0.25*x^3*y^2 - 0.25*x^2*y^3 + 0.5*y^5]
              where [x,y] = z

argyris19 :: [Double] -> [Double]
argyris19 z = [(sqrt 2) * (-8*x^2*y^2 + 8*x^3*y^2 + 8*x^2*y^3)]
              where [x,y] = z

argyris20 :: [Double] -> [Double]
argyris20 z = [-16*x*y^2 + 32*x^2*y^2 + 32*x*y^3 - 16*x^3*y^2 - 32*x^2*y^3 - 16*x*y^4]
              where [x,y] = z

argyris21 :: [Double] -> [Double]
argyris21 z = [-16*x^2*y + 32*x^3*y + 32*x^2*y^2 - 16*x^4*y - 32*x^3*y^2 - 16*x^2*y^3]
              where [x,y] = z