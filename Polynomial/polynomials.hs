-------------------------------------------------------------------------------
-- polynomials.hs
-- Author: David Wells <drwells@vt.edu>
-- Description: data and manipulating functions for polynomials.
-------------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}
import Data.List

-- TODO make this not so crappy - wrap with newtype and fields for the
-- coefficient and exponents

-- store a 3D polynomial as (coefficient, x-power, y-power, z-power)
type PolynomialTerm = (Double, Int, Int, Int)
type Polynomial = [PolynomialTerm]

instance Show Polynomial where
  show polynomial = concat $ map showPolynomialTerm polynomial
      where showPolynomialTerm (a,b,c,d) = intercalate " "
              [show a, "x^" ++ show b, "y^" ++ show c, "z^" ++ show d] ++ " "

-- instance Read Polynomial where
--   read polyString =
--  where (headPoly, bodyPoly) = span (/='+') polynomialString

-- instance Num Polynomial where
--   poly1 + poly2 = clearZeros $ addPolynomials poly1 poly2
--   poly1 - poly2 = clearZeros $ addPolynomials poly1 (inverse poly2)
--   poly1 * poly2 = undefined
--   fromInteger   = undefined
--   abs           = undefined
--   signum        = undefined

-- compute the additive inverse of a polynomial.
inverse :: Polynomial -> Polynomial
inverse [] = []
inverse polynomial = (-1*r1,n1,n2,n3) : (inverse $ tail polynomial)
    where (r1,n1,n2,n3) = head polynomial

-- add two polynomials.
addPolynomials :: Polynomial -> Polynomial -> Polynomial
addPolynomials poly1 poly2 = foldl addSingle poly1 poly2

-- add a single term on to a polynomial.
addSingle :: Polynomial -> PolynomialTerm -> Polynomial
-- null case.
addSingle  [] (r1,n1,n2,n3) = [(r1,n1,n2,n3)]
addSingle polynomial polynomialTerm =
  -- if the powers are equal, add the constants.
  if (n1,n2,n3) == (m1,m2,m3) then (r1 + r2,n1,n2,n3):xs
                              else (r2,m1,m2,m3) : addSingle xs (r1,n1,n2,n3)
  -- otherwise, save the current list value and recur.
    where xs = tail polynomial
          (r2,m1,m2,m3) = head polynomial
          (r1,n1,n2,n3) = polynomialTerm

-- clear out zero terms from a polynomial.
clearZeros :: Polynomial -> Polynomial
clearZeros [] = []
clearZeros polynomial = if r == 0 then clearZeros $ tail polynomial
                        else head polynomial : (clearZeros $ tail polynomial)
    where (r,_,_,_) = head polynomial