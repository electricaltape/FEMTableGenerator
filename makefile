#------------------------------------------------------------------------------
# File: makefile
# Author: David Wells
# Description: compile stuff. What else would a makefile do?
#------------------------------------------------------------------------------

all : quadratics gradients printarray
	
quadratics : ./BasisFunctions/Quadratics.hs
	ghc --make ./BasisFunctions/Quadratics.hs

gradients : ./BasisFunctions/QuadraticGradients.hs
	ghc --make ./BasisFunctions/QuadraticGradients.hs
	
printarray : printarray.hs
	ghc --make printarray.hs
