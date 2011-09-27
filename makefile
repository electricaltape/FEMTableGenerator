#------------------------------------------------------------------------------
# File: makefile
# Author: David Wells
# Description: compile stuff. What else would a makefile do?
#------------------------------------------------------------------------------

# GHC has make-like features built in. This file is useful for typing 'make'.
all : printarray

printarray : printarray.hs
	ghc --make printarray.hs
