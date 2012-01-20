#! /usr/bin/env sage
# =============================================================================
# A collection of quadrature points taken from Burkardt's notes. Nodes and
# weights are saved in dictionaries.
# =============================================================================

import numpy as np
import nodes
import unittest

var('x, y')
tolerance = float(4*10**(-15))
def within_epsilon(a,b):
    """Check to see if two numbers are 'close enough' in floating point
    arithmetic."""
    return abs(a-b)/abs(a) < tolerance

def numerical_integrate(f, nodes, weights):
    """Given nodes and weights integrate a function numerically over the unit
    triangle."""
    return sum(weights * map(lambda line : f(x=line[0], y=line[1]), nodes))

def symbolic_integrate(f):
    """Integrate a symbolic statement of x and y over the unit triangle."""
    return integrate(integrate(f, y, 0, 1 - x), x, 0, 1)

def random_2D_polynomial(order):
    """Return a random 2D polynomial of at most some given order."""
    return sum([randint(0,order*5)*x^randint(0,order) + y^randint(0,order)
                 for i in range(0,randint(1,2 + order*10))])

for test_index in range(1,100):
    for poly_order in range(2,11):
        f = random_2D_polynomial(poly_order)
        symbolic_result = float(symbolic_integrate(f))
        numeric_result  = numerical_integrate(f, nodes.nodes[poly_order],
                                              nodes.weights[poly_order])
        try:
            assert within_epsilon(numeric_result, symbolic_result)
        except AssertionError:
            print "--------------------------------------------------------------------------"
            print "assertion failed for:"
            print "order:", poly_order
            print "polynomial:", f
            print "symbolic result:", symbolic_result
            print "numeric result:", numeric_result
            print "relative error:", (numeric_result - symbolic_result)/symbolic_result
            print "float difference:", abs(numeric_result - symbolic_result)
            print "--------------------------------------------------------------------------"
