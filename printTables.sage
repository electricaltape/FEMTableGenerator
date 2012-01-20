import optparse
import quadrature
import interpolateBasisFunctions as interp

def main():

    parser = optparse.OptionParser()
    parser.add_option("-o", "--order",
        help="""Order of Lagrangian interpolant""", metavar="ORDER")
    parser.add_option("-d", "--derivative",
        help="""set to 1 to calculate gradients, 0 for basis functions""",
        metavar="DERIVATIVE")

    (options, args) = parser.parse_args()
    order_flag      = int(options.order)
    derivative_flag = int(options.derivative)

    # verify command line arguements.
    if order_flag not in [1..10]:
        raise AttributeError("GMSH only supports orders 1 through 10.")
    if derivative_flag not in [0..1]:
        raise AttributeError("""This program can only return either the basis
            functions or their gradients.""")

    # run the program.
    var('x, y')
    points = quadrature.points[order_flag]
    polynomial_list = interp.interpolate_basis_polynomials(order_flag)
    if derivative_flag == 1: # use gradients
        print [map(f, points)
               for f in map(gradient_to_function, map(gradient, polynomial_list))]
    else: # use basis functions
        print [map(f, points) for f in map(polynomial_to_function, polynomial_list)]

def gradient(f):
    """Calculate the gradient of some symbolic expression. Return it as a pair."""
    var('x,y')
    return [f.diff(x), f.diff(y)]

def gradient_to_function(f):
    """Convert some gradient into a function mapping a pair of doubles to a
    pair of doubles."""
    return (lambda coords : [f[0].subs({x : coords[0], y : coords[1]}),
                            f[1].subs({x : coords[0], y : coords[1]})])

def polynomial_to_function(f):
    """Convert some symbolic polynomial into a function mapping a pair of
    doubles to a pair of doubles."""
    return (lambda coords : f.subs({x : coords[0], y : coords[1]}))

if __name__ == '__main__':
    main()
