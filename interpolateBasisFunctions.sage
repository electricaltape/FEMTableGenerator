def sum_until(n):
    """Sum the integers up to n."""
    return (n + 1)*n/2

def build_all_RHS(order):
    """Construct all the RHS vectors (that is, the values of the polynomial
    evaluated at the nodes)."""

    def build_RHS(nonzero_node, order=2):
        """Construct some RHS vector [1,0,0,...,0] given the position of the
        1 and order of the approximating polynomials."""

        RHS = [[0] for n in [0..sum_until(order + 1)-1]]
        RHS[nonzero_node] = [1]
        return matrix(RHS)

    number_of_nodes = sum_until(order + 1)
    return ([build_RHS(nonzero_node, order) for nonzero_node in
            [0 .. (number_of_nodes - 1)]])

def build_coefficient_matrix(order=2):
    """Given an order, build a coefficient matrix."""

    # calculate the big list of symbols. Looks like 1,x,y,x^2,xy,y^2,x^3,x^2y..
    var('x,y')
    polynomial_powers = [x^(n-m)*y^m for n in [0..order] for m in [0..n]]

    node_coordinates = build_node_list(order)

    # create a list of lists; each component list is polynomial_powers
    # evaluated at a node.
    vandermonde_lists = ([map(lambda n : n.subs(x=node[0], y=node[1]),
        polynomial_powers) for node in node_coordinates])

    # return the symbolic matrix representation.
    return matrix(vandermonde_lists)

def build_node_list(order):

    # interior function for labeling the nodes on the edges of a triangle.
    def label_triangle(corner0, corner1, corner2, order):
        """Given locations of corners label the nodes on the sides of a
        triangle."""

        step_towards_center = (corner1[0] - corner0[0])/order
        edge_node_coordinates = ([corner0, corner1, corner2] +

        # add coordinates along x-axis.
        [(corner0[0] + x,corner0[1]) for x in [n*step_towards_center
            for n in [1..(order-1)]]] +
        # add coordinates along the line parallel to y = 1 - x.
        [(corner2[1] - x, corner0[1] + x) for x in [n*step_towards_center
            for n in [1..(order-1)]]] +
        # add coordinates along y-axis.
        [(corner0[0],corner0[1] + y) for y in [n*step_towards_center
            for n in reversed([1..order-1])]])

        return edge_node_coordinates

    # interior function for recursion.
    def construct_nodes(corner0, corner1, corner2, order):
        """Recursively build the list of node coordinates. Order the nodes as GMSH
        does."""

        # check if all the corners are the same; if so, return one item.
        if corner0 == corner1 == corner2:

            return [corner0]

        else:

            # get the outer node coordinates.
            node_coordinates = label_triangle(corner0, corner1, corner2, order)

            # find the nodes for the inner triangle. If they are all equal we
            # are done. Otherwise recur.
            if order > 2: # If order <= 2 then there are no interior nodes.

                step_towards_center = (corner1[0] - corner0[0])/order
                new_corner0 = (corner0[0] + step_towards_center, corner0[1] +
                        step_towards_center)
                new_corner1 = (corner1[0] - 2*step_towards_center, corner1[1] +
                        step_towards_center)
                new_corner2 = (corner2[0] + step_towards_center, corner2[1] -
                        2*step_towards_center)
                return node_coordinates + construct_nodes(new_corner0,
                    new_corner1, new_corner2, order - 3)

            else:

                return node_coordinates

    # call the recurring function with the proper first three corners.
    return construct_nodes((0,0),(1,0),(0,1),order)

def interpolate_basis_polynomials(order):
    """Given some order, interpolate the basis polynomials. Number the interior
    nodes used as GMSH does (recursively)."""

    A = build_coefficient_matrix(order)
    return [A.inverse() * RHS for RHS in build_all_RHS(order)]

def print_basis_polynomials(order):
    """Given some order, interpolate the basis polynomials and return their
    representation as a string that the FEMBasis program can understand."""

    solutions = interpolate_basis_polynomials(order)

