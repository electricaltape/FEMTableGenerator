subroutine quadratic0(basisvals)
    implicit none
    double precision :: basisvals(0:5)
    basisvals = (/-8.52599980736871e-2, &
                  -0.12434719365631879, &
                  -8.52599980736871e-2, &
                  0.20960719173000555, &
                  -0.12434719365631879, &
                  0.20960719173000555/)
end subroutine quadratic0
subroutine quadratic1(basisvals)
    implicit none
    double precision :: basisvals(0:5)
    basisvals = (/0.20960719173000555, &
                  0.20960719173000555, &
                  -0.12434719365631877, &
                  -0.12434719365631877, &
                  -8.52599980736871e-2, &
                  -8.52599980736871e-2/)
end subroutine quadratic1
subroutine quadratic2(basisvals)
    implicit none
    double precision :: basisvals(0:5)
    basisvals = (/-0.12434719365631877, &
                  -8.52599980736871e-2, &
                  0.20960719173000555, &
                  -8.52599980736871e-2, &
                  0.20960719173000555, &
                  -0.12434719365631877/)
end subroutine quadratic2
subroutine quadratic3(basisvals)
    implicit none
    double precision :: basisvals(0:5)
    basisvals = (/0.2874388755813007, &
                  0.6114019857068721, &
                  0.1011591387118275, &
                  0.6114019857068721, &
                  0.10115913871182751, &
                  0.2874388755813007/)
end subroutine quadratic3
subroutine quadratic4(basisvals)
    implicit none
    double precision :: basisvals(0:5)
    basisvals = (/0.6114019857068721, &
                  0.2874388755813007, &
                  0.6114019857068721, &
                  0.1011591387118275, &
                  0.2874388755813007, &
                  0.1011591387118275/)
end subroutine quadratic4
subroutine quadratic5(basisvals)
    implicit none
    double precision :: basisvals(0:5)
    basisvals = (/0.1011591387118275, &
                  0.10115913871182751, &
                  0.2874388755813007, &
                  0.2874388755813007, &
                  0.6114019857068721, &
                  0.6114019857068721/)
end subroutine quadratic5
subroutine quadraticGrad0(basisvals)
    implicit none
    double precision :: basisvals(0:5,0:1)
    basisvals = (/0.563843963708492, &
                  7.226652578787585e-2, &
                  0.563843963708492, &
                  -1.6361104894963678, &
                  7.226652578787585e-2, &
                  -1.6361104894963678, &
                  0.563843963708492, &
                  7.226652578787585e-2, &
                  0.563843963708492, &
                  -1.6361104894963678, &
                  7.226652578787585e-2, &
                  -1.6361104894963678/)
end subroutine quadraticGrad0
subroutine quadraticGrad1(basisvals)
    implicit none
    double precision :: basisvals(0:5,0:1)
    basisvals = (/1.6361104894963678, &
                  1.6361104894963678, &
                  -7.22665257878759e-2, &
                  -7.22665257878759e-2, &
                  -0.563843963708492, &
                  -0.563843963708492, &
                  0.0, &
                  0.0, &
                  0.0, &
                  0.0, &
                  0.0, &
                  0.0/)
end subroutine quadraticGrad1
subroutine quadraticGrad2(basisvals)
    implicit none
    double precision :: basisvals(0:5,0:1)
    basisvals = (/0.0, &
                  0.0, &
                  0.0, &
                  0.0, &
                  0.0, &
                  0.0, &
                  -7.22665257878759e-2, &
                  -0.563843963708492, &
                  1.6361104894963678, &
                  -0.563843963708492, &
                  1.6361104894963678, &
                  -7.22665257878759e-2/)
end subroutine quadraticGrad2
subroutine quadraticGrad3(basisvals)
    implicit none
    double precision :: basisvals(0:5,0:1)
    basisvals = (/-2.19995445320486, &
                  -1.7083770152842437, &
                  -0.49157743792061603, &
                  1.7083770152842437, &
                  0.49157743792061614, &
                  2.19995445320486, &
                  -2.636110489496368, &
                  -2.636110489496368, &
                  -0.927733474212124, &
                  -0.927733474212124, &
                  -0.436156036291508, &
                  -0.436156036291508/)
end subroutine quadraticGrad3
subroutine quadraticGrad4(basisvals)
    implicit none
    double precision :: basisvals(0:5,0:1)
    basisvals = (/0.927733474212124, &
                  0.436156036291508, &
                  2.636110489496368, &
                  0.436156036291508, &
                  2.636110489496368, &
                  0.927733474212124, &
                  2.636110489496368, &
                  2.636110489496368, &
                  0.927733474212124, &
                  0.927733474212124, &
                  0.436156036291508, &
                  0.436156036291508/)
end subroutine quadraticGrad4
subroutine quadraticGrad5(basisvals)
    implicit none
    double precision :: basisvals(0:5,0:1)
    basisvals = (/-0.927733474212124, &
                  -0.436156036291508, &
                  -2.636110489496368, &
                  -0.436156036291508, &
                  -2.636110489496368, &
                  -0.927733474212124, &
                  -0.49157743792061603, &
                  0.49157743792061614, &
                  -2.19995445320486, &
                  2.19995445320486, &
                  -1.7083770152842437, &
                  1.7083770152842437/)
end subroutine quadraticGrad5
