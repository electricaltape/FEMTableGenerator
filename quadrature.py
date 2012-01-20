#! /usr/bin/env sage
# =============================================================================
# A collection of quadrature points taken from Burkardt's notes. Points and
# weights are saved in dictionaries.
# =============================================================================

import numpy as np

points = {}
weights = {}

# degree of precision 1.
points[1] = np.array([[0.33333333333333333333, 0.33333333333333333333]])

weights[1] = np.array([1.0])

# degree of precision 2.
points[2] = np.array([[0.66666666666666666667, 0.16666666666666666667],
                    [0.16666666666666666667, 0.66666666666666666667],
                    [0.16666666666666666667, 0.16666666666666666667]])

weights[2] = np.array([0.33333333333333333333,
                       0.33333333333333333333,
                       0.33333333333333333333]) * 0.5

# degree of precision 3.
points[3] = np.array([[0.33333333333333333333, 0.33333333333333333333],
                    [0.60000000000000000000, 0.20000000000000000000],
                    [0.20000000000000000000, 0.60000000000000000000],
                    [0.20000000000000000000, 0.20000000000000000000]])

weights[3] = np.array([-0.56250000000000000000,
                        0.52083333333333333333,
                        0.52083333333333333333,
                        0.52083333333333333333]) * 0.5

# degree of precision 4.
points[4] = np.array([[0.816847572980459, 0.091576213509771],
                     [0.091576213509771, 0.816847572980459],
                     [0.091576213509771, 0.091576213509771],
                     [0.108103018168070, 0.445948490915965],
                     [0.445948490915965, 0.108103018168070],
                     [0.445948490915965, 0.445948490915965]])

weights[4] = np.array([0.109951743655322,
                       0.109951743655322,
                       0.109951743655322,
                       0.223381589678011,
                       0.223381589678011,
                       0.223381589678011]) * 0.5

# degree of precision 5.
points[5] = np.array([[0.33333333333333333,  0.33333333333333333],
                     [0.79742698535308720,  0.10128650732345633],
                     [0.10128650732345633,  0.79742698535308720],
                     [0.10128650732345633,  0.10128650732345633],
                     [0.05971587178976981,  0.47014206410511505],
                     [0.47014206410511505,  0.05971587178976981],
                     [0.47014206410511505,  0.47014206410511505]])

weights[5] = np.array([0.22500000000000000,
                      0.12593918054482717,
                      0.12593918054482717,
                      0.12593918054482717,
                      0.13239415278850616,
                      0.13239415278850616,
                      0.13239415278850616]) * 0.5

# degree of precision 6.
points[6] = np.array([[0.873821971016996, 0.063089014491502],
                     [0.063089014491502, 0.873821971016996],
                     [0.063089014491502, 0.063089014491502],
                     [0.501426509658179, 0.249286745170910],
                     [0.249286745170910, 0.501426509658179],
                     [0.249286745170910, 0.249286745170910],
                     [0.636502499121399, 0.310352451033785],
                     [0.636502499121399, 0.053145049844816],
                     [0.310352451033785, 0.636502499121399],
                     [0.310352451033785, 0.053145049844816],
                     [0.053145049844816, 0.636502499121399],
                     [0.053145049844816, 0.310352451033785]])

weights[6] = np.array([0.050844906370207,
                       0.050844906370207,
                       0.050844906370207,
                       0.116786275726379,
                       0.116786275726379,
                       0.116786275726379,
                       0.082851075618374,
                       0.082851075618374,
                       0.082851075618374,
                       0.082851075618374,
                       0.082851075618374,
                       0.082851075618374]) * 0.5

# degree of precision 7.
points[7] = np.array([[0.333333333333333,  0.333333333333333],
                     [0.479308067841923,  0.260345966079038],
                     [0.260345966079038,  0.479308067841923],
                     [0.260345966079038,  0.260345966079038],
                     [0.869739794195568,  0.065130102902216],
                     [0.065130102902216,  0.869739794195568],
                     [0.065130102902216,  0.065130102902216],
                     [0.638444188569809,  0.312865496004875],
                     [0.638444188569809,  0.048690315425316],
                     [0.312865496004875,  0.638444188569809],
                     [0.312865496004875,  0.048690315425316],
                     [0.048690315425316,  0.638444188569809],
                     [0.048690315425316,  0.312865496004875]])

weights[7] = np.array([-0.149570044467670,
                        0.175615257433204,
                        0.175615257433204,
                        0.175615257433204,
                        0.053347235608839,
                        0.053347235608839,
                        0.053347235608839,
                        0.077113760890257,
                        0.077113760890257,
                        0.077113760890257,
                        0.077113760890257,
                        0.077113760890257,
                        0.077113760890257]) * 0.5

# degree of precision 8.
points[8] = np.array([[0.3333333333333333, 0.3333333333333333],
                     [0.7974269853530872, 0.1012865073234563],
                     [0.1012865073234563, 0.7974269853530872],
                     [0.1012865073234563, 0.1012865073234563],
                     [0.0597158717897698, 0.4701420641051151],
                     [0.4701420641051151, 0.0597158717897698],
                     [0.4701420641051151, 0.4701420641051151],
                     [0.5357953464498992, 0.2321023267750504],
                     [0.2321023267750504, 0.5357953464498992],
                     [0.2321023267750504, 0.2321023267750504],
                     [0.9410382782311209, 0.0294808608844396],
                     [0.0294808608844396, 0.9410382782311209],
                     [0.0294808608844396, 0.0294808608844396],
                     [0.7384168123405100, 0.2321023267750504],
                     [0.7384168123405100, 0.0294808608844396],
                     [0.2321023267750504, 0.7384168123405100],
                     [0.2321023267750504, 0.0294808608844396],
                     [0.0294808608844396, 0.7384168123405100],
                     [0.0294808608844396, 0.2321023267750504]])

weights[8] = np.array([0.0378610912003147,
                       0.0376204254131829,
                       0.0376204254131829,
                       0.0376204254131829,
                       0.0783573522441174,
                       0.0783573522441174,
                       0.0783573522441174,
                       0.1162714796569659,
                       0.1162714796569659,
                       0.1162714796569659,
                       0.0134442673751655,
                       0.0134442673751655,
                       0.0134442673751655,
                       0.0375097224552317,
                       0.0375097224552317,
                       0.0375097224552317,
                       0.0375097224552317,
                       0.0375097224552317,
                       0.0375097224552317]) * 0.5

# degree of precision 9.
points[9] = np.array([[0.33333333333333331,      0.33333333333333331],
                     [2.06349616025259287E-002, 0.48968251919873701],
                     [0.48968251919873701,      2.06349616025259287E-002],
                     [0.48968251919873701,      0.48968251919873701],
                     [0.12582081701412900,      0.43708959149293553],
                     [0.43708959149293553,      0.12582081701412900],
                     [0.43708959149293553,      0.43708959149293553],
                     [0.62359292876193562,      0.18820353561903219],
                     [0.18820353561903219,      0.62359292876193562],
                     [0.18820353561903219,      0.18820353561903219],
                     [0.91054097321109406,      4.47295133944529688E-002],
                     [4.47295133944529688E-002, 0.91054097321109406],
                     [4.47295133944529688E-002, 4.47295133944529688E-002],
                     [0.74119859878449801,      3.68384120547362581E-002],
                     [0.74119859878449801,      0.22196298916076573],
                     [3.68384120547362581E-002, 0.74119859878449801],
                     [3.68384120547362581E-002, 0.22196298916076573],
                     [0.22196298916076573,      0.74119859878449801],
                     [0.22196298916076573,      3.68384120547362581E-002]])

weights[9] = np.array([9.71357962827961025E-002,
                       3.13347002271398278E-002,
                       3.13347002271398278E-002,
                       3.13347002271398278E-002,
                       7.78275410047754301E-002,
                       7.78275410047754301E-002,
                       7.78275410047754301E-002,
                       7.96477389272090969E-002,
                       7.96477389272090969E-002,
                       7.96477389272090969E-002,
                       2.55776756586981006E-002,
                       2.55776756586981006E-002,
                       2.55776756586981006E-002,
                       4.32835393772893970E-002,
                       4.32835393772893970E-002,
                       4.32835393772893970E-002,
                       4.32835393772893970E-002,
                       4.32835393772893970E-002,
                       4.32835393772893970E-002]) * 0.5

# degree of precision 11. Note that there is no known rule of degree of
# precision 10; for the sake of the interface I will label the 10s as 11s.
points[11] = np.array([[0.33333333333333333, 0.333333333333333333],
                      [0.9480217181434233,  0.02598914092828833],
                      [0.02598914092828833, 0.9480217181434233],
                      [0.02598914092828833, 0.02598914092828833],
                      [0.8114249947041546,  0.09428750264792270],
                      [0.09428750264792270, 0.8114249947041546],
                      [0.09428750264792270, 0.09428750264792270],
                      [0.01072644996557060, 0.4946367750172147],
                      [0.4946367750172147,  0.01072644996557060],
                      [0.4946367750172147,  0.4946367750172147],
                      [0.5853132347709715,  0.2073433826145142],
                      [0.2073433826145142,  0.5853132347709715],
                      [0.2073433826145142,  0.2073433826145142],
                      [0.1221843885990187,  0.4389078057004907],
                      [0.4389078057004907,  0.1221843885990187],
                      [0.4389078057004907,  0.4389078057004907],
                      [0.6779376548825902,  0.04484167758913055],
                      [0.6779376548825902,  0.27722066752827925],
                      [0.04484167758913055, 0.6779376548825902],
                      [0.04484167758913055, 0.27722066752827925],
                      [0.27722066752827925, 0.6779376548825902],
                      [0.27722066752827925, 0.04484167758913055],
                      [0.8588702812826364,  0.00000000000000000],
                      [0.8588702812826364,  0.1411297187173636],
                      [0.0000000000000000,  0.8588702812826364],
                      [0.0000000000000000,  0.1411297187173636],
                      [0.1411297187173636,  0.8588702812826364],
                      [0.1411297187173636,  0.0000000000000000]])

weights[11] = np.array([0.08797730116222190,
                        0.008744311553736190,
                        0.008744311553736190,
                        0.008744311553736190,
                        0.03808157199393533,
                        0.03808157199393533,
                        0.03808157199393533,
                        0.01885544805613125,
                        0.01885544805613125,
                        0.01885544805613125,
                        0.07215969754474100,
                        0.07215969754474100,
                        0.07215969754474100,
                        0.06932913870553720,
                        0.06932913870553720,
                        0.06932913870553720,
                        0.04105631542928860,
                        0.04105631542928860,
                        0.04105631542928860,
                        0.04105631542928860,
                        0.04105631542928860,
                        0.04105631542928860,
                        0.007362383783300573,
                        0.007362383783300573,
                        0.007362383783300573,
                        0.007362383783300573,
                        0.007362383783300573,
                        0.007362383783300573]) * 0.5

points[10] = points[11]

weights[10] = weights[11]
