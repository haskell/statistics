#!/usr/bin/python
"""
"""

from mpmath import *

def printListLiteral(lines) :
    print "  [" + "\n  , ".join(lines) + "\n  ]"

################################################################
# Generate header
print "module Tests.Math.Tables where"
print

################################################################
## Generate table for logGamma
print "tableLogGamma :: [(Double,Double)]"
print "tableLogGamma ="

gammaArg = [ 1.25e-6, 6.82e-5, 2.46e-4, 8.8e-4,  3.12e-3, 2.67e-2,
             7.77e-2, 0.234,   0.86,    1.34,    1.89,    2.45,
             3.65,    4.56,    6.66,    8.25,    11.3,    25.6,
             50.4,    123.3,   487.4,   853.4,   2923.3,  8764.3,
             1.263e4, 3.45e4,  8.234e4, 2.348e5, 8.343e5, 1.23e6,
             ]
printListLiteral(
    [ '(%.15f, %.20g)' % (x, log(gamma(x))) for x in gammaArg ]
    )


################################################################
## Generate table for incompleteBeta

print "tableIncompleteBeta :: [(Double,Double,Double,Double)]"
print "tableIncompleteBeta ="

incompleteBetaArg = [
    (2,    3,    0.03),
    (2,    3,    0.23),
    (2,    3,    0.76),
    (4,    2.3,  0.89),
    (1,    1,    0.55),
    (0.3,  12.2, 0.11),
    (13.1, 9.8,  0.12),
    (13.1, 9.8,  0.42),
    (13.1, 9.8,  0.92),
    ]
printListLiteral(
    [ '(%.15f, %.15f, %.15f, %.20g)' % (p,q,x, betainc(p,q,0,x, regularized=True))
      for (p,q,x) in incompleteBetaArg
      ])
