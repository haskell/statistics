# Statistics: efficient, general purpose statistics

This package provides the Statistics module, a Haskell library for
working with statistical data in a space- and time-efficient way.

Where possible, we give citations and computational complexity
estimates for the algorithms used.


# Performance

This library has been carefully optimised for high performance.  To
obtain the best runtime efficiency, it is imperative to compile
libraries and applications that use this library using a high level of
optimisation.

Suggested GHC options:

    -O -funbox-strict-fields

To illustrate, here are the times (in seconds) to generate and sum 250
million random Word32 values, on a laptop with a 2.4GHz Core2 Duo
P8600 processor, running Fedora 11 and GHC 6.10.3:

    no flags   200+
    -O           1.249
    -O -fvia-C   0.991

As the numbers above suggest, compiling without optimisation will
yield unacceptable performance.


# Get involved!

Please report bugs via the
[bitbucket issue tracker](http://bitbucket.org/bos/statistics/issues).

Master [Mercurial repository](http://bitbucket.org/bos/statistics):

* `hg clone http://bitbucket.org/bos/statistics`

There's also a [git mirror](http://github.com/bos/statistics):

* `git clone git://github.com/bos/statistics.git`

(You can create and contribute changes using either Mercurial or git.)


# Authors

This library is written and maintained by Bryan O'Sullivan,
<bos@serpentine.com>.
