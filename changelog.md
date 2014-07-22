Changes in 0.13.0.0

  * All types now support JSON encoding and decoding.

Changes in 0.12.0.0

  * The `Statistics.Math` module has been removed, after being
    deprecated for several years.  Use the
    [math-functions](http://hackage.haskell.org/package/math-functions)
    package instead.

  * The `Statistics.Test.NonParametric` module has been removed, after
    being deprecated for several years.

  * Added support for Kendall's tau.

  * Added support for OLS regression.

  * Added basic 2D matrix support.

  * Added the Kruskal-Wallis test.

Changes in 0.11.0.3

  * Fixed a subtle bug in calculation of the jackknifed unbiased variance.

  * The test suite now requires QuickCheck 2.7.

  * We now calculate quantiles for normal distribution in a more
    numerically stable way (bug #64).

Changes in 0.10.6.0

  * The Estimator type has become an algebraic data type.  This allows
    the jackknife function to potentially use more efficient jackknife
    implementations.

  * jackknifeMean, jackknifeStdDev, jackknifeVariance,
    jackknifeVarianceUnb: new functions.  These have O(n) cost instead
    of the O(n^2) cost of the standard jackknife.

  * The mean function has been renamed to welfordMean; a new
    implementation of mean has better numerical accuracy in almost all
    cases.

Changes in 0.10.5.2

  * histogram correctly chooses range when all elements in the sample are same
    (bug #57)


Changes in 0.10.5.1

  * Bug fix for S.Distributions.Normal.standard introduced in 0.10.5.0 (Bug #56)


Changes in 0.10.5.0

  * Enthropy type class for distributions is added.

  * Probability and probability density of distribution is given in
    log domain too.

Changes in 0.10.4.0

  * Support for versions of GHC older than 7.2 is discontinued.

  * All datatypes now support 'Data.Binary' and 'GHC.Generics'.

Changes in 0.10.3.0

  * Bug fixes

Changes in 0.10.2.0

  * Bugs in DCT and IDCT are fixed.

  * Accesors for uniform distribution are added.

  * ContGen instances for all continous distribtuions are added.

  * Beta distribution is added.

  * Constructor for improper gamma distribtuion is added.

  * Binomial distribution allows zero trials.

  * Poisson distribution now accept zero parameter.

  * Integer overflow in caculation of Wilcoxon-T test is fixed.

  * Bug in 'ContGen' instance for normal distribution is fixed.

Changes in 0.10.1.0

  * Kolmogorov-Smirnov nonparametric test added.

  * Pearson chi squared test added.

  * Type class for generating random variates for given distribution
    is added.

  * Modules 'Statistics.Math' and 'Statistics.Constants' are moved to
    the @math-functions@ package. They are still available but marked
    as deprecated.


Changed in 0.10.0.1

  * @dct@ and @idct@ now have type @Vector Double -> Vector Double@


Changes in 0.10.0.0

  * The type classes Mean and Variance are split in two. This is
    required for distributions which do not have finite variance or
    mean.

  * The S.Sample.KernelDensity module has been renamed, and
    completely rewritten to be much more robust.  The older module
    oversmoothed multi-modal data.  (The older module is still
    available under the name S.Sample.KernelDensity.Simple).

  * Histogram computation is added, in S.Sample.Histogram.

  * Discrete Fourie transform is added, in S.Transform

  * Root finding is added, in S.Math.RootFinding.

  * The complCumulative function is added to the Distribution
    class in order to accurately assess probalities P(X>x) which are
    used in one-tailed tests.

  * A stdDev function is added to the Variance class for
    distributions.

  * The constructor S.Distribution.normalDistr now takes standard
    deviation instead of variance as its parameter.

  * A bug in S.Quantile.weightedAvg is fixed. It produced a wrong
    answer if a sample contained only one element.

  * Bugs in quantile estimations for chi-square and gamma distribution
    are fixed.

  * Integer overlow in mannWhitneyUCriticalValue is fixed. It
    produced incorrect critical values for moderately large
    samples. Something around 20 for 32-bit machines and 40 for 64-bit
    ones.

  * A bug in mannWhitneyUSignificant is fixed. If either sample was
    larger than 20, it produced a completely incorrect answer.

  * One- and two-tailed tests in S.Tests.NonParametric are selected
    with sum types instead of Bool.

  * Test results returned as enumeration instead of @Bool@.

  * Performance improvements for Mann-Whitney U and Wilcoxon tests.

  * Module @S.Tests.NonParamtric@ is split into @S.Tests.MannWhitneyU@
    and @S.Tests.WilcoxonT@

  * sortBy is added to S.Function.

  * Mean and variance for gamma distribution are fixed.

  * Much faster cumulative probablity functions for Poisson and
    hypergeometric distributions.

  * Better density functions for gamma and Poisson distributions.

  * Student-T, Fisher-Snedecor F-distributions and Cauchy-Lorentz
    distrbution are added.

  * The function S.Function.create is removed. Use generateM from
    the vector package instead.

  * Function to perform approximate comparion of doubles is added to
    S.Function.Comparison

  * Regularized incomplete beta function and its inverse are added to
    S.Function
