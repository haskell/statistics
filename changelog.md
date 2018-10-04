## Unreleased

 * Fix vector index out of bounds in `bootstrapBCA` and `bootstrapRegress`
   (see issue #149)

## Changes in 0.14.0.2

 * Compatibility fixes with older GHC


## Changes in 0.14.0.1

 * Restored compatibility with GHC 7.4 & 7.6


## Changes in 0.14.0.0

Breaking update. It seriously changes parts of API. It adds new data types for
dealing with with estimates, confidence intervals, confidence levels and
p-value. Also API for statistical tests is changed.

 * Module `Statistis.Types` now contains new data types for estimates,
   upper/lower bounds, confidence level, and p-value.

	- `CL` for representing confidence level
	- `PValue` for representing p-values
	- `Estimate` data type moved here from `Statistis.Resampling.Bootstrap` and
      now parametrized by type of error.
	- `NormalError` — represents normal error.
    - `ConfInt` — generic confidence interval
    - `UpperLimit`,`LowerLimit` for upper/lower limits.

 * New API for statistical tests. Instead of simply return significant/not
   significant it returns p-value, test statistics and distribution of test
   statistics if it's available. Tests also return `Nothing` instead of throwing
   error if sample size is not sufficient. Fixes #25.

 * `Statistics.Tests.Types.TestType` data type dropped

 * New smart constructors for distributions are added. They return `Nothing` if
   parameters are outside of allowed range.

 * Serialization instances (`Show/Read, Binary, ToJSON/FromJSON`) for
   distributions no longer allows to create data types with invalid
   parameters. They will fail to parse. Cached values are not serialized either
   so `Binary` instances changed normal and F-distributions.

   Encoding to JSON changed for Normal, F-distribution, and χ²
   distributions. However data created using older statistics will be
   successfully decoded.

   Fixes #59.

 * Statistics.Resample.Bootstrap uses new data types for central estimates.

 * Function for calculation of confidence intervals for Poisson and binomial
   distribution added in `Statistics.ConfidenceInt`

 * Tests of position now allow to ask whether first sample on average larger
   than second, second larger than first or whether they differ significantly.
   Affects Wilcoxon-T, Mann-Whitney-U, and Student-T tests.

 * API for bootstrap changed. New data types added.

 * Bug fixes for #74, #81, #83, #92, #94

 * `complCumulative` added for many distributions.



## Changes in 0.13.3.0

 * Kernel density estimation and FFT use generic versions now.

 * Code for calculation of Spearman and Pearson correlation added. Modules
   `Statistics.Correlation.Spearman` and `Statistics.Correlation.Pearson`.

 * Function for calculation covariance added in `Statistics.Sample`.

 * `Statistics.Function.pair` added. It zips vector and check that lengths are
   equal.

 * New functions added to `Statistics.Matrix`

 * Laplace distribution added.


## Changes in 0.13.2.3

 * Vector dependency restored to >=0.10


## Changes in 0.13.2.2

 * Vector dependency lowered to >=0.9


## Changes in 0.13.2.1

 * Vector dependency bumped to >=0.10


## Changes in 0.13.2.0

 * Support for regression bootstrap added


## Changes in 0.13.1.1

 * Fix for out of bound access in bootstrap (see `bos/criterion#52`)


## Changes in 0.13.1.0

  * All types now support JSON encoding and decoding.


## Changes in 0.12.0.0

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

## Changes in 0.11.0.3

  * Fixed a subtle bug in calculation of the jackknifed unbiased variance.

  * The test suite now requires QuickCheck 2.7.

  * We now calculate quantiles for normal distribution in a more
    numerically stable way (bug #64).

## Changes in 0.10.6.0

  * The Estimator type has become an algebraic data type.  This allows
    the jackknife function to potentially use more efficient jackknife
    implementations.

  * jackknifeMean, jackknifeStdDev, jackknifeVariance,
    jackknifeVarianceUnb: new functions.  These have O(n) cost instead
    of the O(n^2) cost of the standard jackknife.

  * The mean function has been renamed to welfordMean; a new
    implementation of mean has better numerical accuracy in almost all
    cases.

## Changes in 0.10.5.2

  * histogram correctly chooses range when all elements in the sample are same
    (bug #57)


## Changes in 0.10.5.1

  * Bug fix for S.Distributions.Normal.standard introduced in 0.10.5.0 (Bug #56)


## Changes in 0.10.5.0

  * Enthropy type class for distributions is added.

  * Probability and probability density of distribution is given in
    log domain too.

## Changes in 0.10.4.0

  * Support for versions of GHC older than 7.2 is discontinued.

  * All datatypes now support 'Data.Binary' and 'GHC.Generics'.

## Changes in 0.10.3.0

  * Bug fixes

## Changes in 0.10.2.0

  * Bugs in DCT and IDCT are fixed.

  * Accesors for uniform distribution are added.

  * ContGen instances for all continuous distribtuions are added.

  * Beta distribution is added.

  * Constructor for improper gamma distribtuion is added.

  * Binomial distribution allows zero trials.

  * Poisson distribution now accept zero parameter.

  * Integer overflow in caculation of Wilcoxon-T test is fixed.

  * Bug in 'ContGen' instance for normal distribution is fixed.

## Changes in 0.10.1.0

  * Kolmogorov-Smirnov nonparametric test added.

  * Pearson chi squared test added.

  * Type class for generating random variates for given distribution
    is added.

  * Modules 'Statistics.Math' and 'Statistics.Constants' are moved to
    the `math-functions` package. They are still available but marked
    as deprecated.


## Changes in 0.10.0.1

  * `dct` and `idct` now have type `Vector Double -> Vector Double`


## Changes in 0.10.0.0

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

  * Test results returned as enumeration instead of `Bool`.

  * Performance improvements for Mann-Whitney U and Wilcoxon tests.

  * Module `S.Tests.NonParamtric` is split into `S.Tests.MannWhitneyU`
    and `S.Tests.WilcoxonT`

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
