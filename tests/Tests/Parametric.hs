module Tests.Parametric (tests) where

import Statistics.Test.StudentT
import Data.Vector.Unboxed as V
import Test.Framework (Test, testGroup)
import Tests.Helpers (testEquality)

tests :: Test
tests = testGroup "Parametric tests" studentTTests

-- 2 samples x 20 obs data
--
-- Both samples are samples from normal distributions with the same variance (= 1.0),
-- but their means are different (0.0 and 0.5, respectively).
--
-- You can reproduce the data with R (3.1.0) as follows:
--   set.seed(0)
--   sample1 = rnorm(20)
--   sample2 = rnorm(20, 0.5)
--   student = t.test(sample1, sample2, var.equal=T)
--   welch = t.test(sample1, sample2)
--   paired = t.test(sample1, sample2, paired=T)
sample1 = V.fromList [
  1.262954284880793e+00,
 -3.262333607056494e-01,
  1.329799262922501e+00,
  1.272429321429405e+00,
  4.146414344564082e-01,
 -1.539950041903710e+00,
 -9.285670347135381e-01,
 -2.947204467905602e-01,
 -5.767172747536955e-03,
  2.404653388857951e+00,
  7.635934611404596e-01,
 -7.990092489893682e-01,
 -1.147657009236351e+00,
 -2.894615736882233e-01,
 -2.992151178973161e-01,
 -4.115108327950670e-01,
  2.522234481561323e-01,
 -8.919211272845686e-01,
  4.356832993557186e-01,
 -1.237538421929958e+00]
sample2 = V.fromList [
  2.757321147216907e-01,
  8.773956459817011e-01,
  6.333363608148415e-01,
  1.304189509744908e+00,
  4.428932256161913e-01,
  1.003607972233726e+00,
  1.585769362145687e+00,
 -1.909538396968303e-01,
 -7.845993538721883e-01,
  5.467261721883520e-01,
  2.642934435604988e-01,
 -4.288825501025439e-02,
  6.668968254321778e-02,
 -1.494716467962331e-01,
  1.226750747385451e+00,
  1.651911754087200e+00,
  1.492160365445798e+00,
  7.048689050811874e-02,
  1.738304100853380e+00,
  2.206537181457307e-01]

studentTTests :: [Test]
studentTTests = [
    -- R: t.test(sample1, sample2, alt="two.sided", var.equal=T)
    testEquality "two-sample t-test TwoTailed Student" (tTest2 TwoTailed 0.03410 Student sample1 sample2) NotSignificant,
    testEquality "two-sample t-test TwoTailed Student" (tTest2 TwoTailed 0.03411 Student sample1 sample2) Significant,

    -- R: t.test(sample1, sample2, alt="two.sided", var.equal=F)
    testEquality "two-sample t-test TwoTailed Welch" (tTest2 TwoTailed 0.03483 Welch sample1 sample2) NotSignificant,
    testEquality "two-sample t-test TwoTailed Welch" (tTest2 TwoTailed 0.03484 Welch sample1 sample2) Significant,

    -- R: t.test(sample1, sample2, alt="two.sided", paired=T)
    testEquality "two-sample t-test TwoTailed Paired" (tTest2 TwoTailed 0.03411 Paired sample1 sample2) NotSignificant,
    testEquality "two-sample t-test TwoTailed Paired" (tTest2 TwoTailed 0.03412 Paired sample1 sample2) Significant,

    -- R: t.test(sample1, sample2, alt="less", var.equal=T)
    testEquality "two-sample t-test OneTailed Student" (tTest2 OneTailed 0.01705 Student sample1 sample2) NotSignificant,
    testEquality "two-sample t-test OneTailed Student" (tTest2 OneTailed 0.01706 Student sample1 sample2) Significant,

    -- R: t.test(sample1, sample2, alt="less", var.equal=F)
    testEquality "two-sample t-test OneTailed Welch" (tTest2 OneTailed 0.01741 Welch sample1 sample2) NotSignificant,
    testEquality "two-sample t-test OneTailed Welch" (tTest2 OneTailed 0.01742 Welch sample1 sample2) Significant,

    -- R: t.test(sample1, sample2, alt="less", paired=F)
    testEquality "two-sample t-test OneTailed Paired" (tTest2 OneTailed 0.01705 Paired sample1 sample2) NotSignificant,
    testEquality "two-sample t-test OneTailed Paired" (tTest2 OneTailed 0.01706 Paired sample1 sample2) Significant]
