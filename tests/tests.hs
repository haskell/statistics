import Test.Tasty (defaultMain,testGroup)

import qualified Tests.Distribution
import qualified Tests.Function
import qualified Tests.KDE
import qualified Tests.Matrix
import qualified Tests.NonParametric
import qualified Tests.Parametric
import qualified Tests.Transform
import qualified Tests.Correlation
import qualified Tests.Serialization
import qualified Tests.Quantile

main :: IO ()
main = defaultMain $ testGroup "statistics"
  [ Tests.Distribution.tests
  , Tests.Function.tests
  , Tests.KDE.tests
  , Tests.Matrix.tests
  , Tests.NonParametric.tests
  , Tests.Parametric.tests
  , Tests.Transform.tests
  , Tests.Correlation.tests
  , Tests.Serialization.tests
  , Tests.Quantile.tests
  ]
