import Test.Framework (defaultMain)
import qualified Tests.Distribution as Distribution
import qualified Tests.Function as Function
import qualified Tests.KDE as KDE
import qualified Tests.Matrix as Matrix
import qualified Tests.NonParametric as NonParametric
import qualified Tests.Transform as Transform
import qualified Tests.Correlation as Correlation

main :: IO ()
main = defaultMain [ Distribution.tests
                   , Function.tests
                   , KDE.tests
                   , Matrix.tests
                   , NonParametric.tests
                   , Transform.tests
                   , Correlation.tests
                   ]
