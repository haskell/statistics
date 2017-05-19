import Test.Framework (defaultMain)
import qualified Tests.Distribution as Distribution
import qualified Tests.Function as Function
import qualified Tests.KDE as KDE
import qualified Tests.Matrix as Matrix
import qualified Tests.Matrix.Algorithms as MatrixAlgorithms
import qualified Tests.NonParametric as NonParametric
import qualified Tests.Parametric as Parametric
import qualified Tests.Transform as Transform
import qualified Tests.Correlation as Correlation
import qualified Tests.Serialization
main :: IO ()
main = defaultMain [ Distribution.tests
                   , Function.tests
                   , KDE.tests
                   , Matrix.tests
                   , MatrixAlgorithms.tests
                   , NonParametric.tests
                   , Parametric.tests
                   , Transform.tests
                   , Correlation.tests
                   , Tests.Serialization.tests
                   ]
