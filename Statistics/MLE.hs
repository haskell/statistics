module Statistics.MLE (
	maxLikelihood,
	probit
	) where 

import Numeric.GSL.Minimization
import Statistics.Distribution
import Statistics.Distribution.Normal
import Statistics.Matrix
import System.Random.MWC
import Control.Applicative
import Control.Monad
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

data MaxControl = MaxControl { maxit :: Int, epsilon :: Double } deriving (Show)

type ObjectiveF = Matrix -> Vector -> [Double] -> Double
type GradientF = Matrix -> Vector -> [Double] -> [Double] 

defaultMaxCtrl :: MaxControl
defaultMaxCtrl = MaxControl 10 10e-2

maxLikelihood :: ObjectiveF -> GradientF -> MaxControl -> Matrix -> Vector -> Vector 
maxLikelihood obj dobj ctrl x y = U.fromList . fst $ bhat 
	where 
		bhat = minimizeD VectorBFGS2 (epsilon ctrl) (maxit ctrl) 1.0 (epsilon ctrl) (obj x y) (dobj x y) sval
		sval = replicate (snd (dimension x)) 0.0 

llprobit :: ObjectiveF
llprobit x y b = negate $ G.sum $ G.map contr (G.zip y (x `multiplyV` U.fromList b))
	where 	
		contr (y,xb) = y * log (cdf xb) + (1 - y) * log (1 - cdf xb)
		cdf = cumulative (normalDistr 0 1) 

dllprobit :: GradientF 
dllprobit x y b = fmap negate $ U.toList $ transpose x `multiplyV` G.map contr (G.zip y (x `multiplyV` U.fromList b))
	where 	
		contr (y,xb) = y * pdf xb / cdf xb - (1 - y) * pdf xb / (1 - cdf xb)
		pdf = density (normalDistr 0 1) 
		cdf = cumulative (normalDistr 0 1) 

probit :: Matrix -> Vector -> Vector
probit = maxLikelihood llprobit dllprobit defaultMaxCtrl
