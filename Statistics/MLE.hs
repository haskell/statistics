module Statistics.MLE (
	maxLikelihood,
	probit
	) where 

import Numeric.GSL.Minimization
import Statistics.Distribution
import Statistics.Distribution.Normal
import Statistics.Matrix
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

data MaxControl = MaxControl { maxit :: Int, epsilon :: Double } deriving (Show)

type ObjectiveF = [Double] -> Double
type GradientF = [Double] -> [Double]
type LikelihoodModel = Matrix -> Vector -> (ObjectiveF, GradientF)

defaultMaxCtrl :: MaxControl
defaultMaxCtrl = MaxControl 100 10e-2

maxLikelihood :: LikelihoodModel -> MaxControl -> Matrix -> Vector -> Vector 
maxLikelihood model ctrl x y = U.fromList . fst $ bhat 
	where 
		bhat = uncurry (minimizeD VectorBFGS2 (epsilon ctrl) (maxit ctrl) 1.0 0.1) (model x y) sval
		sval = replicate (snd (dimension x)) (epsilon ctrl) 

llprobit :: LikelihoodModel 
llprobit x y = (loglik, grad)  
	where 	
		loglik b = negate $ G.sum $ G.map contr (G.zip y (x `multiplyV` U.fromList b))
		grad b = fmap negate $ U.toList $ transpose x `multiplyV` G.map gradcontr (G.zip y (x `multiplyV` U.fromList b))
		contr (y,xb) = y * log (cdf xb) + (1.0 - y) * log (1.0 - cdf xb)
		gradcontr (y,xb) = y * pdf xb / cdf xb - (1 - y) * pdf xb / (1 - cdf xb)
		pdf = density (normalDistr 0 1) 
		cdf x = min (max (cumulative (normalDistr 0 1) x) (epsilon defaultMaxCtrl)) (1 - epsilon defaultMaxCtrl) -- BUG in cumulative: range not [0,1]

probit :: Matrix -> Vector -> Vector
probit = maxLikelihood llprobit defaultMaxCtrl
