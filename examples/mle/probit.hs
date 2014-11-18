import Statistics.Distribution
import Statistics.Distribution.Normal
import Statistics.Matrix
import Statistics.MLE
import System.Random.MWC
import Control.Applicative
import Control.Monad
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

main :: IO String 
main = do 
	let n = 100 :: Int
	let k = 5 :: Int
	let b0 = 1.0
	let b1 = U.fromList $ take k (enumFrom 1.0)

	gen <- create
	e <- fromList n 1 <$> replicateM n (genContinous (normalDistr 0 1) gen)
	x <- fromList n k <$> replicateM (n * k) (genContinous (normalDistr 0 1) gen)
	let y = G.zipWith3 (\x y z -> if x+y+z > 0 then 1.0 else 0.0 :: Double ) (G.replicate n b0) (x `multiplyV` b1) (toVector e)
	let bhat2 = U.toList $ probit x y
	putStrLn "Calculating probit..."
	return (show bhat2) 

