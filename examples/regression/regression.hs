{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Statistics.Regression
import Statistics.Types (NormalErr(..), TErr(..))
import Statistics.Matrix hiding (map)
import Data.Foldable
import System.IO
import System.Exit
import Data.List
import Data.Ord

data Wine = Wine
  { fixedAcidity       :: !Double
  , volatileAcidity    :: !Double
  , citricAcid         :: !Double
  , residualSugar      :: !Double
  , chlorides          :: !Double
  , freeSulfurDioxide  :: !Double
  , totalSulferDioxide :: !Double
  , density            :: !Double
  , pH                 :: !Double
  , sulphates          :: !Double
  , alcohol            :: !Double 
  , quality            :: !Double
  } deriving Show

data Weights = Weights
  { weights :: !Double }

instance FromNamedRecord Wine where
  parseNamedRecord r = Wine <$> r .: "fixedAcidity"
                            <*> r .: "volatileAcidity"
                            <*> r .: "citricAcid"
                            <*> r .: "residualSugar"
                            <*> r .: "chlorides"
                            <*> r .: "freeSulfurDioxide"
                            <*> r .: "totalSulfurDioxide"
                            <*> r .: "density"
                            <*> r .: "pH"
                            <*> r .: "sulphates"
                            <*> r .: "alcohol"
                            <*> r .: "quality"

instance FromNamedRecord Weights where
  parseNamedRecord r = Weights <$> r .: "weights"

toXY :: V.Vector Wine -> ([Vector],Vector)
toXY wine = (toX wine, toY wine)
  where toX :: V.Vector Wine -> [Vector]
        toX w = V.convert <$> (mapX <*> [w])
        mapX  = V.map <$> [ fixedAcidity
                          , volatileAcidity
                          , citricAcid
                          , residualSugar
                          , chlorides
                          , freeSulfurDioxide
                          , totalSulferDioxide
                          , density
                          , pH
                          , sulphates
                          , alcohol 
                          ]
        toY :: V.Vector Wine -> Vector
        toY w = V.convert $ V.map quality w

main :: IO()
main = do
  wineCSV <- BL.readFile "data/wine.csv"
  wine <- case decodeByName wineCSV of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right (_header, wine) -> return wine
    
  -- To run example with homoskedastic errors,
  -- comment out from here...
  
  wCSV <- BL.readFile "data/weights.csv"
  w <- case decodeByName wCSV of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right (_header, w) -> return w

  let w1        = V.convert $ V.map weights w
  let (preds,y) = toXY wine
  putStrLn $ show (weightedNormalRegress preds y (Just w1) Unknown)
  
  -- putStrLn $ show (weightedNormalRegress preds y (Just w1) $ Normal 0.5382708)
  -- ^ This will give (numerically) equivalent results with different
  -- reference distributions.
  
  -- ...to here, and uncomment lines below.

  -- let (preds,y) = toXY wine
  -- putStrLn $ show (normalRegress preds y Unknown)
  
  -- putStrLn $ show (normalRegerss preds y $ NormalErr 0.7513569)
  -- ^ This will give (numerically) equivalent results with different
  -- reference distributions.


{-
Equivalent R code to check for correctness:

wine.df    <- read.csv("data/wine.csv")
w.df <- read.csv("data/weights.csv")

fit.weighted <- lm(quality ~ ., data = wine.df, weights = 1/w.df$weights)
summary(fit.weighted)

fit.ordinary <- lm(quality ~ ., data = wine.df)
summary(fit.ordinary)
-}
