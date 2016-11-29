{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Statistics.Regression
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
    Left error -> do
      hPutStrLn stderr error
      exitFailure
    Right (_header, wine) -> return wine

  let (preds,y) = toXY wine
  putStrLn $ show (normalRegress preds y)
