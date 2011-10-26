{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Statistics.Sample.KernelDensity (kde)
import Text.Hastache (MuType(..), defaultConfig, hastacheFile)
import Text.Hastache.Context (mkStrContext)
import qualified Data.Attoparsec as B
import qualified Data.Attoparsec.Char8 as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Vector.Unboxed as U

csv = do
  B.takeTill A.isEndOfLine
  (A.double `A.sepBy` A.char ',') `A.sepBy` A.endOfLine

main = do
  waits <- (either error (U.fromList . map last . filter (not.null)) .
            A.parseOnly csv) <$> B.readFile "data/faithful.csv"
  let xs = map (\(a,b) -> [a,b]) . U.toList . uncurry U.zip . kde 64 $ waits
      context "data" = MuVariable . show $ xs
  s <- hastacheFile defaultConfig "kde.tpl" (mkStrContext context)
  L.writeFile "kde.html" s
