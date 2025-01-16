import Test.DocTest (doctest)

main :: IO ()
main = doctest ["-XHaskell2010", "Statistics"]

