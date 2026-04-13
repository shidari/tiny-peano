module Main (main) where

import Lib (interpret)

main :: IO ()
main = do
  let inputs = ["2 + 3", "0 + 0", "1 + 2 + 3", "(1 + 2) + (3 + 4)"]
  mapM_ (putStrLn . interpret) inputs
