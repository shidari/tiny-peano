module Main (main) where

import Lib (interpret)

main :: IO ()
main = do
  let inputs = ["2 + 3", "0 + 0", "1 + 2 + 3", "(1 + 2) + (3 + 4)", "5 - 2", "3 - 5", "10 - 3 - 2", "6 / 2", "7 / 2", "10 / 0", "2 + 6 / 3", "(2 + 6) / 4"]
  mapM_ (putStrLn . interpret) inputs
