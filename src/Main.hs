module Main (main) where

import IO.CLUI

main :: IO ()
main = do
  (loadedItems, loadedStocks, loadedTrans) <- load
  putStrLn "Boot Complete! Enter your command: ('help' for list of cmd)"
  inputHandler loadedItems loadedStocks loadedTrans