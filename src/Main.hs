module Main (main) where
  
-- import CustomData.FileNames
-- import CustomData.Types
-- import CustomData.Updates
-- import Data.Time
-- import IO.CSVHandler
import IO.CLUI

import Tests.CustomDataTest (testCustomData)
import Tests.CSVHandlerTest (testCSVHandlers)
import Tests.UpdateTest (testUpdate)

main :: IO ()
main = do
  testCustomData
  testCSVHandlers
  testUpdate
  -- showListCmd itemHeader [Item 1 "Sample Item", Item 2 "Another Item"]
  -- showListCmd stockHeader [Stock (Item 1 "Sample Item") 10 ["A1", "B2"]]
  -- showListCmd transHeader [Transaction 1 (Item 1 "Sample Item") 5 IN 1 1 2024 12 0 0 ["A1", "B2"]]
  (loadedItems, loadedStocks, loadedTrans) <- load
  inputHandler loadedItems loadedStocks loadedTrans