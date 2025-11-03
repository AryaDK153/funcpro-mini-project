module Main (main) where
  
import Data.Types
import Data.Updates
-- import IO.CSVHandler

main :: IO ()
main = do
  let item1 = Item 1 "Apple" 10
  print item1
  let updatedItem = updateQty 5 item1
  print updatedItem
  let updatedItem2 = updateQty (-3) updatedItem
  print updatedItem2