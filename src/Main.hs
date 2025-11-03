module Main (main) where
  
import CustomData.Types
import CustomData.Updates
import IO.CSVHandler

main :: IO ()
main = do

  -- Data Types Test
  let item1 = Item 1 "Apple" 10
  print item1
  
  -- mutability Test
  let updatedItem = updateQty 5 item1
  print updatedItem
  let updatedItem2 = updateQty (-3) updatedItem
  print updatedItem2
  print (updateQty 20 updatedItem2)
  print updatedItem2

  -- CSV Handling Test
  let stockList = [Stock item1 101, Stock updatedItem 102]
  writeStockCSV "stockDB.csv" stockList
  stocksFromFile <- readStockCSV "stockDB.csv"
  print stocksFromFile