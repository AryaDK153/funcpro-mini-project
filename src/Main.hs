module Main (main) where
  
import CustomData.FileNames
import CustomData.Types
import CustomData.Updates
import IO.CSVHandler

main :: IO ()
main = do

  -- Data Types Test
  let item1 = Item 1 "Apple"
  print item1

  let itemList = [item1, Item 2 "Banana"]
  print itemList

  let stock1 = Stock item1 100 ["CA0101", "DA0202"]
  print stock1

  let stockList = [stock1, Stock (Item 2 "Banana") 50 ["CA0303"]]
  print stockList

  let transaction1 = Transaction 1 item1 20 IN 15 8 2023 14 30 0 ["CA0101"]
  print transaction1

  let transactionList = [transaction1, Transaction 2 (Item 2 "Banana") 10 OUT 16 8 2023 10 0 0 ["DA0202"]]
  print transactionList


  -- CSV Handling Test
  rawItemCSV <- readCSV itemDB
  rawStockCSV <- readCSV stockDB
  rawTransCSV <- readCSV transDB

  let itemList = parseItemCSV rawItemCSV
  print itemList

  let stockList = parseStockCSV rawStockCSV
  print stockList

  let transList = parseTransCSV rawTransCSV
  print transList

  -- Custom Data Updates Test
  let (updatedItems, newItem) = newItemHandler itemList "Orange"
  putStrLn $ "Added/Found Item: " ++ show newItem
  putStrLn $ "Updated Item List: " ++ show updatedItems

  writeItemCSV updatedItems
  rawItemCSV <- readCSV itemDB
  let itemList = parseItemCSV rawItemCSV
  print itemList
