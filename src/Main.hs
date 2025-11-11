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

  let itemListLoaded = parseItemCSV rawItemCSV
  print itemListLoaded

  let stockListLoaded = parseStockCSV rawStockCSV
  print stockListLoaded

  let transListLoaded = parseTransCSV rawTransCSV
  print transListLoaded

  -- Custom Data Updates Test
  let (updatedItems, newItem) = newItemHandler itemListLoaded "Soursop"
  putStrLn $ "Added/Found Item: " ++ show newItem
  putStrLn $ "Updated Item List: " ++ show updatedItems

  writeItemCSV updatedItems
  rawItemCSV2 <- readCSV itemDB
  let itemListUpdated = parseItemCSV rawItemCSV2
  print itemListUpdated

  let (stockListAfter1, s1) = newStockHandler stockListLoaded (Item 1 "Apple")
  putStrLn $ "Reused Stock: " ++ show s1
  putStrLn $ "Stocks (should be same count): " ++ show stockListAfter1

  let (stockListAfter2, s2) = newStockHandler stockListLoaded (Item 99 "Durian")
  putStrLn $ "New Stock: " ++ show s2
  putStrLn $ "Stocks (should +1): " ++ show stockListAfter2

  let transIN = Transaction 3 (Item 1 "Apple") 25 IN 20 10 2023 12 0 0 ["CA0102"]
  let transOUT = Transaction 4 (Item 1 "Apple") 10 OUT 20 10 2023 13 0 0 ["CA0102"]

  -- sneakpeek transHandler logic via stockUpdate
  let maybeStocksIN = stockUpdate stockListAfter2 transIN
  let maybeStocksOUT = case maybeStocksIN of
        Nothing -> Nothing
        Just s  -> stockUpdate s transOUT

  case maybeStocksOUT of
    Just final -> putStrLn $ "Final stocks: " ++ show final
    Nothing    -> putStrLn "Transaction failed (invalid stock operation)"


