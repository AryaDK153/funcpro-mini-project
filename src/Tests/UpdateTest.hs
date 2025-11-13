module Tests.UpdateTest (
    testUpdate
) where

import CustomData.FileNames
import CustomData.Types
import CustomData.Updates
import Data.Time
import IO.CSVHandler
import Tests.CSVHandlerTest (loadTestCSVs)

testUpdate :: IO ()
testUpdate = do
  (itemListLoaded, stockListLoaded, transListLoaded) <- loadTestCSVs
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

  let (stockListAfter2, s2) = newStockHandler stockListLoaded (Item 5 "Durian")
  putStrLn $ "New Stock: " ++ show s2
  putStrLn $ "Stocks (should +1): " ++ show stockListAfter2

  let transIN = Transaction 3 (Item 1 "Apple") 25 IN 20 10 2023 12 0 0 ["CA0102"]
  let transOUT = Transaction 4 (Item 99 "Durian") 10 OUT 20 10 2023 13 0 0 ["CA0102"]

  -- sneakpeek transHandler logic via stockUpdate
  let maybeStocksIN = stockUpdate stockListAfter2 transIN
  let maybeStocksOUT = case maybeStocksIN of
        Nothing -> Nothing
        Just s  -> stockUpdate s transOUT

  case maybeStocksOUT of
    Just final -> putStrLn $ "Final stocks: " ++ show final
    Nothing    -> putStrLn "Transaction failed (invalid stock operation)"

  -- 🕐 Get current date and time
  currentTime <- getCurrentTime
  timezone <- getCurrentTimeZone
  let localTime = utcToLocalTime timezone currentTime
      (yyyy, mm, dd) = toGregorian $ localDay localTime
      timeOfDay = localTimeOfDay localTime
      hh = todHour timeOfDay
      mn = todMin timeOfDay
      ss = floor (todSec timeOfDay)  -- truncate float seconds

  let newTransInput = (Item 99 "Durian", 15, IN, (dd, mm, fromInteger yyyy), (hh, mn, ss), ["DA0303"])
  let maybeNewTrans = newTransHandler (itemListUpdated, stockListAfter2, transListLoaded) newTransInput
  case maybeNewTrans of
    Just (finalItems, finalStocks, finalTrans) -> do
      putStrLn $ "Valid Item List: " ++ show finalItems
      putStrLn $ "New Transaction added. Final Stocks: " ++ show finalStocks
      putStrLn $ "All Transactions: " ++ show finalTrans
      writeItemCSV finalItems
      writeStockCSV finalStocks
      writeTransCSV finalTrans
    Nothing -> putStrLn "New Transaction failed (invalid operation)"