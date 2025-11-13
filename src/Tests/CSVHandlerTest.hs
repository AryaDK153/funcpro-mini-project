module Tests.CSVHandlerTest (
  loadTestCSVs,
  testCSVHandlers
) where

import IO.CSVHandler
import CustomData.FileNames
import CustomData.Types

loadTestCSVs :: IO ([Item], [Stock], [Transaction])
loadTestCSVs = do
  rawItemCSV <- readCSV itemDB
  rawStockCSV <- readCSV stockDB
  rawTransCSV <- readCSV transDB

  let itemListLoaded = parseItemCSV rawItemCSV
  let stockListLoaded = parseStockCSV rawStockCSV
  let transListLoaded = parseTransCSV rawTransCSV

  return (itemListLoaded, stockListLoaded, transListLoaded)

testCSVHandlers :: IO ()
testCSVHandlers = do
  -- CSV Handling Test
  (itemListLoaded, stockListLoaded, transListLoaded) <- loadTestCSVs

  print itemListLoaded
  print stockListLoaded
  print transListLoaded