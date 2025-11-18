-- Command Line User Interface (CLUI) module
module IO.CLUI (
  inputHandler,
  load,
  save,
) where

import CustomData.FileNames
import CustomData.Types
import CustomData.Updates
import Data.List (intercalate)
import Data.Time
import IO.CSVHandler

-- helpers
padOrCut :: Int -> String -> String
padOrCut n str
  | length str > n = take (n - 3) str ++ "..."
  | otherwise      = str ++ replicate (n - length str) ' '

formatDateTime :: Int -> Int -> Int -> Int -> Int -> Int -> String
formatDateTime dd mm yyyy hh mn ss =
    two dd ++ "-" ++ two mm ++ "-" ++ four yyyy ++ " "
    ++ two hh ++ ":" ++ two mn ++ ":" ++ two ss
  where
    two x = if x < 10 then '0' : show x else show x
    four x = if x < 1000 then replicate (4 - length (show x)) '0' ++ show x else show x

-- commands
printListCmd :: String -> (a -> String) -> [a] -> IO ()
printListCmd header render xs = do
  putStrLn ""
  putStrLn header
  mapM_ (putStrLn . render) xs
  
itemHeader = "ID  |Item Name           \n" ++ replicate 25 '-'
stockHeader = "ID  |Item Name           |Qty       |Shelves\n" ++ replicate 57 '-'
transHeader = "TID |IID |Item Name           |Qty       |Dir|Date & Time        |Shelves\n" ++ replicate 86 '-'

renderItem :: Item -> String
renderItem (Item id name) =
  padOrCut 4 (show id) ++ "|" ++ padOrCut 20 name
  
renderStock :: Stock -> String
renderStock (Stock item qty shelves) =
  renderItem item ++ "|" ++ padOrCut 10 (show qty) ++ "|" ++ padOrCut 20 (intercalate "," shelves)

renderTransaction :: Transaction -> String
renderTransaction (Transaction tid item qty dir dd mm yyyy hh mn ss shelves) =
  padOrCut 4 (show tid) ++ "|" ++
  renderItem item ++ "|" ++
  padOrCut 10 (show qty) ++ "|" ++
  padOrCut 3 (show dir) ++ "|" ++
  formatDateTime dd mm yyyy hh mn ss ++ "|" ++
  padOrCut 20 (intercalate "," shelves)

-- findCmd

-- addItemCmd
-- addStockCmd
-- addTransactionCmd

load :: IO ([Item], [Stock], [Transaction])
load = do
  rawItemCSV <- readCSV itemDB
  rawStockCSV <- readCSV stockDB
  rawTransCSV <- readCSV transDB

  let itemListLoaded = parseItemCSV rawItemCSV
  let stockListLoaded = parseStockCSV rawStockCSV
  let transListLoaded = parseTransCSV rawTransCSV

  return (itemListLoaded, stockListLoaded, transListLoaded)

save :: [Item] -> [Stock] -> [Transaction] -> IO ()
save items stocks transactions = do
  writeItemCSV items
  writeStockCSV stocks
  writeTransCSV transactions

inputHandler :: [Item] -> [Stock] -> [Transaction] -> IO ()
inputHandler items stocks trans = do
  putStr "> "
  cmd <- getLine

  case words cmd of
    ["list", "items"] -> do
      printListCmd itemHeader renderItem items
      inputHandler items stocks trans

    ["list", "stock"] -> do
      printListCmd stockHeader renderStock stocks
      inputHandler items stocks trans

    ["list", "trans"] -> do
      printListCmd transHeader renderTransaction trans
      inputHandler items stocks trans

    ["quit"] -> do
      putStrLn "Saving..."
      save items stocks trans
      putStrLn "Bye!"

    _ -> do
      putStrLn "Unknown command."
      inputHandler items stocks trans
