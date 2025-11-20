-- Command Line User Interface (CLUI) module
module IO.CLUI (
  inputHandler,
  load,
  save,
) where

import CustomData.FileNames
import CustomData.Formats
import CustomData.Matchings
import CustomData.Types
import CustomData.Updates
import IO.CSVHandler

-- commands
printListCmd :: String -> (a -> String) -> [a] -> IO ()
printListCmd header render xs = do
  putStrLn ""
  putStrLn header
  mapM_ (putStrLn . render) xs

-- TODO: get by condition cmd --- filter xs (list of items/stocks/transactions) based on criteria (key eqOp value)
findWhereCmd :: (a -> Bool) -> (String, (a -> String), [a]) -> IO ()
findWhereCmd filterFunc (header, renderer, source) =
  let results = filter filterFunc source
  in printListCmd header renderer results

-- TODO: stock report cmd --- print stock and list of transactions related to that stock
reportCmd :: String -> [Stock] -> [Transaction] -> IO ()
reportCmd stockItemName stocks trans = do
  let mStock = filter (\s -> strLower (itemName (stockItem s)) == strLower stockItemName) stocks

  case mStock of
    [] -> putStrLn "Item not found in stock."
    (stock:_) -> do
      let inStock = stockQty stock
          shelves = getShelves stock

      let relatedTrans =
            filter (\t -> strLower (itemName (transItem t)) == strLower stockItemName) trans

      let totalIn  = sum [transQty t | t <- relatedTrans, transDirection t == IN]
          totalOut = sum [transQty t | t <- relatedTrans, transDirection t == OUT]
          netChange = totalIn - totalOut

      let lastTrans
            | null relatedTrans = (0,0,0,0,0,0)
            | otherwise =
                let t = last relatedTrans
                in  ( transDD t, transMM t, transYYYY t
                    , transHH t, transMN t, transSS t )

      putStrLn ""
      putStrLn (reportHeader stockItemName)
      putStrLn (renderReport inStock totalIn totalOut netChange lastTrans shelves)

addTransactionCmd ::
  String -> Int -> TransDirection -> (Int, Int, Int, Int, Int, Int) -> [String] ->
  [Item] -> [Stock] -> [Transaction] ->
  Maybe ([Item], [Stock], [Transaction])
addTransactionCmd targetItemName qty dir (dd, mm, yyyy, hh, mn, ss) shelves items stocks transactions =
  newTransHandler (items, stocks, transactions)
    (targetItemName, qty, dir, (dd, mm, yyyy), (hh, mn, ss), shelves)

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


-- CLUI main loop
inputHandler :: [Item] -> [Stock] -> [Transaction] -> IO ()
inputHandler items stocks trans = do
  putStr "> "
  cmd <- getLine

  case words cmd of
    ["report", targetItemName] -> do
      reportCmd targetItemName stocks trans
      inputHandler items stocks trans

    ["list", dataType] -> do
      case dataType of
        "item"  -> printListCmd itemHeader renderItem items
        "stock" -> printListCmd stockHeader renderStock stocks
        "trans" -> printListCmd transHeader renderTransaction trans
        _       -> putStrLn "Unknown data type."
      inputHandler items stocks trans

    ["find", dataType, "where", key, eqOp, value] -> do
      case dataType of
        "item"  ->
          case itemMatch key eqOp value of
            Just filterFunc -> findWhereCmd filterFunc (itemHeader, renderItem, items)
            Nothing         -> putStrLn "Invalid key or operator."
        "stock" -> 
          case stockMatch key eqOp value of
            Just filterFunc -> findWhereCmd filterFunc (stockHeader, renderStock, stocks)
            Nothing         -> putStrLn "Invalid key or operator."
        "trans" ->
          case transMatch key eqOp value of
            Just filterFunc -> findWhereCmd filterFunc (transHeader, renderTransaction, trans)
            Nothing         -> putStrLn "Invalid key or operator."
        _       -> putStrLn "Unknown data type."
      inputHandler items stocks trans

    ["transact", targetItemName, qtyStr, dirStr, shelfIDsStr] -> do
      let qty = read qtyStr :: Int
          dir = if dirStr == "IN" then IN else OUT
          shelfIDs = wordsWhen (==',') shelfIDsStr
      (dd, mm, yyyy, hh, mn, ss) <- timeNow
      case addTransactionCmd targetItemName qty dir (dd, mm, yyyy, hh, mn, ss) shelfIDs items stocks trans of
        Just (newItems, newStocks, newTrans) -> do
          putStrLn "Transaction successful."
          inputHandler newItems newStocks newTrans
        Nothing -> do
          putStrLn "Transaction failed (invalid operation)."
          inputHandler items stocks trans

    ["quit"] -> do
      putStrLn "Saving..."
      save items stocks trans
      putStrLn "Bye!"

    _ -> do
      putStrLn "Unknown command."
      inputHandler items stocks trans
