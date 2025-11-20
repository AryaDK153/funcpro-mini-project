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

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
      where (w, s'') = break p s'

timeNow :: IO (Int, Int, Int, Int, Int, Int)
timeNow = do
  currentTime <- getCurrentTime
  timezone <- getCurrentTimeZone
  let localTime = utcToLocalTime timezone currentTime
      (yyyy, mm, dd) = toGregorian $ localDay localTime
      timeOfDay = localTimeOfDay localTime
      hh = todHour timeOfDay
      mn = todMin timeOfDay
      ss = floor (todSec timeOfDay)
  return (dd, mm, fromInteger yyyy, hh, mn, ss)

formatDateTime :: Int -> Int -> Int -> Int -> Int -> Int -> String
formatDateTime dd mm yyyy hh mn ss =
  two dd ++ "-" ++ two mm ++ "-" ++ four yyyy ++ " "
  ++ two hh ++ ":" ++ two mn ++ ":" ++ two ss
  where
    two x = if x < 10 then '0' : show x else show x
    four x = if x < 1000 then replicate (4 - length (show x)) '0' ++ show x else show x

-- headers
itemHeader :: String
itemHeader =
  padOrCut 4 "ID" ++ "|" ++
  "Item Name\n" ++ replicate 25 '-'

stockHeader :: String
stockHeader = 
  padOrCut 4 "ID" ++ "|" ++
  padOrCut 20 "Item Name" ++ "|" ++
  padOrCut 10 "Qty" ++ "|" ++
  "Shelves\n" ++ replicate 57 '-'

transHeader :: String
transHeader =
  padOrCut 4 "TID" ++ "|" ++
  padOrCut 4 "IID" ++ "|" ++
  padOrCut 20 "Item Name" ++ "|" ++
  padOrCut 10 "Qty" ++ "|" ++
  "Dir|" ++
  padOrCut 19 "Date & Time" ++ "|" ++
  "Shelves\n" ++ replicate 86 '-'

opsInt :: [(String, Int -> Int -> Bool)]
opsInt =
  [ ("=", (==)), ("==", (==)), ("!=", (/=)), ("/=", (/=)),
    ("<", (<)), (">", (>)), ("<=", (<=)), (">=", (>=)) ]

opsStr :: [(String, String -> String -> Bool)]
opsStr =
  [ ("=", (==)), ("==", (==)), ("!=", (/=)), ("/=", (/=)) ]

matchOpInt :: String -> Maybe (Int -> Int -> Bool)
matchOpInt = flip lookup opsInt

matchOpStr :: String -> Maybe (String -> String -> Bool)
matchOpStr = flip lookup opsStr

-- DD-MM-YYYY
dateMatch :: (Int -> Int -> Bool) -> String -> (Transaction -> Bool)
dateMatch op value =
  case wordsWhen (=='-') value of
    [ddStr, mmStr, yyyyStr] ->
      let dd' = read ddStr :: Int
          mm' = read mmStr :: Int
          yyyy' = read yyyyStr :: Int
      in \t ->
          let dateInt  (y, m, d) = y * 10000 + m * 100 + d
              (syyyy, smm, sdd) = (transYYYY t, transMM t, transDD t)
          in op (dateInt (syyyy, smm, sdd)) (dateInt (yyyy', mm', dd'))
    _ -> const False

-- HH:MM:SS
timeMatch :: (Int -> Int -> Bool) -> String -> (Transaction -> Bool)
timeMatch op value =
  case wordsWhen (==':') value of
    [hhStr, mnStr, ssStr] ->
      let hh' = read hhStr :: Int
          mn' = read mnStr :: Int
          ss' = read ssStr :: Int
      in \t ->
          let timeInt (hh, mn, ss) = hh * 10000 + mn * 100 + ss
              (thh, tmn, tss) = (transHH t, transMN t, transSS t)
          in op (timeInt (thh, tmn, tss)) (timeInt (hh', mn', ss'))
    _ -> const False

-- [String]
shelfMatch :: HasShelves a => (String -> String -> Bool) -> String -> (a -> Bool)
shelfMatch op value = (\x -> any (\s -> op s value) (getShelves x))

itemMatch :: String -> String -> String -> Maybe (Item -> Bool)
itemMatch key eqOp value =
  case key of
    "id"   -> do
        op <- matchOpInt eqOp
        let vid = read value :: Int
        return (\item -> op (itemID item) vid)

    "name" -> do
        op <- matchOpStr eqOp
        return (\item -> op (itemName item) value)

    _ -> Nothing

stockMatch :: String -> String -> String -> Maybe (Stock -> Bool)
stockMatch key eqOp value
  | key `elem` ["id", "qty"] = do
      op <- matchOpInt eqOp
      let vInt = read value :: Int
      case key of
        "id"  -> Just (\stock -> op (itemID (stockItem stock)) vInt)
        "qty" -> Just (\stock -> op (stockQty stock) vInt)
  | key `elem` ["name", "shelves"] = do
      op <- matchOpStr eqOp
      case key of
        "name"    -> Just (\stock -> op (itemName (stockItem stock)) value)
        "shelves" -> Just (shelfMatch op value)
  | otherwise = Nothing

transMatch :: String -> String -> String -> Maybe (Transaction -> Bool)
transMatch key eqOp value
  | key `elem` ["tid", "iid", "qty"] = do
      op <- matchOpInt eqOp
      let vInt = read value :: Int
      case key of
        "tid"       -> Just (\trans -> op (transID trans) vInt)
        "iid"       -> Just (\trans -> op (itemID (transItem trans)) vInt)
        "qty"       -> Just (\trans -> op (transQty trans) vInt)
  | key `elem` ["name", "dir", "shelves"] = do
      op <- matchOpStr eqOp
      case key of
        "name"      -> Just (\trans -> op (itemName (transItem trans)) value)
        "dir"       -> Just (\trans -> op (show (transDirection trans)) value)
        "shelves"   -> Just (shelfMatch op value)
  | key == "date" = do
      op <- matchOpInt eqOp
      return (dateMatch op value)
  | key == "time" = do
      op <- matchOpInt eqOp
      return (timeMatch op value)
  | otherwise = Nothing

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


-- commands
printListCmd :: String -> (a -> String) -> [a] -> IO ()
printListCmd header render xs = do
  putStrLn ""
  putStrLn header
  mapM_ (putStrLn . render) xs

-- TODO: get by condition cmd --- filter xs (list of items/stocks/transactions) based on criteria (key eqOp value)
findWhereCmd :: (Show a) => (a -> Bool) -> (String, (a -> String), [a]) -> IO ()
findWhereCmd filterFunc (header, renderer, source) =
  let results = filter filterFunc source
  in printListCmd header renderer results

-- TODO: stock report cmd --- print stock and list of transactions related to that stock
-- reportCmd stockItemName stocks trans

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

    ["transact", itemName, qtyStr, dirStr, shelfIDsStr] -> do
      let qty = read qtyStr :: Int
          dir = if dirStr == "IN" then IN else OUT
          shelfIDs = wordsWhen (==',') shelfIDsStr
      (dd, mm, yyyy, hh, mn, ss) <- timeNow
      case addTransactionCmd itemName qty dir (dd, mm, yyyy, hh, mn, ss) shelfIDs items stocks trans of
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
