-- focuses on handling csv creation, parsing, and manipulation
module IO.CSVHandler (
    readItemCSV,
    writeItemCSV,
    readStockCSV,
    writeStockCSV,
    readTransCSV,
    writeTransCSV
) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)

import CustomData.Types
import Data.Maybe (mapMaybe)
import Data.List (intercalate)

-- simple CSV split (no quote handling)
splitCSV :: String -> [String]
splitCSV line = case break (== ',') line of
    (val, ',' : rest) -> val : splitCSV rest
    (val, _)          -> [val]

-- split semicolon-delimited list (for shelf IDs)
splitShelves :: String -> [String]
splitShelves s = case break (== ';') s of
    (val, ';' : rest) -> val : splitShelves rest
    (val, _)          -> [val]

joinShelves :: [String] -> String
joinShelves = intercalate ";"

readForced :: FilePath -> IO String
readForced path = do
    content <- readFile path
    _ <- evaluate (force content)
    return content

-----------------------------------
-- ITEM
-----------------------------------

readItemCSV :: FilePath -> IO [Item]
readItemCSV path = do
    content <- readForced path
    let ls = drop 1 (lines content)
    let parseLine l = case splitCSV l of
            [iid, name] -> Just (Item (read iid) name)
            _              -> Nothing
    return (mapMaybe parseLine ls)

writeItemCSV :: FilePath -> [Item] -> IO ()
writeItemCSV path items = do
    let header = "ID,Name"
    let rows = map (\(Item iid name) ->
                      show iid ++ "," ++ name) items
    writeFile path (unlines (header : rows))
    putStrLn "Item data written successfully!"

-----------------------------------
-- STOCK
-----------------------------------

readStockCSV :: FilePath -> IO [Stock]
readStockCSV path = do
    content <- readForced path
    let ls = drop 1 (lines content)
    let parseLine l = case splitCSV l of
            [iid, name, qty, sid] ->
                Just (Stock (Item (read iid) name)
                            (read qty)
                            (splitShelves sid))
            _ -> Nothing
    return (mapMaybe parseLine ls)

writeStockCSV :: FilePath -> [Stock] -> IO ()
writeStockCSV path stocks = do
    let header = "ID,Name,Qty,ShelfIDs"
    let rows = map (\(Stock item qty shelves) ->
                      show (itemID item) ++ "," ++
                      itemName item ++ "," ++
                      show qty ++ "," ++
                      joinShelves shelves) stocks
    writeFile path (unlines (header : rows))
    putStrLn "Stock data written successfully!"


-----------------------------------
-- TRANSACTIONS
-----------------------------------

readTransCSV :: FilePath -> IO [Transaction]
readTransCSV path = do
    content <- readForced path
    let ls = drop 1 (lines content)
    let parseLine l = case splitCSV l of
            [tid, iid, name, qty, dir, dd, mm, yyyy, hh, mn, ss, sid] ->
                Just (Transaction (read tid)
                                  (Item (read iid) name)
                                  (read qty)
                                  (read dir)
                                  (read dd)
                                  (read mm)
                                  (read yyyy)
                                  (read hh)
                                  (read mn)
                                  (read ss)
                                  (splitShelves sid))
            _ -> Nothing
    return (mapMaybe parseLine ls)

writeTransCSV :: FilePath -> [Transaction] -> IO ()
writeTransCSV path trans = do
    let header = "TransID,ItemID,Name,Qty,Direction,DD,MM,YYYY,HH,MN,SS,ShelfIDs"
    let rows = map (\t ->
                      show (transID t) ++ "," ++
                      show (itemID (transItem t)) ++ "," ++
                      itemName (transItem t) ++ "," ++
                      show (transQty t) ++ "," ++
                      show (transDirection t) ++ "," ++
                      show (transDD t) ++ "," ++
                      show (transMM t) ++ "," ++
                      show (transYYYY t) ++ "," ++
                      show (transHH t) ++ "," ++
                      show (transMN t) ++ "," ++
                      show (transSS t) ++ "," ++
                      joinShelves (transShelfID t)) trans
    writeFile path (unlines (header : rows))
    putStrLn "Transaction data written successfully!"
