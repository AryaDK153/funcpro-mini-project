-- focuses on handling csv creation, parsing, and manipulation
module IO.CSVHandler (
    readCSV,
    parseItemCSV,
    parseStockCSV,
    parseTransCSV,
    writeItemCSV,
    writeStockCSV,
    writeTransCSV
) where

import Control.DeepSeq (force)
import Control.Exception (evaluate)

import CustomData.FileNames
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

-----------------------------------
-- REVAMP for CSV-to-List Parsing
-----------------------------------
readCSV :: FilePath -> IO String
readCSV path = do
    ioContent <- readFile path
    _ <- evaluate (force ioContent)
    return ioContent

parse :: (String -> Maybe a) -> String -> [a]
parse lineParser content = mapMaybe lineParser (drop 1 (lines content))

itemLineParser :: String -> Maybe Item
itemLineParser l = case splitCSV l of
    [iid, name] -> Just (Item (read iid) name)
    _ -> Nothing

stockLineParser :: String -> Maybe Stock
stockLineParser l = case splitCSV l of
    [iid, name, qty, sid] -> Just (Stock (Item (read iid) name)
                                         (read qty)
                                         (splitShelves sid))
    _ -> Nothing

transLineParser :: String -> Maybe Transaction
transLineParser l = case splitCSV l of
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

parseItemCSV :: String -> [Item]
parseItemCSV = parse itemLineParser

parseStockCSV :: String -> [Stock]
parseStockCSV = parse stockLineParser

parseTransCSV :: String -> [Transaction]
parseTransCSV = parse transLineParser

-----------------------------------
-- REVAMP for List-to-CSV Writing
-----------------------------------

itemComposer :: Item -> String
itemComposer (Item iid name) =
    show iid ++ "," ++ name

stockComposer :: Stock -> String
stockComposer (Stock item qty shelves) =
    show (itemID item) ++ "," ++
    itemName item ++ "," ++
    show qty ++ "," ++
    joinShelves shelves

transComposer :: Transaction -> String
transComposer t =
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
    joinShelves (transShelfID t)

writeCSV :: (a -> String) -> FilePath -> String -> [a] -> IO ()
writeCSV composer path header content = do
    writeFile path (unlines (header : map composer content))
    putStrLn "Data saved successfully!"

writeItemCSV :: [Item] -> IO ()
writeItemCSV = writeCSV itemComposer itemDB "ID,Name"

writeStockCSV :: [Stock] -> IO ()
writeStockCSV = writeCSV stockComposer stockDB "ID,Name,Qty,ShelfIDs"

writeTransCSV :: [Transaction] -> IO ()
writeTransCSV = writeCSV transComposer transDB "TransID,ItemID,Name,Qty,Direction,DD,MM,YYYY,HH,MN,SS,ShelfIDs"
