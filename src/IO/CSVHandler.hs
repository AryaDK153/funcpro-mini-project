-- focuses on handling csv creation, parsing, and manipulation
module IO.CSVHandler (
    readStockCSV,
    writeStockCSV,
    -- readTransCSV,
    -- writeTransCSV
) where

import CustomData.Types
-- import Data.List
-- import System.IO
import Data.Maybe (mapMaybe)

splitCSV :: String -> [String]
splitCSV line = case break (== ',') line of
    (val, ',' : rest) -> val : splitCSV rest
    (val, _)          -> [val]

readStockCSV :: FilePath -> IO [Stock]
readStockCSV path = do
    content <- readFile path
    let ls = drop 1 (lines content)
    let parseLine l = case splitCSV l of
            [sid, name, qty, shelf] ->
                Just (Stock (Item (read sid) name (read qty)) (read shelf))
            _ -> Nothing
    return (mapMaybe parseLine ls)

writeStockCSV :: FilePath -> [Stock] -> IO ()
writeStockCSV path stocks = do
    let header = "ID,Name,Qty,ShelfID"
    let rows = map (\(Stock item shelf) ->
                      show (itemID item) ++ "," ++
                      itemName item ++ "," ++
                      show (itemQty item) ++ "," ++
                      show shelf) stocks
    writeFile path (unlines (header : rows))
    putStrLn "Stock data written successfully!"