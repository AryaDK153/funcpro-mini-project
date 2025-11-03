-- focuses on handling csv creation, parsing, and manipulation
module IO.CSVHandler (
    readStockCSV,
    writeStockCSV,
    readTransCSV,
    writeTransCSV
) where

import Data.Types
import Data.List
import System.IO

-- simplistic CSV split (no escaping handling)
splitCSV :: String -> [String]
splitCSV line = case break (== ',') line of
    (val, ',' : rest) -> val : splitCSV rest
    (val, _)          -> [val]

readStockCSV :: FilePath -> IO [Stock]
readStockCSV path = do
    content <- readFile path
    let ls = drop 1 (lines content) -- skip header
    return [ Stock (read sid) (read qty)
           | l <- ls, let [sid, _, qty] = splitCSV l ]

writeStockCSV :: FilePath -> [Stock] -> IO ()
writeStockCSV path stocks = do
    let header = "id,name,qty\n"
        rows = [ show (stockItemID s) ++ ",," ++ show (stockQty s) | s <- stocks ]
    writeFile path (header ++ unlines rows)
