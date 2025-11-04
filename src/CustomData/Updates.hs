module CustomData.Updates (
    newItem
) where

import CustomData.FileNames
import CustomData.Types
import Data.List (find)
import IO.CSVHandler

-- new Item rule: newName must be unique, if exist, return existing Item
newItem :: String -> IO Item
newItem newName = do
    items <- readItemCSV itemDB
    case find (\it -> itemName it == newName) items of
        Just existing -> do
            putStrLn $ "Item \"" ++ newName ++ "\" already exists (ID: " ++ show (itemID existing) ++ ")."
            return existing
        Nothing -> do
            let newID = if null items then 1 else maximum (map itemID items) + 1
            let item = Item newID newName
            writeItemCSV itemDB (items ++ [item])
            putStrLn $ "Created new item \"" ++ newName ++ "\" with ID " ++ show newID ++ "."
            return item

-- new Transaction rule: updates stock based on transaction direction, invalid if insufficient stock