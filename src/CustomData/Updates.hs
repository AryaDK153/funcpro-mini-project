module CustomData.Updates (
    newItemHandler
) where

import CustomData.FileNames
import CustomData.Types
import Data.List (find)
import IO.CSVHandler

-- new Item rule: newName must be unique, if exist, return existing Item
newItemHandler :: [Item] -> String -> ([Item], Item)
newItemHandler existingItems newName = case find (\it -> itemName it == newName) existingItems of
    Just existing -> (existingItems, existing)
    Nothing ->
        let newID = if null existingItems then 1 else maximum (map itemID existingItems) + 1
            newItem = Item newID newName
        in (existingItems ++ [newItem], newItem)

-- new Transaction rule: updates stock based on transaction direction, invalid if insufficient stock