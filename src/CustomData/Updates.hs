module CustomData.Updates (
    newItemHandler,
    newStockHandler,
    stockUpdate,
    newTransHandler
) where

import CustomData.FileNames
import CustomData.Types
import Data.List (find, partition)
import IO.CSVHandler

-- new Item rule: newName must be unique, if exist, return existing Item
newItemHandler :: [Item] -> String -> ([Item], Item)
newItemHandler existingItems newName = case find (\it -> itemName it == newName) existingItems of
    Just existing -> (existingItems, existing)
    Nothing ->
        let newID = if null existingItems then 1 else maximum (map itemID existingItems) + 1
            newItem = Item newID newName
        in (existingItems ++ [newItem], newItem)

-- new Stock rule: if Item exists, return existing Stock, else create new Stock with 0 qty and empty shelf list
newStockHandler :: [Stock] -> Item -> ([Stock], Stock)
newStockHandler existingStocks item = case find (\st -> stockItem st == item) existingStocks of
    Just existing -> (existingStocks, existing)
    Nothing ->
        let newStock = Stock item 0 []
        in (existingStocks ++ [newStock], newStock)

stockQtyUpdate :: Stock -> TransDirection -> Int -> Maybe Stock
stockQtyUpdate stock IN qty = Just stock { stockQty = stockQty stock + qty }
stockQtyUpdate stock OUT qty
  | stockQty stock >= qty = Just stock { stockQty = stockQty stock - qty }
  | otherwise = Nothing

-----------------------------------
-- FUTURE REVAMP IDEA: STOCK DB - {ID, ItemName, Cap, Qty}
-- For now we let it be unlimited capacity but still unique for each Item
-----------------------------------
stockShelfUpdate :: [Stock] -> Stock -> [String] -> ([Stock], [String])
stockShelfUpdate stocks targetStock newShelves =
    let
        -- Separate free shelves from shelves occupied by other items
        occupiedShelves = concatMap stockShelfID (filter (\s -> stockItem s /= stockItem targetStock) stocks)
        (taken, free) = partition (`elem` occupiedShelves) newShelves

        currentStockShelves = stockShelfID targetStock
        -- Only add shelves that are not already in the stock's shelf list
        freeToAdd = filter (`notElem` currentStockShelves) free

        updatedStock = targetStock { stockShelfID = stockShelfID targetStock ++ freeToAdd }
        updatedStocks = map (\s -> if stockItem s == stockItem targetStock then updatedStock else s) stocks
    in
        (updatedStocks, taken)

stockUpdate :: [Stock] -> Transaction -> Maybe [Stock]
stockUpdate stocks trans = case find (\st -> stockItem st == transItem trans) stocks of
    Nothing -> -- No stock found
        case transDirection trans of
            IN ->
                let newStock = Stock
                        { stockItem = transItem trans
                        , stockQty = transQty trans
                        , stockShelfID = transShelfID trans
                        }
                in Just (stocks ++ [newStock])
            OUT -> Nothing -- can't remove what doesn’t exist!
    Just s  -> case stockQtyUpdate s (transDirection trans) (transQty trans) of
        Nothing -> Nothing -- Insufficient stock for OUT transaction
        Just s' ->
            let (updatedStocks, _) = stockShelfUpdate stocks s' (transShelfID trans)
            in Just updatedStocks

-- new Transaction rule: validate values (stock exist, date, time), updates stock qty based on transaction direction and add shelf id if not yet in, invalid if insufficient stock on OUT transaction
newTransHandler ::
  ([Item], [Stock], [Transaction]) ->
  (Item, Int, TransDirection, (Int, Int, Int), (Int, Int, Int), [String]) ->
  Maybe ([Item], [Stock], [Transaction])
newTransHandler (existingItems, existingStocks, existingTrans) (item, qty, direction, (dd, mm, yyyy), (hh, mn, ss), shelfIDs) =
  let (validItems, validItem) = newItemHandler existingItems (itemName item)
      newID = if null existingTrans then 1 else maximum (map transID existingTrans) + 1
      newTrans = Transaction newID validItem qty direction dd mm yyyy hh mn ss shelfIDs
  in case stockUpdate existingStocks newTrans of
       Nothing -> Nothing
       Just updatedStocks -> Just (validItems, updatedStocks, existingTrans ++ [newTrans])