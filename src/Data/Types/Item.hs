-- focuses on defining data types for items
module Data.Types.Item where

data Item = Item {
    itemID :: Int,
    itemName :: String,
    itemQty :: Int
}