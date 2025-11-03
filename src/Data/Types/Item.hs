-- focuses on defining data types for items
module Data.Types.Item (
    Item(..)
) where

data Item = Item {
    itemID :: Int,
    itemName :: String,
    itemQty :: Int
} deriving (Show, Eq)