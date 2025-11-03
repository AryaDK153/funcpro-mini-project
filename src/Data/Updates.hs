-- mainly focuses on list comprehensions
module Data.Updates (
    updateQty
) where


import Data.Types

updateQty :: Int -> Item -> Item
updateQty newQty item = item { itemQty = itemQty item + newQty }