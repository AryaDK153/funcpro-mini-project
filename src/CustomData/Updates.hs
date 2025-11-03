-- mainly focuses on list comprehensions
module CustomData.Updates (
    updateQty
) where


import CustomData.Types

updateQty :: Int -> Item -> Item
updateQty newQty item = item { itemQty = itemQty item + newQty }