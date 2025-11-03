module CustomData.Types (
    Item(..),
    Transaction(..),
    Stock(..)
) where

data Item = Item {
    itemID :: Int,
    itemName :: String,
    itemQty :: Int
} deriving (Show, Eq)


data TransDirection = IN | OUT
  deriving (Show, Read, Eq)

data Transaction = Transaction {
    transID :: Int,
    transItem :: Item,
    transQty :: Int,
    transDirection :: TransDirection,
    transDD :: Int,
    transMM :: Int,
    transYYYY :: Int,
    transHH :: Int,
    transMN :: Int,
    transSS :: Int,
    transShelfID :: Int
} deriving (Show, Eq)


data Stock = Stock {
    stockItem :: Item,
    stockShelfID :: Int
} deriving (Show, Eq)