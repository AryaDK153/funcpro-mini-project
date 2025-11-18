module CustomData.Types (
    Item(..),
    TransDirection(..),
    Transaction(..),
    Stock(..)
) where

data Item = Item {
    itemID :: Int,
    itemName :: String
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
    transShelfID :: [String]
} deriving (Show, Eq)


data Stock = Stock {
    stockItem :: Item,
    stockQty :: Int,
    stockShelfID :: [String]
} deriving (Show, Eq)