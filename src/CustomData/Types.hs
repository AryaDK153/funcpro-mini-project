module CustomData.Types (
    Item(..),
    TransDirection(..),
    Transaction(..),
    Stock(..)
) where

data Item = Item {
    itemID :: Int,
    itemName :: String
} deriving (Eq)

instance Show Item where
    show (Item id name) =
        show id ++ replicate (4 - length (show id)) ' ' ++ "|" ++ fitName name
            where
                fitName name
                    | length name > 20  = take 17 name ++ "..."
                    | otherwise         = name ++ replicate (20 - length name) ' '


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
} deriving (Eq)

instance Show Transaction where
    show (Transaction tid item qty dir dd mm yyyy hh mn ss shelves) =
        show tid ++ replicate (4 - length (show tid)) ' ' ++ "|" ++
        show item ++ "|" ++
        show qty ++ replicate (10 - length (show qty)) ' ' ++ "|" ++
        show dir ++ replicate (3 - length (show dir)) ' ' ++ "|" ++
        showDateTime ++ "|" ++
        showShelves
            where
                showDateTime = 
                    let dayStr = if dd < 10 then '0':show dd else show dd
                        monthStr = if mm < 10 then '0':show mm else show mm
                        yearStr = if yyyy < 1000 then '0':show yyyy else show yyyy
                        hourStr = if hh < 10 then '0':show hh else show hh
                        minStr = if mn < 10 then '0':show mn else show mn
                        secStr = if ss < 10 then '0':show ss else show ss
                    in dayStr ++ "-" ++ monthStr ++ "-" ++ yearStr ++ " " ++ hourStr ++ ":" ++ minStr ++ ":" ++ secStr

                showShelves =
                    case shelves of
                        [] -> "-"
                        xs -> concatMap (++ ",") (init xs) ++ last xs


data Stock = Stock {
    stockItem :: Item,
    stockQty :: Int,
    stockShelfID :: [String]
} deriving (Eq)

instance Show Stock where
    show (Stock item qty shelves) =
        show item ++ "|" ++
        show qty ++ replicate (10 - length (show qty)) ' ' ++ "|" ++
        showShelves
            where
                showShelves =
                    case shelves of
                        [] -> "-"
                        xs -> concatMap (++ ",") (init xs) ++ last xs