module CustomData.Formats (
  strLower,
  padOrCut,
  wordsWhen,
  timeNow,
  formatDateTime,
  itemHeader,
  stockHeader,
  transHeader,
  reportHeader,
  renderItem,
  renderStock,
  renderTransaction,
  renderReport
)
where

import CustomData.Types
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Time

strLower :: String -> String
strLower = map toLower

padOrCut :: Int -> String -> String
padOrCut n str
  | length str > n = take (n - 3) str ++ "..."
  | otherwise      = str ++ replicate (n - length str) ' '

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
      where (w, s'') = break p s'

timeNow :: IO (Int, Int, Int, Int, Int, Int)
timeNow = do
  currentTime <- getCurrentTime
  timezone <- getCurrentTimeZone
  let localTime = utcToLocalTime timezone currentTime
      (yyyy, mm, dd) = toGregorian $ localDay localTime
      timeOfDay = localTimeOfDay localTime
      hh = todHour timeOfDay
      mn = todMin timeOfDay
      ss = floor (todSec timeOfDay)
  return (dd, mm, fromInteger yyyy, hh, mn, ss)

formatDateTime :: Int -> Int -> Int -> Int -> Int -> Int -> String
formatDateTime dd mm yyyy hh mn ss =
  two dd ++ "-" ++ two mm ++ "-" ++ four yyyy ++ " "
  ++ two hh ++ ":" ++ two mn ++ ":" ++ two ss
  where
    two x = if x < 10 then '0' : show x else show x
    four x = if x < 1000 then replicate (4 - length (show x)) '0' ++ show x else show x

-- headers & renderers
itemHeader :: String
itemHeader =
  padOrCut 4 "ID" ++ "|" ++
  "Item Name\n" ++ replicate 25 '-'

stockHeader :: String
stockHeader = 
  padOrCut 4 "ID" ++ "|" ++
  padOrCut 20 "Item Name" ++ "|" ++
  padOrCut 10 "Qty" ++ "|" ++
  "Shelves\n" ++ replicate 57 '-'

transHeader :: String
transHeader =
  padOrCut 4 "TID" ++ "|" ++
  padOrCut 4 "IID" ++ "|" ++
  padOrCut 20 "Item Name" ++ "|" ++
  padOrCut 10 "Qty" ++ "|" ++
  "Dir|" ++
  padOrCut 19 "Date & Time" ++ "|" ++
  "Shelves\n" ++ replicate 86 '-'

reportHeader :: String -> String
reportHeader targetStockItemName =
  "Stock Report for " ++ targetStockItemName ++ "\n" ++ replicate 37 '-' ++ "\n" ++
  padOrCut 10 "In Stock" ++ "|" ++
  padOrCut 10 "Total In" ++ "|" ++
  padOrCut 10 "Total Out" ++ "|" ++
  padOrCut 10 "Net Change" ++ "|" ++
  padOrCut 19 "Last Trans" ++ "|" ++
  "Shelves\n" ++ replicate 84 '-'

renderItem :: Item -> String
renderItem (Item iid name) =
  padOrCut 4 (show iid) ++ "|" ++ padOrCut 20 name
  
renderStock :: Stock -> String
renderStock (Stock item qty shelves) =
  renderItem item ++ "|" ++ padOrCut 10 (show qty) ++ "|" ++ padOrCut 20 (intercalate "," shelves)

renderTransaction :: Transaction -> String
renderTransaction (Transaction tid item qty dir dd mm yyyy hh mn ss shelves) =
  padOrCut 4 (show tid) ++ "|" ++
  renderItem item ++ "|" ++
  padOrCut 10 (show qty) ++ "|" ++
  padOrCut 3 (show dir) ++ "|" ++
  formatDateTime dd mm yyyy hh mn ss ++ "|" ++
  padOrCut 20 (intercalate "," shelves)

renderReport :: Int -> Int -> Int -> Int -> (Int, Int, Int, Int, Int, Int) -> [String] -> String
renderReport inStock totalIn totalOut netChange (dd, mm, yyyy, hh, mn, ss) shelves =
  padOrCut 10 (show inStock) ++ "|" ++
  padOrCut 10 (show totalIn) ++ "|" ++
  padOrCut 10 (show totalOut) ++ "|" ++
  padOrCut 10 (show netChange) ++ "|" ++
  formatDateTime dd mm yyyy hh mn ss ++ "|" ++
  padOrCut 20 (intercalate "," shelves)