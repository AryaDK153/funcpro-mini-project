module CustomData.Matchings (
  itemMatch,
  stockMatch,
  transMatch
) where

import CustomData.Types (Item(..), Stock(..), Transaction(..), HasShelves(..))
import CustomData.Formats(wordsWhen)

opsInt :: [(String, Int -> Int -> Bool)]
opsInt =
  [ ("=", (==)), ("==", (==)), ("!=", (/=)), ("/=", (/=)),
    ("<", (<)), (">", (>)), ("<=", (<=)), (">=", (>=)) ]

opsStr :: [(String, String -> String -> Bool)]
opsStr =
  [ ("=", (==)), ("==", (==)), ("!=", (/=)), ("/=", (/=)) ]

matchOpInt :: String -> Maybe (Int -> Int -> Bool)
matchOpInt = flip lookup opsInt

matchOpStr :: String -> Maybe (String -> String -> Bool)
matchOpStr = flip lookup opsStr

-- DD-MM-YYYY
dateMatch :: (Int -> Int -> Bool) -> String -> (Transaction -> Bool)
dateMatch op value =
  case wordsWhen (=='-') value of
    [ddStr, mmStr, yyyyStr] ->
      let dd' = read ddStr :: Int
          mm' = read mmStr :: Int
          yyyy' = read yyyyStr :: Int
      in \t ->
          let dateInt  (y, m, d) = y * 10000 + m * 100 + d
              (syyyy, smm, sdd) = (transYYYY t, transMM t, transDD t)
          in op (dateInt (syyyy, smm, sdd)) (dateInt (yyyy', mm', dd'))
    _ -> const False

-- HH:MM:SS
timeMatch :: (Int -> Int -> Bool) -> String -> (Transaction -> Bool)
timeMatch op value =
  case wordsWhen (==':') value of
    [hhStr, mnStr, ssStr] ->
      let hh' = read hhStr :: Int
          mn' = read mnStr :: Int
          ss' = read ssStr :: Int
      in \t ->
          let timeInt (hh, mn, ss) = hh * 10000 + mn * 100 + ss
              (thh, tmn, tss) = (transHH t, transMN t, transSS t)
          in op (timeInt (thh, tmn, tss)) (timeInt (hh', mn', ss'))
    _ -> const False

-- [String]
shelfMatch :: HasShelves a => (String -> String -> Bool) -> String -> (a -> Bool)
shelfMatch op value = (\x -> any (\s -> op s value) (getShelves x))

itemMatch :: String -> String -> String -> Maybe (Item -> Bool)
itemMatch key eqOp value =
  case key of
    "id"   -> do
        op <- matchOpInt eqOp
        let vid = read value :: Int
        return (\item -> op (itemID item) vid)

    "name" -> do
        op <- matchOpStr eqOp
        return (\item -> op (itemName item) value)

    _ -> Nothing

stockMatch :: String -> String -> String -> Maybe (Stock -> Bool)
stockMatch key eqOp value
  | key `elem` ["id", "qty"] = do
      op <- matchOpInt eqOp
      let vInt = read value :: Int
      case key of
        "id"  -> Just (\stock -> op (itemID (stockItem stock)) vInt)
        "qty" -> Just (\stock -> op (stockQty stock) vInt)
  | key `elem` ["name", "shelves"] = do
      op <- matchOpStr eqOp
      case key of
        "name"    -> Just (\stock -> op (itemName (stockItem stock)) value)
        "shelves" -> Just (shelfMatch op value)
  | otherwise = Nothing

transMatch :: String -> String -> String -> Maybe (Transaction -> Bool)
transMatch key eqOp value
  | key `elem` ["tid", "iid", "qty"] = do
      op <- matchOpInt eqOp
      let vInt = read value :: Int
      case key of
        "tid"       -> Just (\trans -> op (transID trans) vInt)
        "iid"       -> Just (\trans -> op (itemID (transItem trans)) vInt)
        "qty"       -> Just (\trans -> op (transQty trans) vInt)
  | key `elem` ["name", "dir", "shelves"] = do
      op <- matchOpStr eqOp
      case key of
        "name"      -> Just (\trans -> op (itemName (transItem trans)) value)
        "dir"       -> Just (\trans -> op (show (transDirection trans)) value)
        "shelves"   -> Just (shelfMatch op value)
  | key == "date" = do
      op <- matchOpInt eqOp
      return (dateMatch op value)
  | key == "time" = do
      op <- matchOpInt eqOp
      return (timeMatch op value)
  | otherwise = Nothing