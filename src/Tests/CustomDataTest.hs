module Tests.CustomDataTest (
    testCustomData
) where

import CustomData.Types

testCustomData :: IO ()
testCustomData = do
  -- Data Types Test
  let item1 = Item 1 "Apple"
  print item1

  let itemList = [item1, Item 2 "Banana"]
  print itemList

  let stock1 = Stock item1 100 ["CA0101", "DA0202"]
  print stock1

  let stockList = [stock1, Stock (Item 2 "Banana") 50 ["CA0303"]]
  print stockList

  let transaction1 = Transaction 1 item1 20 IN 15 8 2023 14 30 0 ["CA0101"]
  print transaction1

  let transactionList = [transaction1, Transaction 2 (Item 2 "Banana") 10 OUT 16 8 2023 10 0 0 ["DA0202"]]
  print transactionList