module CustomTests.TypeTest (testType) where

import Test.Tasty
import Test.Tasty.HUnit

import CustomData.Types

testType :: TestTree
testType = testGroup "Custom Data Types Tests"
  [ testCase "Item constructor" $
      let item1 = Item 1 "Apple"
      in do
        itemID item1 @?= 1
        itemName item1 @?= "Apple"
    
    , testCase "Stock constructor" $
      let item1 = Item 1 "Apple"
          stock1 = Stock item1 100 ["A1", "B2"]
      in do
        itemID (stockItem stock1) @?= 1
        itemName (stockItem stock1) @?= "Apple"
        stockQty stock1 @?= 100
        stockShelfID stock1 @?= ["A1", "B2"]

    , testCase "Transaction constructor" $
      let item1 = Item 1 "Apple"
          trans1 = Transaction 1 item1 20 IN 15 8 2023 14 30 0 ["A1"]
      in do
        transID trans1 @?= 1
        itemID (transItem trans1) @?= 1
        itemName (transItem trans1) @?= "Apple"
        transQty trans1 @?= 20
        transDirection trans1 @?= IN
        transDD trans1 @?= 15
        transMM trans1 @?= 8
        transYYYY trans1 @?= 2023
        transHH trans1 @?= 14
        transMN trans1 @?= 30
        transSS trans1 @?= 0
        transShelfID trans1 @?= ["A1"]

    , testCase "getShelves for Stock" $
      let st = Stock (Item 1 "Apple") 10 ["A1", "B2"]
      in getShelves st @?= ["A1", "B2"]

    , testCase "getShelves for Transaction" $
      let t = Transaction 1 (Item 1 "Apple") 3 IN 1 1 2025 12 30 0 ["S1", "S2"]
      in getShelves t @?= ["S1", "S2"]

  ]



  -- -- Data Types Test
  -- let item1 = Item 1 "Apple"
  -- print item1

  -- let itemList = [item1, Item 2 "Banana"]
  -- print itemList

  -- let stock1 = Stock item1 100 ["CA0101", "DA0202"]
  -- print stock1

  -- let stockList = [stock1, Stock (Item 2 "Banana") 50 ["CA0303"]]
  -- print stockList

  -- let transaction1 = Transaction 1 item1 20 IN 15 8 2023 14 30 0 ["CA0101"]
  -- print transaction1

  -- let transactionList = [transaction1, Transaction 2 (Item 2 "Banana") 10 OUT 16 8 2023 10 0 0 ["DA0202"]]
  -- print transactionList