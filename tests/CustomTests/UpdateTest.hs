module CustomTests.UpdateTest (testUpdate) where

import Test.Tasty
import Test.Tasty.HUnit

import CustomData.Types
import CustomData.Updates

-- TODO: test dummies (duplication issue)

testUpdate :: TestTree
testUpdate = testGroup "Custom Data Updates Tests"
  [ testCase "newItemHandler adds and returns new item" $
      let initialItems = [Item 1 "Apple", Item 2 "Banana"]
          (updatedItems, newItem) = newItemHandler initialItems "Cherry"
      in do
        itemID newItem @?= 3
        itemName newItem @?= "Cherry"
        length updatedItems @?= 3
        itemID (updatedItems !! 2) @?= 3
        itemName (updatedItems !! 2) @?= "Cherry"
    
  , testCase "newItemHandler finds and returns existing item" $
      let initialItems = [Item 1 "Apple", Item 2 "Banana"]
          (updatedItems, existingItem) = newItemHandler initialItems "Apple"
      in do
        itemID existingItem @?= 1
        itemName existingItem @?= "Apple"
        updatedItems @?= initialItems
  
  , testCase "newStockHandler adds new stock for new item" $
      let initialStocks = [Stock (Item 1 "Apple") 100 ["A1"]]
          (updatedStocks, newStock) = newStockHandler initialStocks (Item 2 "Banana")
      in do
        itemID (stockItem newStock) @?= 2
        itemName (stockItem newStock) @?= "Banana"
        stockQty newStock @?= 0
        stockShelfID newStock @?= []
        length updatedStocks @?= 2
        itemID (stockItem (updatedStocks !! 1)) @?= 2
        itemName (stockItem (updatedStocks !! 1)) @?= "Banana"
        stockQty (updatedStocks !! 1) @?= 0
        stockShelfID (updatedStocks !! 1) @?= []

  , testCase "newStockHandler reuses existing stock" $
      let initialStocks = [Stock (Item 1 "Apple") 100 ["A1"]]
          (updatedStocks, existingStock) = newStockHandler initialStocks (Item 1 "Apple")
      in do
        itemID (stockItem existingStock) @?= 1
        itemName (stockItem existingStock) @?= "Apple"
        stockQty existingStock @?= 100
        stockShelfID existingStock @?= ["A1"]
        updatedStocks @?= initialStocks

  , testCase "stockQtyUpdate adds stockQty count" $
      let initialQty = 100
          initialStock = Stock (Item 1 "Apple") initialQty ["A1"]
          moveQty = 105
      in case stockQtyUpdate initialStock IN moveQty of
          Just updatedStock -> do
            itemID (stockItem updatedStock) @?= 1
            itemName (stockItem updatedStock) @?= "Apple"
            stockQty updatedStock @?= initialQty + moveQty
            stockShelfID updatedStock @?= ["A1"]
          Nothing ->
            assertFailure "Expected successful IN update, got Nothing"

  , testCase "stockQtyUpdate subtracts stockQty count" $
      let initialQty = 100
          initialStock = Stock (Item 1 "Apple") initialQty ["A1"]
          moveQty = 95
      in case stockQtyUpdate initialStock OUT moveQty of
          Just updatedStock -> do
            itemID (stockItem updatedStock) @?= 1
            itemName (stockItem updatedStock) @?= "Apple"
            stockQty updatedStock @?= initialQty - moveQty
            stockShelfID updatedStock @?= ["A1"]
          Nothing ->
            assertFailure "Expected successful OUT update, got Nothing"

  , testCase "stockQtyUpdate fails when OUT > stockQty" $
      let initialQty = 100
          initialStock = Stock (Item 1 "Apple") initialQty ["A1"]
          moveQty = 105
      in case stockQtyUpdate initialStock OUT moveQty of
          Nothing -> return ()
          Just _  -> assertFailure "Expected update failure but got Just"

  , testCase "stockShelfUpdate basic behavior" $
      let stockA = Stock (Item 1 "Apple")    10 ["A1", "A2"]
          stockB = Stock (Item 2 "Banana")   5  ["B1"]
          allStocks = [stockA, stockB]
          newShelves = ["A2", "A3", "B1", "C1"]
          (updated, taken) = stockShelfUpdate allStocks stockA newShelves
      -- Expected:
      -- - "A2" is already in Apple → ignored
      -- - "A3" is new and free → added
      -- - "B1" is occupied by Banana → goes to `taken`
      -- - "C1" is new and free → added  
    in do
      map stockItem updated @?= map stockItem allStocks
      taken @?= ["B1"]
      stockShelfID (head updated) @?= ["A1","A2","A3","C1"]

  , testCase "stockUpdate IN updates qty and shelves properly" $
      let apple = Item 1 "Apple"
          banana = Item 2 "Banana"
          stockA = Stock apple 10 ["A1"]
          stockB = Stock banana 5  ["B1"]
          allStocks = [stockA, stockB]
          moveQty = 3
          trans = Transaction 1 apple moveQty IN 11 11 1111 11 11 11 ["A1", "B1", "A2"]
          result = stockUpdate allStocks trans
      in case result of
        Nothing -> assertFailure "Expected successful IN update, got Nothing"
        Just updated -> do
          stockQty (head updated) @?= (stockQty stockA) + moveQty
          stockShelfID (head updated) @?= ["A1","A2"]
          map stockItem updated @?= map stockItem allStocks

  , testCase "stockUpdate OUT reduces quantity" $
      let apple = Item 1 "Apple"
          stockA = Stock apple 10 ["A1"]
          allStocks = [stockA]
          moveQty = 4
          trans = Transaction 1 apple moveQty OUT 11 11 1111 11 11 11 ["A1", "B1", "A2"]
          result = stockUpdate allStocks trans
      in case result of
        Nothing -> assertFailure "Expected OUT to succeed"
        Just updated -> stockQty (head updated) @?= (stockQty stockA) - moveQty

  , testCase "stockUpdate OUT fails on insufficient qty" $
      let apple = Item 1 "Apple"
          stockA = Stock apple 3 ["A1"]
          moveQty = 4
          trans = Transaction 1 apple moveQty OUT 11 11 1111 11 11 11 ["A1", "B1", "A2"]
      in stockUpdate [stockA] trans @?= Nothing

  , testCase "newTransHandler creates new item if missing" $
      let items  = [Item 1 "Apple"]
          stocks = [Stock (Item 1 "Apple") 10 ["A1"]]
          trans  = []
          input  = ("Banana", 5, IN, (1,1,2025), (10,0,0), ["B1"])
          result = newTransHandler (items, stocks, trans) input
      in case result of
        Nothing -> assertFailure "Expected new item + transaction"
        Just (newItems, newStocks, newTrans) -> do
          length newItems @?= 2
          itemName (last newItems) @?= "Banana"
          length newTrans @?= 1
          transItem (head newTrans) @?= Item 2 "Banana"
          map stockItem newStocks @?= [Item 1 "Apple", Item 2 "Banana"]

  , testCase "newTransHandler reuses existing item" $
      let items  = [Item 1 "Apple", Item 2 "Banana"]
          stocks = [Stock (Item 1 "Apple") 10 ["A1"], Stock (Item 2 "Banana") 5 ["B1"]]
          trans  = []
          input  = ("Banana", 3, IN, (1,1,2025), (10,0,0), ["B2"])
          result = newTransHandler (items, stocks, trans) input
      in case result of
        Nothing -> assertFailure "Expected reuse of existing item"
        Just (newItems, newStocks, newTrans) -> do
          newItems @?= items
          stockQty (newStocks !! 1) @?= 5 + 3
          stockShelfID (newStocks !! 1) @?= ["B1","B2"]
          length newTrans @?= 1
          transItem (head newTrans) @?= Item 2 "Banana"

  , testCase "newTransHandler fails OUT when not enough stock" $
      let items  = [Item 1 "Apple"]
          stocks = [Stock (Item 1 "Apple") 10 ["A1"]]
          trans  = []
          input  = ("Apple", 20, OUT, (1,1,2025), (10,0,0), ["A1"])
          result = newTransHandler (items, stocks, trans) input
      in result @?= Nothing

  , testCase "newTransHandler assigns correct next transaction ID" $
      let items  = [Item 1 "Apple"]
          stocks = [Stock (Item 1 "Apple") 10 ["A1"]]
          trans  =
            [ Transaction 1 (Item 1 "Apple") 2 IN 1 1 2025 9 0 0 ["A1"]
            , Transaction 2 (Item 1 "Apple") 3 OUT 1 1 2025 9 5 0 ["A1"]
            ]
          input = ("Apple", 4, IN, (1,1,2025), (10,0,0), ["A2"])
          result = newTransHandler (items, stocks, trans) input
      in case result of
        Nothing -> assertFailure "Expected successful transaction append"
        Just (_, _, newTrans) -> do
          length newTrans @?= 3
          transID (last newTrans) @?= 3

  ]