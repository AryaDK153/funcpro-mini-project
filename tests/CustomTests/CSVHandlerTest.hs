module CustomTests.CSVHandlerTest (testCSVHandlers) where

import Test.Tasty
import Test.Tasty.HUnit
import IO.CSVHandler
import CustomData.Types

testCSVHandlers :: TestTree
testCSVHandlers =
  testGroup "CSV Parsing Tests"
    [ testCase "parseItemCSV parses simple items" $ do
        let csv =
              "ID,Name\n\
              \1,Apple\n\
              \2,Banana\n"

        parseItemCSV csv
          @?=
          [ Item 1 "Apple"
          , Item 2 "Banana"
          ]

    , testCase "parseStockCSV parses stock with shelves" $ do
        let csv =
              "ID,Name,Qty,ShelfIDs\n\
              \1,Apple,100,CA01;CA02\n"

        parseStockCSV csv
          @?=
          [ Stock (Item 1 "Apple") 100 ["CA01","CA02"] ]

    , testCase "parseTransCSV parses a full transaction" $ do
        let csv =
              "TransID,ItemID,Name,Qty,Direction,DD,MM,YYYY,HH,MN,SS,ShelfIDs\n\
              \1,1,Apple,20,IN,15,8,2023,14,30,0,CA0101;DA0202\n"

        parseTransCSV csv
          @?=
          [ Transaction 1
              (Item 1 "Apple")
              20
              IN
              15 8 2023
              14 30 0
              ["CA0101","DA0202"]
          ]
    ]
