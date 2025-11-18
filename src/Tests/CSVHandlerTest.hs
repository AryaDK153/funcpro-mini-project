module Tests.CSVHandlerTest (
  testCSVHandlers
) where

import IO.CLUI (load)
import IO.CSVHandler
import CustomData.FileNames
import CustomData.Types

testCSVHandlers :: IO ()
testCSVHandlers = do
  -- CSV Handling Test
  (itemListLoaded, stockListLoaded, transListLoaded) <- load

  print itemListLoaded
  print stockListLoaded
  print transListLoaded