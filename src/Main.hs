module Main (main) where
  
import CustomData.FileNames
import CustomData.Types
import CustomData.Updates
import Data.Time
import IO.CSVHandler

import Tests.CustomDataTest (testCustomData)
import Tests.CSVHandlerTest (testCSVHandlers)
import Tests.UpdateTest (testUpdate)

main :: IO ()
main = do
  testCustomData
  testCSVHandlers
  testUpdate