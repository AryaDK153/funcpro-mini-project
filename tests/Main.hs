import Test.Tasty
import Test.Tasty.HUnit
import CustomTests.TypeTest
import CustomTests.UpdateTest
import CustomTests.CSVHandlerTest

main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [ testType
  , testCSVHandlers
  , testUpdate
  ]