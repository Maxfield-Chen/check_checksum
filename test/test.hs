import Checksum
import Data.List
import Data.Map.Strict
import Data.Ord
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

unitTests =
  testGroup
    "Unit tests"
    [ testCase "incrementCharacter new item" $
        incrementCharacter empty 'a' @?= insert 'a' 1 empty
    ]
