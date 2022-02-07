import Checksum
import Checksum (splitFrequencies)
import Data.Map.Strict
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [ testCase "incrementCharacter new item" $
        incrementCharacter empty 'a' @?= insert 'a' 1 empty,
      testCase "incrementCharacter existing item" $ fromList [('a', 2)] @=? incrementCharacter (fromList [('a', 1)]) 'a',
      testCase "Frequency verification" $
        fromList [('a', 2), ('b', 3)]
          @=? T.foldl
            incrementCharacter
            empty
            (extractData $ T.pack "ababb[ab]"),
      testCase
        "SplitFrequencies empty"
        $ []
          @=? splitFrequencies
            []
            0
            [],
      testCase
        "SplitFrequencies one item"
        $ [T.singleton 'a']
          @=? splitFrequencies [('a', 1)] 1 [],
      testCase
        "SplitFrequencies two items"
        $ [T.pack "ba"]
          @=? splitFrequencies [('a', 1), ('b', 1)] 1 [],
      testCase
        "SplitFrequencies two splits"
        $ [T.pack "d", T.pack "ba"]
          @=? splitFrequencies [('a', 1), ('b', 1), ('d', 2)] 1 [],
      testCase
        "checksum one item"
        $ T.pack "a"
          @=? checksum
            (fromList [('a', 1)]),
      testCase
        "checksum two items same freq no sorting"
        $ T.pack "ab" @=? checksum (fromList [('a', 1), ('b', 1)]),
      testCase
        "checksum two items same freq sorting"
        $ T.pack "ab" @=? checksum (fromList [('b', 1), ('a', 1)]),
      testCase
        "checksum two items different freq no sorting"
        $ T.pack "ba"
          @=? checksum
            (fromList [('a', 1), ('b', 2)]),
      testCase
        "checksum two items different freq sorting"
        $ T.pack "bac"
          @=? checksum
            (fromList [('c', 1), ('a', 1), ('b', 2)]),
      testCase
        "checksum equal frequency items"
        $ T.pack "abc"
          @=? checksum
            (fromList [('c', 1), ('a', 1), ('b', 1)]),
      testCase
        "extractData"
        $ T.pack "abcde"
          @=? extractData
            (T.pack "a-b-c-d-e[abcde]"),
      testCase
        "extractChecksum"
        $ T.pack "abcde"
          @=? extractChecksum
            (T.pack "a-b-c-d-e[abcde]"),
      testCase
        "verifyChecksum one freq succeed case"
        $ True
          @=? verifyChecksum
            (T.pack "a-b-c-d-e[abcde]"),
      testCase
        "verifyChecksum one freq sort succeed case"
        $ True
          @=? verifyChecksum
            (T.pack "b-a-c-d-e[abcde]"),
      testCase
        "verifyChecksum one freq fail on higher freq case"
        $ False
          @=? verifyChecksum
            (T.pack "a-b-c-e-d-d[abcde]"),
      testCase
        "verifyChecksum two freq succeed case"
        $ True
          @=? verifyChecksum
            (T.pack "a-b-c-e-d-d[dabce]"),
      testCase
        "verifyChecksum two freq sort succeed case"
        $ True
          @=? verifyChecksum
            (T.pack "a-b-c-e-d-d-a[adbce]")
    ]
