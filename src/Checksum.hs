module Checksum where

import Data.List
import Data.Map.Strict (Map, empty, insertWith, toList)
import qualified Data.Text as T

verifyChecksum :: T.Text -> Bool
verifyChecksum input =
  let inputChecksum = extractChecksum input
      inputData = extractData input
      frequencies :: Map Char Int
      frequencies =
        T.foldl incrementCharacter empty inputData
      correctChecksum = T.unpack $ checksum frequencies
   in inputChecksum == T.pack correctChecksum

checksum :: Map Char Int -> T.Text
checksum frequencies =
  let listFreq = toList frequencies
      sortedFreq = sortBy (\(_, x) (_, y) -> compare x y) listFreq
      splitFreq = splitFrequencies sortedFreq 1 []
   in foldl T.append T.empty (T.pack . sort . T.unpack <$> splitFreq)

-- Assumes that the frequency list is sorted by frequencies
splitFrequencies :: [(Char, Int)] -> Int -> [T.Text] -> [T.Text]
splitFrequencies [] _ ret = ret
splitFrequencies ((nextChar, freq) : xs) currentFreq [] = splitFrequencies xs freq [T.singleton nextChar]
splitFrequencies ((nextChar, freq) : ys) currentFreq (currentSplit : xs)
  | freq == currentFreq =
      let newSplit = T.cons nextChar currentSplit
       in splitFrequencies ys freq $ [newSplit] ++ xs
  | otherwise = splitFrequencies ys freq $ [T.singleton nextChar] ++ [currentSplit] ++ xs

extractChecksum :: T.Text -> T.Text
extractChecksum = T.init . T.tail . T.dropWhile (/= '[')

extractData :: T.Text -> T.Text
extractData = T.takeWhile (/= '[') . T.filter (/= '-')

incrementCharacter ::
  Map Char Int ->
  Char ->
  Map Char Int
incrementCharacter frequencies character =
  insertWith (+) character 1 frequencies

biggerValue :: (Char, Int) -> (Char, Int) -> Ordering
biggerValue (x, a) (y, b) = compare a b
