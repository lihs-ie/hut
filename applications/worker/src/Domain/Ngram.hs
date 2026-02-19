module Domain.Ngram
  ( generateNgramsBySize,
  )
where

import Data.Char (isLetter, isNumber, isSpace, toLower)
import Data.Set qualified as Set

type Ngram = String

normalize :: String -> String
normalize = filter (\character -> isLetter character || isNumber character) . filter (not . isSpace) . map toLower

type NgramMin = Int

type NgramMax = Int

generateNgramsBySize :: NgramMin -> NgramMax -> String -> [[Ngram]]
generateNgramsBySize minLength maxLength text =
  let normalized = normalize text
   in [ Set.toList (Set.fromList [take n (drop i normalized) | i <- [0 .. length normalized - n]])
        | n <- [minLength .. maxLength]
      ]
