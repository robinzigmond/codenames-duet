{-# LANGUAGE OverloadedStrings #-}

module GamePlay
  ( Card
  , randomCardsIO
  , takeUniques
  ) where

import           Data.List.Split (chunksOf)
import           Data.Text       (Text)
import           System.Random   (RandomGen, getStdGen, randomRs)

type Card = Text

type Grid = [[Card]]

allCards :: [Text]
-- get real cards later, just random "words" for now
allCards =
  [ "aaa"
  , "bbb"
  , "ccc"
  , "ddd"
  , "eee"
  , "fff"
  , "ggg"
  , "hhh"
  , "iii"
  , "jjj"
  , "kkk"
  , "lll"
  , "mmm"
  , "nnn"
  , "ooo"
  , "ppp"
  , "qqq"
  , "rrr"
  , "sss"
  , "ttt"
  , "uuu"
  , "vvv"
  , "www"
  , "xxx"
  , "yyy"
  , "zzz"
  ]

randomCards :: RandomGen g => g -> [[Card]]
randomCards gen =
  let numCards = length allCards
      allRandoms = randomRs (0, numCards - 1) gen
   in chunksOf 5 . map (allCards !!) $ takeUniques 25 allRandoms

-- takes numbers from the start of the (presumed infinite) list until there
-- are n unique ones
takeUniques :: (Eq a) => Int -> [a] -> [a]
takeUniques n source = go [] source
  where
    go acc (a:as)
      | length acc >= n = acc
      | a `elem` acc = go acc as
      | otherwise = go (a : acc) as

randomCardsIO :: IO [[Card]]
randomCardsIO = getStdGen >>= return . randomCards
