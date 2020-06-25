module GamePlay
  ( Card
  , KeyCard(..)
  , KeyCardSide
  , randomCardsIO
  , randomKeyCardIO
  , takeUniques
  ) where

import           Data.List.Split (chunksOf)
import           Data.Text       (Text)
import           System.Random   (RandomGen, getStdGen, randomIO, randomR,
                                  randomRs)

import           Data            (KeyCard(..), KeyCardSide, allCards, allKeyCards)

type Card = Text

type Grid = [[Card]]

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

randomKeycard :: RandomGen g => g -> KeyCard
randomKeycard gen =
  let numCards = length allKeyCards
      (rand, _) = randomR (0, numCards - 1) gen
   in allKeyCards !! rand

randomKeyCardIO :: IO KeyCard
randomKeyCardIO = getStdGen >>= return . randomKeycard
