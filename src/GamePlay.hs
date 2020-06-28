module GamePlay
  ( randomCardsIO
  , randomKeyCardIO
  , takeUniques
  ) where

import           Data.Function   (on)
import           Data.List.Split (chunksOf)
import           Data.Text       (Text)
import           System.Random   (RandomGen, getStdGen, randomIO, randomR,
                                  randomRs)

import           Data            (allCards, allKeyCards)
import           GameTypes

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
-- not only choose a random keycard, but rotate it at random
randomKeyCardIO = do
  g <- getStdGen
  rotateOrNot <- randomIO
  let rotateFunc =
        if rotateOrNot
          then rotateCard
          else id
  return . rotateFunc $ randomKeycard g

rotateCard :: KeyCard -> KeyCard
rotateCard (KeyCard side1 side2) = (KeyCard `on` rotateSide) side1 side2
