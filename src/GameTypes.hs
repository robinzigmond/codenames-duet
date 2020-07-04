{-# LANGUAGE DeriveGeneric #-}

module GameTypes
  ( Card
  , CardType
  , KeyCardSide
  , KeyCard(..)
  , lookupResult
  , makeKeyCard
  , rotateSide
  ) where

import           Data.Aeson   (ToJSON (..))
import           Data.Text    (Text)
import           GHC.Generics (Generic)

type Card = Text

data CardType
  = Agent
  | Assassin
  | Bystander
  deriving (Generic)

instance ToJSON CardType

data KeyCardSide =
  KeyCardSide [[CardType]]

instance ToJSON KeyCardSide where
  toJSON (KeyCardSide types) = toJSON types

data KeyCard =
  KeyCard
    { side1 :: KeyCardSide
    , side2 :: KeyCardSide
    }

-- "smart" constructor
makeKeyCard :: [(Int, Int)] -> [(Int, Int)] -> KeyCardSide
makeKeyCard agents assassins =
  KeyCardSide $
  flip map [1 .. 5] $ \row ->
    flip map [1 .. 5] $ \col ->
      if (row, col) `elem` agents
        then Agent
        else if (row, col) `elem` assassins
               then Assassin
               else Bystander

rotateSide :: KeyCardSide -> KeyCardSide
rotateSide (KeyCardSide types) = KeyCardSide . reverse $ map reverse types

lookupSide :: Int -> Int -> KeyCardSide -> CardType
lookupSide row col (KeyCardSide types) = (types !! (row - 1)) !! (col - 1)

lookupResult :: Int -> Int -> (KeyCard -> KeyCardSide) -> KeyCard -> CardType
lookupResult row col whichSide keyCard = lookupSide row col (whichSide keyCard)
