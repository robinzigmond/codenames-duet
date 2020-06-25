{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Data
  ( KeyCard(..)
  , KeyCardSide
  , allCards
  , allKeyCards
  ) where

import           Data.Aeson   (ToJSON (..))
import           Data.Text    (Text)
import           GHC.Generics (Generic)

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
  KeyCard { side1 :: KeyCardSide, side2 :: KeyCardSide }

-- "smart" constructor
keyCard :: [(Int, Int)] -> [(Int, Int)] -> KeyCardSide
keyCard agents assassins =
  KeyCardSide $
  flip map [1 .. 5] $ \row ->
    flip map [1 .. 5] $ \col ->
      if (row, col) `elem` agents
        then Agent
        else if (row, col) `elem` assassins
               then Assassin
               else Bystander

allKeyCards :: [KeyCard]
allKeyCards =
  [ KeyCard
      (keyCard
         [ (2, 1)
         , (2, 3)
         , (2, 5)
         , (3, 1)
         , (3, 4)
         , (3, 5)
         , (5, 1)
         , (5, 4)
         , (5, 5)
         ]
         [(1, 5), (3, 3), (5, 2)])
      (keyCard
         [ (1, 2)
         , (1, 4)
         , (2, 4)
         , (3, 1)
         , (3, 3)
         , (3, 5)
         , (4, 1)
         , (4, 4)
         , (5, 4)
         ]
         [(1, 3), (3, 4), (5, 2)])
  , KeyCard
      (keyCard
         [ (1, 1)
         , (1, 3)
         , (2, 1)
         , (3, 2)
         , (3, 4)
         , (4, 3)
         , (5, 1)
         , (5, 2)
         , (5, 4)
         ]
         [(2, 2), (3, 5), (5, 3)])
      (keyCard
         [ (1, 2)
         , (1, 3)
         , (1, 5)
         , (3, 1)
         , (3, 4)
         , (4, 4)
         , (4, 5)
         , (5, 1)
         , (5, 3)
         ]
         [(3, 5), (4, 2), (5, 4)])
  ]

allCards :: [Text]
allCards =
  [ "ACE"
  , "ALASKA"
  , "ANCHOR"
  , "ANT"
  , "ANTHEM"
  , "APRON"
  , "ARMOR"
  , "ARMY"
  , "ASH"
  , "ASTRONAUT"
  , "ATTIC"
  , "AVALANCHE"
  , "AXE"
  , "BABY"
  , "BACON"
  , "BALLOON"
  , "BANANA"
  , "BARBECUE"
  , "BASS"
  , "BATH"
  , "BATTLE"
  , "BATTLESHIP"
  , "BAY"
  , "BEAM"
  , "BEAN"
  , "BEARD"
  , "BEE"
  , "BEER"
  , "BENCH"
  , "BICYCLE"
  , "BIG BANG"
  , "BIG BEN"
  , "BIKINI"
  , "BISCUIT"
  , "BLACK HOLE"
  , "BLACKSMITH"
  , "BLADE"
  , "BLIND"
  , "BLIZZARD"
  , "BLUES"
  , "BOIL"
  , "BONSAI"
  , "BOOK"
  , "BOSS"
  , "BOWL"
  , "BOWLER"
  , "BOXER"
  , "BRAIN"
  , "BRASS"
  , "BRAZIL"
  , "BREAD"
  , "BREAK"
  , "BRICK"
  , "BRIDE"
  , "BROTHER"
  , "BUBBLE"
  , "BUCKET"
  , "BULB"
  , "BUNK"
  , "BUTTER"
  , "BUTTERFLY"
  , "CABLE"
  , "CAESAR"
  , "CAKE"
  , "CAMP"
  , "CANE"
  , "CAPTAIN"
  , "CASTLE"
  , "CAVE"
  , "CHAIN"
  , "CHALK"
  , "CHEESE"
  , "CHERRY"
  , "CHIP"
  , "CHRISTMAS"
  , "CLEOPATRA"
  , "CLOCK"
  , "CLOUD"
  , "COACH"
  , "COAST"
  , "COFFEE"
  , "COLLAR"
  , "COLUMBUS"
  , "COMB"
  , "COMET"
  , "COMPUTER"
  , "CONE"
  , "COUNTRY"
  , "COW"
  , "COWBOY"
  , "CRAB"
  , "CRAFT"
  , "CROW"
  , "CRUSADER"
  , "CRYSTAL"
  , "CUCKOO"
  , "CURRY"
  , "DASH"
  , "DELTA"
  , "DENTIST"
  , "DESK"
  , "DIRECTOR"
  , "DISK"
  , "DOLL"
  , "DOLLAR"
  , "DOOR"
  , "DRAWING"
  , "DREAM"
  , "DRESSING"
  , "DRIVER"
  , "DRONE"
  , "DRUM"
  , "DRYER"
  , "DUST"
  , "EAR"
  , "EARTH"
  , "EARTHQUAKE"
  , "EASTER"
  , "EDEN"
  , "EGG"
  , "EINSTEIN"
  , "ELEPHANT"
  , "FARM"
  , "FEVER"
  , "FIDDLE"
  , "FLAG"
  , "FLAT"
  , "FLOOD"
  , "FLOOR"
  , "FOAM"
  , "FOG"
  , "FROG"
  , "FROST"
  , "FUEL"
  , "GANGSTER"
  , "GARDEN"
  , "GEAR"
  , "GENIE"
  , "GLACIER"
  , "GLASSES"
  , "GOAT"
  , "GOLDILOCKS"
  , "GOLF"
  , "GOVERNOR"
  , "GREENHOUSE"
  , "GROOM"
  , "GUITAR"
  , "GUM"
  , "GYMNAST"
  , "HAIR"
  , "HALLOWEEN"
  , "HAMBURGER"
  , "HAMMER"
  , "HAWAII"
  , "HELMET"
  , "HERCULES"
  , "HIDE"
  , "HIT"
  , "HOMER"
  , "HOSE"
  , "HOUSE"
  , "ICE AGE"
  , "ICELAND"
  , "IGLOO"
  , "INK"
  , "JAIL"
  , "JELLYFISH"
  , "JEWELLER"
  , "JOCKEY"
  , "JOKER"
  , "JOAN OF ARC"
  , "JUDGE"
  , "JUMPER"
  , "KICK"
  , "KILT"
  , "KING ARTHUR"
  , "KISS"
  , "KITCHEN"
  , "KNOT"
  , "KUNG FU"
  , "LACE"
  , "LADDER"
  , "LAUNDRY"
  , "LEAF"
  , "LEATHER"
  , "LEMONADE"
  , "LETTER"
  , "LIGHTNING"
  , "LIP"
  , "LOCUST"
  , "LOVE"
  , "LUMBERJACK"
  , "LUNCH"
  , "MAGAZINE"
  , "MAGICIAN"
  , "MAKEUP"
  , "MANICURE"
  , "MAP"
  , "MARACAS"
  , "MARATHON"
  , "MARK"
  , "MEDIC"
  , "MEMORY"
  , "MESS"
  , "METER"
  , "MICROWAVE"
  , "MILE"
  , "MILK"
  , "MILL"
  , "MINOTAUR"
  , "MINUTE"
  , "MIRROR"
  , "MISS"
  , "MOHAWK"
  , "MONA LISA"
  , "MONKEY"
  , "MOSES"
  , "MOSQUITO"
  , "MOTHER"
  , "MOUNTIE"
  , "MUD"
  , "MUMMY"
  , "MUSKETEER"
  , "MUSTARD"
  , "NAPOLEON"
  , "NERVE"
  , "NEWTON"
  , "NOAH"
  , "NOSE"
  , "NOTRE DAME"
  , "NYLON"
  , "OASIS"
  , "ONION"
  , "PACIFIC"
  , "PAD"
  , "PADDLE"
  , "PAGE"
  , "PAINT"
  , "PARADE"
  , "PARROT"
  , "PATIENT"
  , "PEA"
  , "PEACH"
  , "PEANUT"
  , "PEARL"
  , "PEN"
  , "PENNY"
  , "PENTAGON"
  , "PEPPER"
  , "PEW"
  , "PIG"
  , "PILLOW"
  , "PINE"
  , "PITCHER"
  , "PIZZA"
  , "POCKET"
  , "POLISH"
  , "POLO"
  , "POP"
  , "POPCORN"
  , "POTATO"
  , "POTTER"
  , "POWDER"
  , "PUPPET"
  , "PURSE"
  , "QUACK"
  , "QUARTER"
  , "RADIO"
  , "RAIL"
  , "RAINBOW"
  , "RAM"
  , "RANCH"
  , "RAT"
  , "RAZOR"
  , "RECORD"
  , "REINDEER"
  , "RICE"
  , "RIFLE"
  , "RIP"
  , "RIVER"
  , "ROAD"
  , "RODEO"
  , "ROLL"
  , "ROPE"
  , "RUBBER"
  , "RUSSIA"
  , "RUST"
  , "SACK"
  , "SADDLE"
  , "SAHARA"
  , "SAIL"
  , "SALAD"
  , "SALOON"
  , "SALSA"
  , "SALT"
  , "SAND"
  , "SANTA"
  , "SAW"
  , "SCARECROW"
  , "SCRATCH"
  , "SCROLL"
  , "SECOND"
  , "SHAMPOO"
  , "SHED"
  , "SHEET"
  , "SHELL"
  , "SHERLOCK"
  , "SHERWOOD"
  , "SHOOT"
  , "SHORTS"
  , "SHOULDER"
  , "SHOWER"
  , "SIGN"
  , "SILK"
  , "SISTER"
  , "SKATES"
  , "SKI"
  , "SKULL"
  , "SLED"
  , "SLEEP"
  , "SLING"
  , "SLIPPER"
  , "SLOTH"
  , "SMELL"
  , "SMOKE"
  , "SMOOTHIE"
  , "SNAKE"
  , "SNAP"
  , "SOAP"
  , "SOUP"
  , "SPHINX"
  , "SPIRIT"
  , "SPOON"
  , "SPRAY"
  , "SPURS"
  , "SQUASH"
  , "SQUIRREL"
  , "ST. PATRICK"
  , "STABLE"
  , "STAMP"
  , "STEAM"
  , "STEEL"
  , "STEP"
  , "STETHOSCOPE"
  , "STICKER"
  , "STORM"
  , "STORY"
  , "STREET"
  , "SUGAR"
  , "SUMO"
  , "SUN"
  , "SWAMP"
  , "SWEAT"
  , "SWORD"
  , "TANK"
  , "TASTE"
  , "TATTOO"
  , "TEA"
  , "TEAM"
  , "TEAR"
  , "TEXAS"
  , "THUNDER"
  , "TIGER"
  , "TIN"
  , "TIP"
  , "TIPI"
  , "TOAST"
  , "TORNADO"
  , "TRICK"
  , "TROLL"
  , "TUNNEL"
  , "TURTLE"
  , "TUTU"
  , "TUXEDO"
  , "UNIVERSITY"
  , "VALENTINE"
  , "VAMPIRE"
  , "VENUS"
  , "VIKING"
  , "VIOLET"
  , "VIRUS"
  , "VOLCANO"
  , "VOLUME"
  , "WAGON"
  , "WAITRESS"
  , "WALRUS"
  , "WEDDING"
  , "WEREWOLF"
  , "WHEEL"
  , "WHEELCHAIR"
  , "WHISTLE"
  , "WINDOW"
  , "WING"
  , "WISH"
  , "WIZARD"
  , "WONDERLAND"
  , "WOOD"
  , "WOOL"
  , "YELLOWSTONE"
  , "ZOMBIE"
  ]