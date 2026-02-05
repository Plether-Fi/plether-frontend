module Plether.Types.User
  ( UserDashboard (..)
  , UserBalances (..)
  , LeveragePosition (..)
  , LendingPosition (..)
  , UserAllowances (..)
  , UsdcAllowances (..)
  , BearAllowances (..)
  , BullAllowances (..)
  , UserPositions (..)
  , LeveragePositions (..)
  , LendingPositions (..)
  ) where

import Data.Aeson (ToJSON (..), object, (.=))
import GHC.Generics (Generic)

data UserBalances = UserBalances
  { balUsdc :: Integer
  , balBear :: Integer
  , balBull :: Integer
  , balStakedBear :: Integer
  , balStakedBull :: Integer
  , balStakedBearAssets :: Integer
  , balStakedBullAssets :: Integer
  }
  deriving stock (Show, Generic)

instance ToJSON UserBalances where
  toJSON UserBalances {..} =
    object
      [ "usdc" .= balUsdc
      , "bear" .= balBear
      , "bull" .= balBull
      , "stakedBear" .= balStakedBear
      , "stakedBull" .= balStakedBull
      , "stakedBearAssets" .= balStakedBearAssets
      , "stakedBullAssets" .= balStakedBullAssets
      ]

data LeveragePosition = LeveragePosition
  { levCollateral :: Integer
  , levCollateralUsd :: Integer
  , levDebt :: Integer
  , levHealthFactor :: Integer
  , levLiquidationPrice :: Integer
  , levLeverage :: Integer
  , levNetValue :: Integer
  }
  deriving stock (Show, Generic)

instance ToJSON LeveragePosition where
  toJSON LeveragePosition {..} =
    object
      [ "collateral" .= levCollateral
      , "collateralUsd" .= levCollateralUsd
      , "debt" .= levDebt
      , "healthFactor" .= levHealthFactor
      , "liquidationPrice" .= levLiquidationPrice
      , "leverage" .= levLeverage
      , "netValue" .= levNetValue
      ]

data LendingPosition = LendingPosition
  { lendSupplied :: Integer
  , lendSuppliedShares :: Integer
  , lendBorrowed :: Integer
  , lendBorrowedShares :: Integer
  , lendAvailableToBorrow :: Integer
  , lendCollateral :: Integer
  , lendHealthFactor :: Integer
  }
  deriving stock (Show, Generic)

instance ToJSON LendingPosition where
  toJSON LendingPosition {..} =
    object
      [ "supplied" .= lendSupplied
      , "suppliedShares" .= lendSuppliedShares
      , "borrowed" .= lendBorrowed
      , "borrowedShares" .= lendBorrowedShares
      , "availableToBorrow" .= lendAvailableToBorrow
      , "collateral" .= lendCollateral
      , "healthFactor" .= lendHealthFactor
      ]

data LeveragePositions = LeveragePositions
  { levPosBear :: Maybe LeveragePosition
  , levPosBull :: Maybe LeveragePosition
  }
  deriving stock (Show, Generic)

instance ToJSON LeveragePositions where
  toJSON LeveragePositions {..} =
    object
      [ "bear" .= levPosBear
      , "bull" .= levPosBull
      ]

data LendingPositions = LendingPositions
  { lendPosBear :: Maybe LendingPosition
  , lendPosBull :: Maybe LendingPosition
  }
  deriving stock (Show, Generic)

instance ToJSON LendingPositions where
  toJSON LendingPositions {..} =
    object
      [ "bear" .= lendPosBear
      , "bull" .= lendPosBull
      ]

data UserDashboard = UserDashboard
  { dashBalances :: UserBalances
  , dashLeverage :: LeveragePositions
  , dashLending :: LendingPositions
  }
  deriving stock (Show, Generic)

instance ToJSON UserDashboard where
  toJSON UserDashboard {..} =
    object
      [ "balances" .= dashBalances
      , "leverage" .= dashLeverage
      , "lending" .= dashLending
      ]

data UsdcAllowances = UsdcAllowances
  { usdcAllowSplitter :: Integer
  , usdcAllowZap :: Integer
  , usdcAllowMorphoBear :: Integer
  , usdcAllowMorphoBull :: Integer
  }
  deriving stock (Show, Generic)

instance ToJSON UsdcAllowances where
  toJSON UsdcAllowances {..} =
    object
      [ "splitter" .= usdcAllowSplitter
      , "zap" .= usdcAllowZap
      , "morphoBear" .= usdcAllowMorphoBear
      , "morphoBull" .= usdcAllowMorphoBull
      ]

data BearAllowances = BearAllowances
  { bearAllowSplitter :: Integer
  , bearAllowStaking :: Integer
  , bearAllowLeverageRouter :: Integer
  , bearAllowCurvePool :: Integer
  }
  deriving stock (Show, Generic)

instance ToJSON BearAllowances where
  toJSON BearAllowances {..} =
    object
      [ "splitter" .= bearAllowSplitter
      , "staking" .= bearAllowStaking
      , "leverageRouter" .= bearAllowLeverageRouter
      , "curvePool" .= bearAllowCurvePool
      ]

data BullAllowances = BullAllowances
  { bullAllowSplitter :: Integer
  , bullAllowStaking :: Integer
  , bullAllowLeverageRouter :: Integer
  }
  deriving stock (Show, Generic)

instance ToJSON BullAllowances where
  toJSON BullAllowances {..} =
    object
      [ "splitter" .= bullAllowSplitter
      , "staking" .= bullAllowStaking
      , "leverageRouter" .= bullAllowLeverageRouter
      ]

data UserAllowances = UserAllowances
  { allowUsdc :: UsdcAllowances
  , allowBear :: BearAllowances
  , allowBull :: BullAllowances
  }
  deriving stock (Show, Generic)

instance ToJSON UserAllowances where
  toJSON UserAllowances {..} =
    object
      [ "usdc" .= allowUsdc
      , "bear" .= allowBear
      , "bull" .= allowBull
      ]

data UserPositions = UserPositions
  { posLeverage :: LeveragePositions
  , posLending :: LendingPositions
  }
  deriving stock (Show, Generic)

instance ToJSON UserPositions where
  toJSON UserPositions {..} =
    object
      [ "leverage" .= posLeverage
      , "lending" .= posLending
      ]
