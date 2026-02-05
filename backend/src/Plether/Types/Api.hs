module Plether.Types.Api
  ( ApiResponse (..)
  , ApiMeta (..)
  , mkResponse
  , mkCachedResponse
  ) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

data ApiMeta = ApiMeta
  { metaCached :: Bool
  , metaCachedAt :: Maybe POSIXTime
  , metaStale :: Maybe Bool
  , metaBlockNumber :: Integer
  , metaChainId :: Integer
  }
  deriving stock (Show, Generic)

instance ToJSON ApiMeta where
  toJSON ApiMeta {..} =
    object $
      [ "cached" .= metaCached
      , "blockNumber" .= metaBlockNumber
      , "chainId" .= metaChainId
      ]
        ++ maybe [] (\t -> ["cachedAt" .= (round t :: Integer)]) metaCachedAt
        ++ maybe [] (\s -> ["stale" .= s]) metaStale

data ApiResponse a = ApiResponse
  { respData :: a
  , respMeta :: ApiMeta
  }
  deriving stock (Show, Generic)

instance (ToJSON a) => ToJSON (ApiResponse a) where
  toJSON ApiResponse {..} =
    object
      [ "data" .= respData
      , "meta" .= respMeta
      ]

mkResponse :: Integer -> Integer -> a -> ApiResponse a
mkResponse blockNum chainId dat =
  ApiResponse
    { respData = dat
    , respMeta =
        ApiMeta
          { metaCached = False
          , metaCachedAt = Nothing
          , metaStale = Nothing
          , metaBlockNumber = blockNum
          , metaChainId = chainId
          }
    }

mkCachedResponse :: Integer -> Integer -> POSIXTime -> Bool -> a -> ApiResponse a
mkCachedResponse blockNum chainId cachedAt stale dat =
  ApiResponse
    { respData = dat
    , respMeta =
        ApiMeta
          { metaCached = True
          , metaCachedAt = Just cachedAt
          , metaStale = Just stale
          , metaBlockNumber = blockNum
          , metaChainId = chainId
          }
    }
