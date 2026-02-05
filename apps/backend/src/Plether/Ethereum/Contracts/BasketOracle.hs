module Plether.Ethereum.Contracts.BasketOracle
  ( latestRoundData
  , decimals
  , latestRoundDataCall
  , decimalsCall
  , decodeLatestRoundData
  , decodeDecimals
  , RoundData (..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Plether.Ethereum.Abi (decodeInt256, decodeUint256, encodeCall)
import Plether.Ethereum.Client (CallParams (..), EthClient, RpcError, ethCall)

data RoundData = RoundData
  { rdRoundId :: Integer
  , rdAnswer :: Integer
  , rdStartedAt :: Integer
  , rdUpdatedAt :: Integer
  , rdAnsweredInRound :: Integer
  }
  deriving stock (Show)

latestRoundDataCall :: ByteString
latestRoundDataCall = encodeCall "latestRoundData()" []

decimalsCall :: ByteString
decimalsCall = encodeCall "decimals()" []

decodeLatestRoundData :: ByteString -> RoundData
decodeLatestRoundData bs =
  RoundData
    { rdRoundId = decodeUint256 (BS.take 32 bs)
    , rdAnswer = decodeInt256 (BS.take 32 $ BS.drop 32 bs)
    , rdStartedAt = decodeUint256 (BS.take 32 $ BS.drop 64 bs)
    , rdUpdatedAt = decodeUint256 (BS.take 32 $ BS.drop 96 bs)
    , rdAnsweredInRound = decodeUint256 (BS.take 32 $ BS.drop 128 bs)
    }

decodeDecimals :: ByteString -> Int
decodeDecimals bs = fromIntegral $ decodeUint256 bs

latestRoundData :: EthClient -> Text -> IO (Either RpcError RoundData)
latestRoundData client oracle = do
  result <- ethCall client (CallParams oracle latestRoundDataCall)
  pure $ fmap decodeLatestRoundData result

decimals :: EthClient -> Text -> IO (Either RpcError Int)
decimals client oracle = do
  result <- ethCall client (CallParams oracle decimalsCall)
  pure $ fmap decodeDecimals result
