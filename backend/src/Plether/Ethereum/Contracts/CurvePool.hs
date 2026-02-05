module Plether.Ethereum.Contracts.CurvePool
  ( getDy
  , getBalances
  , getDyCall
  , getBalancesCall
  , decodeDy
  , decodeBalances
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Plether.Ethereum.Abi (decodeUint256, encodeCall, encodeUint256)
import Plether.Ethereum.Client (CallParams (..), EthClient, RpcError, ethCall)

getDyCall :: Integer -> Integer -> Integer -> ByteString
getDyCall i j dx =
  encodeCall "get_dy(uint256,uint256,uint256)" [encodeUint256 i, encodeUint256 j, encodeUint256 dx]

getBalancesCall :: Integer -> ByteString
getBalancesCall i = encodeCall "balances(uint256)" [encodeUint256 i]

decodeDy :: ByteString -> Integer
decodeDy = decodeUint256

decodeBalances :: ByteString -> Integer
decodeBalances = decodeUint256

getDy :: EthClient -> Text -> Integer -> Integer -> Integer -> IO (Either RpcError Integer)
getDy client pool i j dx = do
  result <- ethCall client (CallParams pool (getDyCall i j dx))
  pure $ fmap decodeDy result

getBalances :: EthClient -> Text -> Integer -> IO (Either RpcError Integer)
getBalances client pool i = do
  result <- ethCall client (CallParams pool (getBalancesCall i))
  pure $ fmap decodeBalances result
