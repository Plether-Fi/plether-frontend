module Plether.Ethereum.Contracts.ERC20
  ( balanceOf
  , allowance
  , totalSupply
  , balanceOfCall
  , allowanceCall
  , totalSupplyCall
  , decodeBalanceOf
  , decodeAllowance
  , decodeTotalSupply
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Plether.Ethereum.Abi (decodeUint256, encodeAddress, encodeCall)
import Plether.Ethereum.Client (CallParams (..), EthClient, RpcError, ethCall)

balanceOfCall :: Text -> ByteString
balanceOfCall owner = encodeCall "balanceOf(address)" [encodeAddress owner]

allowanceCall :: Text -> Text -> ByteString
allowanceCall owner spender =
  encodeCall "allowance(address,address)" [encodeAddress owner, encodeAddress spender]

totalSupplyCall :: ByteString
totalSupplyCall = encodeCall "totalSupply()" []

decodeBalanceOf :: ByteString -> Integer
decodeBalanceOf = decodeUint256

decodeAllowance :: ByteString -> Integer
decodeAllowance = decodeUint256

decodeTotalSupply :: ByteString -> Integer
decodeTotalSupply = decodeUint256

balanceOf :: EthClient -> Text -> Text -> IO (Either RpcError Integer)
balanceOf client token owner = do
  result <- ethCall client (CallParams token (balanceOfCall owner))
  pure $ fmap decodeBalanceOf result

allowance :: EthClient -> Text -> Text -> Text -> IO (Either RpcError Integer)
allowance client token owner spender = do
  result <- ethCall client (CallParams token (allowanceCall owner spender))
  pure $ fmap decodeAllowance result

totalSupply :: EthClient -> Text -> IO (Either RpcError Integer)
totalSupply client token = do
  result <- ethCall client (CallParams token totalSupplyCall)
  pure $ fmap decodeTotalSupply result
