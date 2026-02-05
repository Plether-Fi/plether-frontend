module Plether.Ethereum.Contracts.StakedToken
  ( totalAssets
  , totalSupply
  , convertToAssets
  , convertToShares
  , balanceOf
  , totalAssetsCall
  , totalSupplyCall
  , convertToAssetsCall
  , convertToSharesCall
  , balanceOfCall
  , decodeTotalAssets
  , decodeTotalSupply
  , decodeConvertToAssets
  , decodeConvertToShares
  , decodeBalanceOf
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Plether.Ethereum.Abi (decodeUint256, encodeAddress, encodeCall, encodeUint256)
import Plether.Ethereum.Client (CallParams (..), EthClient, RpcError, ethCall)

totalAssetsCall :: ByteString
totalAssetsCall = encodeCall "totalAssets()" []

totalSupplyCall :: ByteString
totalSupplyCall = encodeCall "totalSupply()" []

convertToAssetsCall :: Integer -> ByteString
convertToAssetsCall shares = encodeCall "convertToAssets(uint256)" [encodeUint256 shares]

convertToSharesCall :: Integer -> ByteString
convertToSharesCall assets = encodeCall "convertToShares(uint256)" [encodeUint256 assets]

balanceOfCall :: Text -> ByteString
balanceOfCall owner = encodeCall "balanceOf(address)" [encodeAddress owner]

decodeTotalAssets :: ByteString -> Integer
decodeTotalAssets = decodeUint256

decodeTotalSupply :: ByteString -> Integer
decodeTotalSupply = decodeUint256

decodeConvertToAssets :: ByteString -> Integer
decodeConvertToAssets = decodeUint256

decodeConvertToShares :: ByteString -> Integer
decodeConvertToShares = decodeUint256

decodeBalanceOf :: ByteString -> Integer
decodeBalanceOf = decodeUint256

totalAssets :: EthClient -> Text -> IO (Either RpcError Integer)
totalAssets client vault = do
  result <- ethCall client (CallParams vault totalAssetsCall)
  pure $ fmap decodeTotalAssets result

totalSupply :: EthClient -> Text -> IO (Either RpcError Integer)
totalSupply client vault = do
  result <- ethCall client (CallParams vault totalSupplyCall)
  pure $ fmap decodeTotalSupply result

convertToAssets :: EthClient -> Text -> Integer -> IO (Either RpcError Integer)
convertToAssets client vault shares = do
  result <- ethCall client (CallParams vault (convertToAssetsCall shares))
  pure $ fmap decodeConvertToAssets result

convertToShares :: EthClient -> Text -> Integer -> IO (Either RpcError Integer)
convertToShares client vault assets = do
  result <- ethCall client (CallParams vault (convertToSharesCall assets))
  pure $ fmap decodeConvertToShares result

balanceOf :: EthClient -> Text -> Text -> IO (Either RpcError Integer)
balanceOf client vault owner = do
  result <- ethCall client (CallParams vault (balanceOfCall owner))
  pure $ fmap decodeBalanceOf result
