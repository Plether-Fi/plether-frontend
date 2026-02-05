module Plether.Ethereum.Contracts.LeverageRouter
  ( getActualDebt
  , previewOpenLeverage
  , getActualDebtCall
  , previewOpenLeverageCall
  , decodeActualDebt
  , decodePreviewOpenLeverage
  , PreviewOpenLeverageResult (..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Plether.Ethereum.Abi (decodeUint256, encodeAddress, encodeCall, encodeUint256)
import Plether.Ethereum.Client (CallParams (..), EthClient, RpcError, ethCall)

data PreviewOpenLeverageResult = PreviewOpenLeverageResult
  { polLoanAmount :: Integer
  , polTotalUSDC :: Integer
  , polExpectedTokens :: Integer
  , polExpectedDebt :: Integer
  }
  deriving stock (Show)

getActualDebtCall :: Text -> ByteString
getActualDebtCall user = encodeCall "getActualDebt(address)" [encodeAddress user]

previewOpenLeverageCall :: Integer -> Integer -> ByteString
previewOpenLeverageCall principal leverage =
  encodeCall "previewOpenLeverage(uint256,uint256)" [encodeUint256 principal, encodeUint256 leverage]

decodeActualDebt :: ByteString -> Integer
decodeActualDebt = decodeUint256

decodePreviewOpenLeverage :: ByteString -> PreviewOpenLeverageResult
decodePreviewOpenLeverage bs =
  PreviewOpenLeverageResult
    { polLoanAmount = decodeUint256 (BS.take 32 bs)
    , polTotalUSDC = decodeUint256 (BS.take 32 $ BS.drop 32 bs)
    , polExpectedTokens = decodeUint256 (BS.take 32 $ BS.drop 64 bs)
    , polExpectedDebt = decodeUint256 (BS.take 32 $ BS.drop 96 bs)
    }

getActualDebt :: EthClient -> Text -> Text -> IO (Either RpcError Integer)
getActualDebt client router user = do
  result <- ethCall client (CallParams router (getActualDebtCall user))
  pure $ fmap decodeActualDebt result

previewOpenLeverage :: EthClient -> Text -> Integer -> Integer -> IO (Either RpcError PreviewOpenLeverageResult)
previewOpenLeverage client router principal leverage = do
  result <- ethCall client (CallParams router (previewOpenLeverageCall principal leverage))
  pure $ fmap decodePreviewOpenLeverage result
