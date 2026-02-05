module Plether.Ethereum.Contracts.SyntheticSplitter
  ( currentStatus
  , getCap
  , previewMint
  , previewBurn
  , currentStatusCall
  , capCall
  , previewMintCall
  , previewBurnCall
  , decodeCurrentStatus
  , decodeCap
  , decodePreviewMint
  , decodePreviewBurn
  , ProtocolStatusRaw (..)
  , PreviewMintResult (..)
  , PreviewBurnResult (..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Plether.Ethereum.Abi (decodeUint256, encodeCall, encodeUint256)
import Plether.Ethereum.Client (CallParams (..), EthClient, RpcError, ethCall)

data ProtocolStatusRaw = ProtocolStatusRaw
  { statusValue :: Integer
  }
  deriving stock (Show)

data PreviewMintResult = PreviewMintResult
  { pmUsdcRequired :: Integer
  , pmDepositToAdapter :: Integer
  , pmKeptInBuffer :: Integer
  }
  deriving stock (Show)

data PreviewBurnResult = PreviewBurnResult
  { pbUsdcRefund :: Integer
  , pbWithdrawnFromAdapter :: Integer
  }
  deriving stock (Show)

currentStatusCall :: ByteString
currentStatusCall = encodeCall "currentStatus()" []

capCall :: ByteString
capCall = encodeCall "CAP()" []

previewMintCall :: Integer -> ByteString
previewMintCall amount = encodeCall "previewMint(uint256)" [encodeUint256 amount]

previewBurnCall :: Integer -> ByteString
previewBurnCall amount = encodeCall "previewBurn(uint256)" [encodeUint256 amount]

decodeCurrentStatus :: ByteString -> ProtocolStatusRaw
decodeCurrentStatus bs = ProtocolStatusRaw $ decodeUint256 bs

decodeCap :: ByteString -> Integer
decodeCap = decodeUint256

decodePreviewMint :: ByteString -> PreviewMintResult
decodePreviewMint bs =
  PreviewMintResult
    { pmUsdcRequired = decodeUint256 (BS.take 32 bs)
    , pmDepositToAdapter = decodeUint256 (BS.take 32 $ BS.drop 32 bs)
    , pmKeptInBuffer = decodeUint256 (BS.take 32 $ BS.drop 64 bs)
    }

decodePreviewBurn :: ByteString -> PreviewBurnResult
decodePreviewBurn bs =
  PreviewBurnResult
    { pbUsdcRefund = decodeUint256 (BS.take 32 bs)
    , pbWithdrawnFromAdapter = decodeUint256 (BS.take 32 $ BS.drop 32 bs)
    }

currentStatus :: EthClient -> Text -> IO (Either RpcError ProtocolStatusRaw)
currentStatus client splitter = do
  result <- ethCall client (CallParams splitter currentStatusCall)
  pure $ fmap decodeCurrentStatus result

getCap :: EthClient -> Text -> IO (Either RpcError Integer)
getCap client splitter = do
  result <- ethCall client (CallParams splitter capCall)
  pure $ fmap decodeCap result

previewMint :: EthClient -> Text -> Integer -> IO (Either RpcError PreviewMintResult)
previewMint client splitter amount = do
  result <- ethCall client (CallParams splitter (previewMintCall amount))
  pure $ fmap decodePreviewMint result

previewBurn :: EthClient -> Text -> Integer -> IO (Either RpcError PreviewBurnResult)
previewBurn client splitter amount = do
  result <- ethCall client (CallParams splitter (previewBurnCall amount))
  pure $ fmap decodePreviewBurn result
