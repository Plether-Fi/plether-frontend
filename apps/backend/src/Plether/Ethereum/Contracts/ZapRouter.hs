module Plether.Ethereum.Contracts.ZapRouter
  ( previewZapMint
  , previewZapBurn
  , previewZapMintCall
  , previewZapBurnCall
  , decodePreviewZapMint
  , decodePreviewZapBurn
  , PreviewZapMintResult (..)
  , PreviewZapBurnResult (..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Plether.Ethereum.Abi (decodeUint256, encodeCall, encodeUint256)
import Plether.Ethereum.Client (CallParams (..), EthClient, RpcError, ethCall)

data PreviewZapMintResult = PreviewZapMintResult
  { pzmFlashAmount :: Integer
  , pzmExpectedSwapOut :: Integer
  , pzmTotalUSDC :: Integer
  , pzmExpectedTokensOut :: Integer
  , pzmFlashFee :: Integer
  }
  deriving stock (Show)

data PreviewZapBurnResult = PreviewZapBurnResult
  { pzbExpectedUsdcFromBurn :: Integer
  , pzbUsdcForBearBuyback :: Integer
  , pzbExpectedUsdcOut :: Integer
  , pzbFlashFee :: Integer
  }
  deriving stock (Show)

previewZapMintCall :: Integer -> ByteString
previewZapMintCall amount = encodeCall "previewZapMint(uint256)" [encodeUint256 amount]

previewZapBurnCall :: Integer -> ByteString
previewZapBurnCall amount = encodeCall "previewZapBurn(uint256)" [encodeUint256 amount]

decodePreviewZapMint :: ByteString -> PreviewZapMintResult
decodePreviewZapMint bs =
  PreviewZapMintResult
    { pzmFlashAmount = decodeUint256 (BS.take 32 bs)
    , pzmExpectedSwapOut = decodeUint256 (BS.take 32 $ BS.drop 32 bs)
    , pzmTotalUSDC = decodeUint256 (BS.take 32 $ BS.drop 64 bs)
    , pzmExpectedTokensOut = decodeUint256 (BS.take 32 $ BS.drop 96 bs)
    , pzmFlashFee = decodeUint256 (BS.take 32 $ BS.drop 128 bs)
    }

decodePreviewZapBurn :: ByteString -> PreviewZapBurnResult
decodePreviewZapBurn bs =
  PreviewZapBurnResult
    { pzbExpectedUsdcFromBurn = decodeUint256 (BS.take 32 bs)
    , pzbUsdcForBearBuyback = decodeUint256 (BS.take 32 $ BS.drop 32 bs)
    , pzbExpectedUsdcOut = decodeUint256 (BS.take 32 $ BS.drop 64 bs)
    , pzbFlashFee = decodeUint256 (BS.take 32 $ BS.drop 96 bs)
    }

previewZapMint :: EthClient -> Text -> Integer -> IO (Either RpcError PreviewZapMintResult)
previewZapMint client router amount = do
  result <- ethCall client (CallParams router (previewZapMintCall amount))
  pure $ fmap decodePreviewZapMint result

previewZapBurn :: EthClient -> Text -> Integer -> IO (Either RpcError PreviewZapBurnResult)
previewZapBurn client router amount = do
  result <- ethCall client (CallParams router (previewZapBurnCall amount))
  pure $ fmap decodePreviewZapBurn result
