module Plether.Handlers.Quote
  ( getMintQuote
  , getBurnQuote
  , getZapQuote
  , getTradeQuote
  , getLeverageQuote
  ) where

import Data.Text (Text)
import Plether.Config (Addresses (..), Config (..))
import Plether.Ethereum.Client (EthClient, ethBlockNumber)
import qualified Plether.Ethereum.Contracts.BasketOracle as Oracle
import qualified Plether.Ethereum.Contracts.CurvePool as Curve
import qualified Plether.Ethereum.Contracts.LeverageRouter as Leverage
import qualified Plether.Ethereum.Contracts.SyntheticSplitter as Splitter
import qualified Plether.Ethereum.Contracts.ZapRouter as Zap
import Plether.Types
import Plether.Utils.Numeric (wad)

calculatePriceImpact :: Integer -> Integer -> Integer -> Integer -> Integer
calculatePriceImpact actualOutput actualInput refOutput refInput
  | actualInput == 0 || refInput == 0 || refOutput == 0 = 0
  | spotRate == 0 = 0
  | actualRate >= spotRate = 0
  | otherwise = (spotRate - actualRate) * 10000 `div` spotRate
  where
    spotRate = refOutput * wad `div` refInput
    actualRate = actualOutput * wad `div` actualInput

getMintQuote :: EthClient -> Config -> Integer -> IO (Either ApiError (ApiResponse MintQuote))
getMintQuote client cfg amount = do
  let addrs = cfgAddresses cfg

  eBlockNum <- ethBlockNumber client
  eCap <- Splitter.getCap client (addrSyntheticSplitter addrs)
  eOracle <- Oracle.latestRoundData client (addrBasketOracle addrs)

  case (eBlockNum, eCap, eOracle) of
    (Right blockNum, Right cap, Right oracle) -> do
      let oraclePrice = Oracle.rdAnswer oracle
          usdcRequired = (amount * (oraclePrice + (cap - oraclePrice))) `div` (10 ^ (12 :: Integer))
          pricePerToken = (usdcRequired * wad) `div` amount

          quote =
            MintQuote
              { mintUsdcIn = usdcRequired
              , mintBearOut = amount
              , mintBullOut = amount
              , mintPricePerToken = pricePerToken
              }

      pure $ Right $ mkResponse blockNum (cfgChainId cfg) quote
    (Left err, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, Left err, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, Left err) -> pure $ Left $ rpcErrorToApiError err

getBurnQuote :: EthClient -> Config -> Integer -> IO (Either ApiError (ApiResponse BurnQuote))
getBurnQuote client cfg amount = do
  let addrs = cfgAddresses cfg

  eBlockNum <- ethBlockNumber client
  ePreview <- Splitter.previewBurn client (addrSyntheticSplitter addrs) amount

  case (eBlockNum, ePreview) of
    (Right blockNum, Right preview) -> do
      let quote =
            BurnQuote
              { burnPairIn = amount
              , burnUsdcOut = Splitter.pbUsdcRefund preview
              , burnBearIn = amount
              , burnBullIn = amount
              }

      pure $ Right $ mkResponse blockNum (cfgChainId cfg) quote
    (Left err, _) -> pure $ Left $ rpcErrorToApiError err
    (_, Left err) -> pure $ Left $ rpcErrorToApiError err

getZapQuote :: EthClient -> Config -> Text -> Integer -> IO (Either ApiError (ApiResponse ZapQuote))
getZapQuote client cfg direction amount = do
  let addrs = cfgAddresses cfg
      router = addrZapRouter addrs

  eBlockNum <- ethBlockNumber client

  case direction of
    "buy" -> do
      let refAmount = 10 ^ (6 :: Integer)
      ePreview <- Zap.previewZapMint client router amount
      eRefPreview <- Zap.previewZapMint client router refAmount

      case (eBlockNum, ePreview, eRefPreview) of
        (Right blockNum, Right preview, Right refPreview) -> do
          let outAmount = Zap.pzmExpectedTokensOut preview
              refOut = Zap.pzmExpectedTokensOut refPreview
              priceImpact = calculatePriceImpact outAmount amount refOut refAmount
              quote =
                ZapQuote
                  { zapDirection = Buy
                  , zapInput = ZapInput { zapInToken = "usdc", zapInAmount = amount }
                  , zapOutput =
                      ZapOutput
                        { zapOutToken = "bull"
                        , zapOutAmount = outAmount
                        , zapOutMinAmount = (outAmount * 99) `div` 100
                        }
                  , zapPriceImpact = priceImpact
                  , zapRoute = ["USDC", "plDXY-BEAR (flash)", "Curve", "Splitter", "plDXY-BULL"]
                  }
          pure $ Right $ mkResponse blockNum (cfgChainId cfg) quote
        (Left err, _, _) -> pure $ Left $ rpcErrorToApiError err
        (_, Left err, _) -> pure $ Left $ rpcErrorToApiError err
        (_, _, Left err) -> pure $ Left $ rpcErrorToApiError err

    _ -> do
      let refAmount = 10 ^ (18 :: Integer)
      ePreview <- Zap.previewZapBurn client router amount
      eRefPreview <- Zap.previewZapBurn client router refAmount

      case (eBlockNum, ePreview, eRefPreview) of
        (Right blockNum, Right preview, Right refPreview) -> do
          let outAmount = Zap.pzbExpectedUsdcOut preview
              refOut = Zap.pzbExpectedUsdcOut refPreview
              priceImpact = calculatePriceImpact outAmount amount refOut refAmount
              quote =
                ZapQuote
                  { zapDirection = Sell
                  , zapInput = ZapInput { zapInToken = "bull", zapInAmount = amount }
                  , zapOutput =
                      ZapOutput
                        { zapOutToken = "usdc"
                        , zapOutAmount = outAmount
                        , zapOutMinAmount = (outAmount * 99) `div` 100
                        }
                  , zapPriceImpact = priceImpact
                  , zapRoute = ["plDXY-BULL", "Splitter", "Curve", "USDC"]
                  }
          pure $ Right $ mkResponse blockNum (cfgChainId cfg) quote
        (Left err, _, _) -> pure $ Left $ rpcErrorToApiError err
        (_, Left err, _) -> pure $ Left $ rpcErrorToApiError err
        (_, _, Left err) -> pure $ Left $ rpcErrorToApiError err

getTradeQuote :: EthClient -> Config -> Text -> Integer -> IO (Either ApiError (ApiResponse TradeQuote))
getTradeQuote client cfg from amount = do
  let addrs = cfgAddresses cfg
      pool = addrCurvePool addrs

  eBlockNum <- ethBlockNumber client

  let (i, j, fromToken, toToken, refAmount) = case from of
        "usdc" -> (0, 1, FromUsdc, FromBear, 10 ^ (6 :: Integer))
        _ -> (1, 0, FromBear, FromUsdc, 10 ^ (18 :: Integer))

  eDy <- Curve.getDy client pool i j amount
  eRefDy <- Curve.getDy client pool i j refAmount

  case (eBlockNum, eDy, eRefDy) of
    (Right blockNum, Right dy, Right refDy) -> do
      let minOut = (dy * 99) `div` 100
          spotPrice =
            if amount > 0
              then (dy * wad) `div` amount
              else 0
          priceImpact = calculatePriceImpact dy amount refDy refAmount
          fee = (amount * 4) `div` 10000

          quote =
            TradeQuote
              { tradeFrom = fromToken
              , tradeTo = toToken
              , tradeAmountIn = amount
              , tradeAmountOut = dy
              , tradeMinAmountOut = minOut
              , tradeSpotPrice = spotPrice
              , tradePriceImpact = priceImpact
              , tradeFee = fee
              }

      pure $ Right $ mkResponse blockNum (cfgChainId cfg) quote
    (Left err, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, Left err, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, Left err) -> pure $ Left $ rpcErrorToApiError err

getLeverageQuote :: EthClient -> Config -> Text -> Integer -> Integer -> IO (Either ApiError (ApiResponse LeverageQuote))
getLeverageQuote client cfg side principal leverage = do
  let addrs = cfgAddresses cfg

  eBlockNum <- ethBlockNumber client

  let router = case side of
        "bear" -> addrLeverageRouter addrs
        _ -> addrBullLeverageRouter addrs

      sideVal = case side of
        "bear" -> Bear
        _ -> Bull

  ePreview <- Leverage.previewOpenLeverage client router principal leverage

  case (eBlockNum, ePreview) of
    (Right blockNum, Right preview) -> do
      let positionSize = Leverage.polExpectedTokens preview
          debt = Leverage.polExpectedDebt preview
          healthFactor =
            if debt > 0
              then (positionSize * 86 * (10 ^ (16 :: Integer))) `div` debt
              else maxHealthFactor

          quote =
            LeverageQuote
              { levqSide = sideVal
              , levqPrincipal = principal
              , levqLeverage = leverage
              , levqPositionSize = positionSize
              , levqPositionSizeUsd = positionSize
              , levqDebt = debt
              , levqHealthFactor = healthFactor
              , levqLiquidationPrice = 0
              , levqPriceImpact = 0
              , levqBorrowRate = 0
              }

      pure $ Right $ mkResponse blockNum (cfgChainId cfg) quote
    (Left err, _) -> pure $ Left $ rpcErrorToApiError err
    (_, Left err) -> pure $ Left $ rpcErrorToApiError err
  where
    maxHealthFactor = 10 ^ (20 :: Integer)

