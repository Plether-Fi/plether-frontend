module Plether.Indexer.Events
  ( ParsedEvent (..)
  , MorphoMarkets (..)
  , parseEventLog
  , EventLog (..)
  ) where

import Data.Aeson (Value, object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Plether.Indexer.Contracts

data EventLog = EventLog
  { elTxHash :: Text
  , elBlockNumber :: Integer
  , elAddress :: Text
  , elTopics :: [ByteString]
  , elData :: ByteString
  }
  deriving stock (Show)

data ParsedEvent = ParsedEvent
  { peUserAddress :: Text
  , peTxType :: Text
  , peSide :: Maybe Text
  , peData :: Value
  }
  deriving stock (Show)

data MorphoMarkets = MorphoMarkets
  { mmBearMarketIds :: [Text]
  , mmBullMarketIds :: [Text]
  }
  deriving stock (Show)

parseEventLog :: EventLog -> [Text] -> [Text] -> MorphoMarkets -> Maybe ParsedEvent
parseEventLog log bearContracts bullContracts morphoMarkets
  | null (elTopics log) = Nothing
  | otherwise = parseByTopic (head $ elTopics log) log bearContracts bullContracts morphoMarkets

parseByTopic :: ByteString -> EventLog -> [Text] -> [Text] -> MorphoMarkets -> Maybe ParsedEvent
parseByTopic topic log bearContracts bullContracts morphoMarkets
  | topic == esTopic mintEvent = parseMintBurnEvent log "mint" Nothing
  | topic == esTopic burnEvent = parseMintBurnEvent log "burn" Nothing
  | topic == esTopic tokenExchangeEvent = parseTokenExchangeEvent log
  | topic == esTopic zapMintEvent = parseZapMintEvent log
  | topic == esTopic zapBurnEvent = parseZapBurnEvent log
  | topic == esTopic stakingDepositEvent = parseStakingEvent log "stake" bearContracts bullContracts
  | topic == esTopic stakingWithdrawEvent = parseUnstakeEvent log bearContracts bullContracts
  | topic == esTopic leverageOpenedEvent = parseLeverageOpenEvent log bearContracts bullContracts
  | topic == esTopic leverageClosedEvent = parseLeverageCloseEvent log bearContracts bullContracts
  | topic == esTopic morphoSupplyEvent = parseMorphoSupplyRepayEvent log "lending_supply" morphoMarkets
  | topic == esTopic morphoWithdrawEvent = parseMorphoWithdrawBorrowEvent log "lending_withdraw" morphoMarkets
  | topic == esTopic morphoBorrowEvent = parseMorphoWithdrawBorrowEvent log "lending_borrow" morphoMarkets
  | topic == esTopic morphoRepayEvent = parseMorphoSupplyRepayEvent log "lending_repay" morphoMarkets
  | otherwise = Nothing

parseMintBurnEvent :: EventLog -> Text -> Maybe Text -> Maybe ParsedEvent
parseMintBurnEvent log txType side = do
  userAddr <- getIndexedAddress (elTopics log) 1
  let amount = decodeUint256Single (elData log)
  Just $ ParsedEvent
    { peUserAddress = userAddr
    , peTxType = txType
    , peSide = side
    , peData = object
        [ "amount" .= amount
        ]
    }

parseTokenExchangeEvent :: EventLog -> Maybe ParsedEvent
parseTokenExchangeEvent log = do
  buyer <- getIndexedAddress (elTopics log) 1
  let (soldId, tokensSold, boughtId, tokensBought, fee, packedPriceScale) = decodeUint256Sextuple (elData log)
  Just $ ParsedEvent
    { peUserAddress = buyer
    , peTxType = "swap"
    , peSide = Nothing
    , peData = object
        [ "soldId" .= soldId
        , "tokensSold" .= tokensSold
        , "boughtId" .= boughtId
        , "tokensBought" .= tokensBought
        , "fee" .= fee
        , "packedPriceScale" .= packedPriceScale
        ]
    }

parseZapMintEvent :: EventLog -> Maybe ParsedEvent
parseZapMintEvent log = do
  userAddr <- getIndexedAddress (elTopics log) 1
  let (usdcIn, tokensOut, maxSlippageBps, actualSwapOut) = decodeUint256Quad (elData log)
  Just $ ParsedEvent
    { peUserAddress = userAddr
    , peTxType = "zap_buy"
    , peSide = Just "bull"
    , peData = object
        [ "usdcIn" .= usdcIn
        , "tokensOut" .= tokensOut
        , "maxSlippageBps" .= maxSlippageBps
        , "actualSwapOut" .= actualSwapOut
        ]
    }

parseZapBurnEvent :: EventLog -> Maybe ParsedEvent
parseZapBurnEvent log = do
  userAddr <- getIndexedAddress (elTopics log) 1
  let (amountIn, amountOut) = decodeUint256Pair (elData log)
  Just $ ParsedEvent
    { peUserAddress = userAddr
    , peTxType = "zap_sell"
    , peSide = Just "bull"
    , peData = object
        [ "amountIn" .= amountIn
        , "amountOut" .= amountOut
        ]
    }

parseStakingEvent :: EventLog -> Text -> [Text] -> [Text] -> Maybe ParsedEvent
parseStakingEvent log txType bearContracts bullContracts = do
  ownerAddr <- getIndexedAddress (elTopics log) 2
  let (assets, shares) = decodeUint256Pair (elData log)
      side = determineSide (elAddress log) bearContracts bullContracts
  Just $ ParsedEvent
    { peUserAddress = ownerAddr
    , peTxType = txType
    , peSide = side
    , peData = object
        [ "assets" .= assets
        , "shares" .= shares
        ]
    }

parseUnstakeEvent :: EventLog -> [Text] -> [Text] -> Maybe ParsedEvent
parseUnstakeEvent log bearContracts bullContracts = do
  ownerAddr <- getIndexedAddress (elTopics log) 3
  let (assets, shares) = decodeUint256Pair (elData log)
      side = determineSide (elAddress log) bearContracts bullContracts
  Just $ ParsedEvent
    { peUserAddress = ownerAddr
    , peTxType = "unstake"
    , peSide = side
    , peData = object
        [ "assets" .= assets
        , "shares" .= shares
        ]
    }

parseLeverageOpenEvent :: EventLog -> [Text] -> [Text] -> Maybe ParsedEvent
parseLeverageOpenEvent log bearContracts bullContracts = do
  userAddr <- getIndexedAddress (elTopics log) 1
  let (principal, leverage, loanAmount, tokensReceived, debtIncurred, maxSlippageBps) = decodeUint256Sextuple (elData log)
      side = determineSide (elAddress log) bearContracts bullContracts
  Just $ ParsedEvent
    { peUserAddress = userAddr
    , peTxType = "leverage_open"
    , peSide = side
    , peData = object
        [ "principal" .= principal
        , "leverage" .= leverage
        , "loanAmount" .= loanAmount
        , "tokensReceived" .= tokensReceived
        , "debtIncurred" .= debtIncurred
        , "maxSlippageBps" .= maxSlippageBps
        ]
    }

parseLeverageCloseEvent :: EventLog -> [Text] -> [Text] -> Maybe ParsedEvent
parseLeverageCloseEvent log bearContracts bullContracts = do
  userAddr <- getIndexedAddress (elTopics log) 1
  let (debtRepaid, collateralWithdrawn, usdcReturned, maxSlippageBps) = decodeUint256Quad (elData log)
      side = determineSide (elAddress log) bearContracts bullContracts
  Just $ ParsedEvent
    { peUserAddress = userAddr
    , peTxType = "leverage_close"
    , peSide = side
    , peData = object
        [ "debtRepaid" .= debtRepaid
        , "collateralWithdrawn" .= collateralWithdrawn
        , "usdcReturned" .= usdcReturned
        , "maxSlippageBps" .= maxSlippageBps
        ]
    }

parseMorphoSupplyRepayEvent :: EventLog -> Text -> MorphoMarkets -> Maybe ParsedEvent
parseMorphoSupplyRepayEvent log txType morphoMarkets = do
  onBehalf <- getIndexedAddress (elTopics log) 3
  let (assets, shares) = decodeUint256Pair (elData log)
      side = determineSideByMarketId (elTopics log) morphoMarkets
  Just $ ParsedEvent
    { peUserAddress = onBehalf
    , peTxType = txType
    , peSide = side
    , peData = object
        [ "assets" .= assets
        , "shares" .= shares
        ]
    }

parseMorphoWithdrawBorrowEvent :: EventLog -> Text -> MorphoMarkets -> Maybe ParsedEvent
parseMorphoWithdrawBorrowEvent log txType morphoMarkets = do
  onBehalf <- getIndexedAddress (elTopics log) 3
  let (assets, shares) = decodeUint256PairSkip1 (elData log)
      side = determineSideByMarketId (elTopics log) morphoMarkets
  Just $ ParsedEvent
    { peUserAddress = onBehalf
    , peTxType = txType
    , peSide = side
    , peData = object
        [ "assets" .= assets
        , "shares" .= shares
        ]
    }

determineSideByMarketId :: [ByteString] -> MorphoMarkets -> Maybe Text
determineSideByMarketId topics morphoMarkets
  | length topics < 2 = Nothing
  | marketId `elem` map normalizeAddress (mmBearMarketIds morphoMarkets) = Just "bear"
  | marketId `elem` map normalizeAddress (mmBullMarketIds morphoMarkets) = Just "bull"
  | otherwise = Nothing
  where
    marketId = normalizeAddress $ "0x" <> bytesToHex (topics !! 1)

determineSide :: Text -> [Text] -> [Text] -> Maybe Text
determineSide addr bearContracts bullContracts
  | normalizeAddress addr `elem` map normalizeAddress bearContracts = Just "bear"
  | normalizeAddress addr `elem` map normalizeAddress bullContracts = Just "bull"
  | otherwise = Nothing

normalizeAddress :: Text -> Text
normalizeAddress = T.toLower

getIndexedAddress :: [ByteString] -> Int -> Maybe Text
getIndexedAddress topics idx
  | idx < length topics = Just $ "0x" <> T.drop 24 (bytesToHex (topics !! idx))
  | otherwise = Nothing

decodeUint256Single :: ByteString -> Integer
decodeUint256Single bs = bytesToInteger (BS.take 32 bs)

decodeUint256Pair :: ByteString -> (Integer, Integer)
decodeUint256Pair bs =
  ( bytesToInteger (BS.take 32 bs)
  , bytesToInteger (BS.take 32 (BS.drop 32 bs))
  )

decodeUint256PairSkip1 :: ByteString -> (Integer, Integer)
decodeUint256PairSkip1 bs =
  ( bytesToInteger (BS.take 32 (BS.drop 32 bs))
  , bytesToInteger (BS.take 32 (BS.drop 64 bs))
  )

decodeUint256Quad :: ByteString -> (Integer, Integer, Integer, Integer)
decodeUint256Quad bs =
  ( bytesToInteger (BS.take 32 bs)
  , bytesToInteger (BS.take 32 (BS.drop 32 bs))
  , bytesToInteger (BS.take 32 (BS.drop 64 bs))
  , bytesToInteger (BS.take 32 (BS.drop 96 bs))
  )

decodeUint256Sextuple :: ByteString -> (Integer, Integer, Integer, Integer, Integer, Integer)
decodeUint256Sextuple bs =
  ( bytesToInteger (BS.take 32 bs)
  , bytesToInteger (BS.take 32 (BS.drop 32 bs))
  , bytesToInteger (BS.take 32 (BS.drop 64 bs))
  , bytesToInteger (BS.take 32 (BS.drop 96 bs))
  , bytesToInteger (BS.take 32 (BS.drop 128 bs))
  , bytesToInteger (BS.take 32 (BS.drop 160 bs))
  )

bytesToInteger :: ByteString -> Integer
bytesToInteger = BS.foldl' (\acc byte -> acc * 256 + fromIntegral byte) 0

bytesToHex :: ByteString -> Text
bytesToHex = TE.decodeUtf8 . B16.encode
