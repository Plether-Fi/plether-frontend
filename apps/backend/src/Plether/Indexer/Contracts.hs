module Plether.Indexer.Contracts
  ( EventSignature (..)
  , allEventSignatures
  , mintEvent
  , burnEvent
  , tokenExchangeEvent
  , zapMintEvent
  , zapBurnEvent
  , stakingDepositEvent
  , stakingWithdrawEvent
  , leverageOpenedEvent
  , leverageClosedEvent
  , morphoSupplyEvent
  , morphoWithdrawEvent
  , morphoBorrowEvent
  , morphoRepayEvent
  , keccak256Text
  ) where

import Crypto.Hash (Digest, Keccak_256, hash)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

data EventSignature = EventSignature
  { esName :: Text
  , esTopic :: ByteString
  , esTxType :: Text
  , esSide :: Maybe Text
  }
  deriving stock (Show)

keccak256 :: ByteString -> ByteString
keccak256 input = convert (hash input :: Digest Keccak_256)

keccak256Text :: Text -> ByteString
keccak256Text = keccak256 . TE.encodeUtf8

eventTopic :: Text -> ByteString
eventTopic = keccak256Text

mintEvent :: EventSignature
mintEvent = EventSignature
  { esName = "Minted"
  , esTopic = eventTopic "Minted(address,uint256)"
  , esTxType = "mint"
  , esSide = Nothing
  }

burnEvent :: EventSignature
burnEvent = EventSignature
  { esName = "Burned"
  , esTopic = eventTopic "Burned(address,uint256)"
  , esTxType = "burn"
  , esSide = Nothing
  }

tokenExchangeEvent :: EventSignature
tokenExchangeEvent = EventSignature
  { esName = "TokenExchange"
  , esTopic = eventTopic "TokenExchange(address,uint256,uint256,uint256,uint256,uint256,uint256)"
  , esTxType = "swap"
  , esSide = Nothing
  }

zapMintEvent :: EventSignature
zapMintEvent = EventSignature
  { esName = "ZapMint"
  , esTopic = eventTopic "ZapMint(address,uint256,uint256,uint256,uint256)"
  , esTxType = "zap_buy"
  , esSide = Just "bull"
  }

zapBurnEvent :: EventSignature
zapBurnEvent = EventSignature
  { esName = "ZapBurn"
  , esTopic = eventTopic "ZapBurn(address,uint256,uint256)"
  , esTxType = "zap_sell"
  , esSide = Just "bull"
  }

stakingDepositEvent :: EventSignature
stakingDepositEvent = EventSignature
  { esName = "Deposit"
  , esTopic = eventTopic "Deposit(address,address,uint256,uint256)"
  , esTxType = "stake"
  , esSide = Nothing
  }

stakingWithdrawEvent :: EventSignature
stakingWithdrawEvent = EventSignature
  { esName = "Withdraw"
  , esTopic = eventTopic "Withdraw(address,address,address,uint256,uint256)"
  , esTxType = "unstake"
  , esSide = Nothing
  }

leverageOpenedEvent :: EventSignature
leverageOpenedEvent = EventSignature
  { esName = "LeverageOpened"
  , esTopic = eventTopic "LeverageOpened(address,uint256,uint256,uint256,uint256,uint256,uint256)"
  , esTxType = "leverage_open"
  , esSide = Nothing
  }

leverageClosedEvent :: EventSignature
leverageClosedEvent = EventSignature
  { esName = "LeverageClosed"
  , esTopic = eventTopic "LeverageClosed(address,uint256,uint256,uint256,uint256)"
  , esTxType = "leverage_close"
  , esSide = Nothing
  }

morphoSupplyEvent :: EventSignature
morphoSupplyEvent = EventSignature
  { esName = "MorphoSupply"
  , esTopic = eventTopic "Supply(bytes32,address,address,uint256,uint256)"
  , esTxType = "lending_supply"
  , esSide = Nothing
  }

morphoWithdrawEvent :: EventSignature
morphoWithdrawEvent = EventSignature
  { esName = "MorphoWithdraw"
  , esTopic = eventTopic "Withdraw(bytes32,address,address,address,uint256,uint256)"
  , esTxType = "lending_withdraw"
  , esSide = Nothing
  }

morphoBorrowEvent :: EventSignature
morphoBorrowEvent = EventSignature
  { esName = "MorphoBorrow"
  , esTopic = eventTopic "Borrow(bytes32,address,address,address,uint256,uint256)"
  , esTxType = "lending_borrow"
  , esSide = Nothing
  }

morphoRepayEvent :: EventSignature
morphoRepayEvent = EventSignature
  { esName = "MorphoRepay"
  , esTopic = eventTopic "Repay(bytes32,address,address,uint256,uint256)"
  , esTxType = "lending_repay"
  , esSide = Nothing
  }

allEventSignatures :: [EventSignature]
allEventSignatures =
  [ mintEvent
  , burnEvent
  , tokenExchangeEvent
  , zapMintEvent
  , zapBurnEvent
  , stakingDepositEvent
  , stakingWithdrawEvent
  , leverageOpenedEvent
  , leverageClosedEvent
  , morphoSupplyEvent
  , morphoWithdrawEvent
  , morphoBorrowEvent
  , morphoRepayEvent
  ]
