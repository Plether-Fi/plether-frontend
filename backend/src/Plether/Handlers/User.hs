module Plether.Handlers.User
  ( getUserDashboard
  , getUserBalances
  , getUserPositions
  , getUserAllowances
  ) where

import Data.Text (Text)
import Plether.Config (Addresses (..), Config (..))
import Plether.Ethereum.Client (EthClient, RpcError (..), ethBlockNumber)
import qualified Plether.Ethereum.Contracts.ERC20 as ERC20
import qualified Plether.Ethereum.Contracts.StakedToken as Staked
import Plether.Types

getUserDashboard :: EthClient -> Config -> Text -> IO (Either ApiError (ApiResponse UserDashboard))
getUserDashboard client cfg userAddr = do
  eBalances <- getUserBalancesRaw client cfg userAddr
  eBlockNum <- ethBlockNumber client

  case (eBalances, eBlockNum) of
    (Right balances, Right blockNum) -> do
      let dashboard =
            UserDashboard
              { dashBalances = balances
              , dashLeverage =
                  LeveragePositions
                    { levPosBear = Nothing
                    , levPosBull = Nothing
                    }
              , dashLending =
                  LendingPositions
                    { lendPosBear = Nothing
                    , lendPosBull = Nothing
                    }
              }
      pure $ Right $ mkResponse blockNum (cfgChainId cfg) dashboard
    (Left err, _) -> pure $ Left err
    (_, Left err) -> pure $ Left $ rpcErrorToApiError err

getUserBalances :: EthClient -> Config -> Text -> IO (Either ApiError (ApiResponse UserBalances))
getUserBalances client cfg userAddr = do
  eBalances <- getUserBalancesRaw client cfg userAddr
  eBlockNum <- ethBlockNumber client

  case (eBalances, eBlockNum) of
    (Right balances, Right blockNum) ->
      pure $ Right $ mkResponse blockNum (cfgChainId cfg) balances
    (Left err, _) -> pure $ Left err
    (_, Left err) -> pure $ Left $ rpcErrorToApiError err

getUserBalancesRaw :: EthClient -> Config -> Text -> IO (Either ApiError UserBalances)
getUserBalancesRaw client cfg userAddr = do
  let addrs = cfgAddresses cfg

  eUsdc <- ERC20.balanceOf client (addrUsdc addrs) userAddr
  eBear <- ERC20.balanceOf client (addrDxyBear addrs) userAddr
  eBull <- ERC20.balanceOf client (addrDxyBull addrs) userAddr
  eStakedBear <- Staked.balanceOf client (addrStakingBear addrs) userAddr
  eStakedBull <- Staked.balanceOf client (addrStakingBull addrs) userAddr

  case (eUsdc, eBear, eBull, eStakedBear, eStakedBull) of
    (Right usdc, Right bear, Right bull, Right stakedBear, Right stakedBull) -> do
      eStakedBearAssets <-
        if stakedBear > 0
          then Staked.convertToAssets client (addrStakingBear addrs) stakedBear
          else pure (Right 0)
      eStakedBullAssets <-
        if stakedBull > 0
          then Staked.convertToAssets client (addrStakingBull addrs) stakedBull
          else pure (Right 0)

      case (eStakedBearAssets, eStakedBullAssets) of
        (Right stakedBearAssets, Right stakedBullAssets) ->
          pure $
            Right $
              UserBalances
                { balUsdc = usdc
                , balBear = bear
                , balBull = bull
                , balStakedBear = stakedBear
                , balStakedBull = stakedBull
                , balStakedBearAssets = stakedBearAssets
                , balStakedBullAssets = stakedBullAssets
                }
        (Left err, _) -> pure $ Left $ rpcErrorToApiError err
        (_, Left err) -> pure $ Left $ rpcErrorToApiError err
    (Left err, _, _, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, Left err, _, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, Left err, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, _, Left err, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, _, _, Left err) -> pure $ Left $ rpcErrorToApiError err

getUserPositions :: EthClient -> Config -> Text -> IO (Either ApiError (ApiResponse UserPositions))
getUserPositions client cfg _userAddr = do
  eBlockNum <- ethBlockNumber client

  case eBlockNum of
    Right blockNum -> do
      let positions =
            UserPositions
              { posLeverage =
                  LeveragePositions
                    { levPosBear = Nothing
                    , levPosBull = Nothing
                    }
              , posLending =
                  LendingPositions
                    { lendPosBear = Nothing
                    , lendPosBull = Nothing
                    }
              }
      pure $ Right $ mkResponse blockNum (cfgChainId cfg) positions
    Left err -> pure $ Left $ rpcErrorToApiError err

getUserAllowances :: EthClient -> Config -> Text -> IO (Either ApiError (ApiResponse UserAllowances))
getUserAllowances client cfg userAddr = do
  let addrs = cfgAddresses cfg

  eBlockNum <- ethBlockNumber client

  eUsdcSplitter <- ERC20.allowance client (addrUsdc addrs) userAddr (addrSyntheticSplitter addrs)
  eUsdcZap <- ERC20.allowance client (addrUsdc addrs) userAddr (addrZapRouter addrs)

  eBearSplitter <- ERC20.allowance client (addrDxyBear addrs) userAddr (addrSyntheticSplitter addrs)
  eBearStaking <- ERC20.allowance client (addrDxyBear addrs) userAddr (addrStakingBear addrs)
  eBearLeverage <- ERC20.allowance client (addrDxyBear addrs) userAddr (addrLeverageRouter addrs)
  eBearCurve <- ERC20.allowance client (addrDxyBear addrs) userAddr (addrCurvePool addrs)

  eBullSplitter <- ERC20.allowance client (addrDxyBull addrs) userAddr (addrSyntheticSplitter addrs)
  eBullStaking <- ERC20.allowance client (addrDxyBull addrs) userAddr (addrStakingBull addrs)
  eBullLeverage <- ERC20.allowance client (addrDxyBull addrs) userAddr (addrBullLeverageRouter addrs)

  case ( eBlockNum
       , eUsdcSplitter
       , eUsdcZap
       , eBearSplitter
       , eBearStaking
       , eBearLeverage
       , eBearCurve
       , eBullSplitter
       , eBullStaking
       , eBullLeverage
       ) of
    ( Right blockNum
      , Right usdcSplitter
      , Right usdcZap
      , Right bearSplitter
      , Right bearStaking
      , Right bearLeverage
      , Right bearCurve
      , Right bullSplitter
      , Right bullStaking
      , Right bullLeverage
      ) -> do
        let allowances =
              UserAllowances
                { allowUsdc =
                    UsdcAllowances
                      { usdcAllowSplitter = usdcSplitter
                      , usdcAllowZap = usdcZap
                      , usdcAllowMorphoBear = 0
                      , usdcAllowMorphoBull = 0
                      }
                , allowBear =
                    BearAllowances
                      { bearAllowSplitter = bearSplitter
                      , bearAllowStaking = bearStaking
                      , bearAllowLeverageRouter = bearLeverage
                      , bearAllowCurvePool = bearCurve
                      }
                , allowBull =
                    BullAllowances
                      { bullAllowSplitter = bullSplitter
                      , bullAllowStaking = bullStaking
                      , bullAllowLeverageRouter = bullLeverage
                      }
                }
        pure $ Right $ mkResponse blockNum (cfgChainId cfg) allowances
    (Left err, _, _, _, _, _, _, _, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, Left err, _, _, _, _, _, _, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, Left err, _, _, _, _, _, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, _, Left err, _, _, _, _, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, _, _, Left err, _, _, _, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, _, _, _, Left err, _, _, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, _, _, _, _, Left err, _, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, _, _, _, _, _, Left err, _, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, _, _, _, _, _, _, Left err, _) -> pure $ Left $ rpcErrorToApiError err
    (_, _, _, _, _, _, _, _, _, Left err) -> pure $ Left $ rpcErrorToApiError err

rpcErrorToApiError :: RpcError -> ApiError
rpcErrorToApiError = \case
  RpcHttpError msg -> rpcError msg
  RpcJsonError msg -> rpcError msg
  RpcNodeError _ msg -> rpcError msg
