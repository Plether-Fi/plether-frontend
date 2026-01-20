# Plether Protocol API Reference

Public interfaces for all Plether smart contracts. Generated from NatSpec documentation.

## Table of Contents

- [Core Contracts](#core-contracts)
  - [SyntheticSplitter](#syntheticsplitter)
  - [SyntheticToken](#synthetictoken)
  - [StakedToken](#stakedtoken)
- [Routers](#routers)
  - [ZapRouter](#zaprouter)
  - [LeverageRouter](#leveragerouter)
  - [BullLeverageRouter](#bullleveragerouter)
- [Oracles](#oracles)
  - [BasketOracle](#basketoracle)
  - [MorphoOracle](#morphooracle)
  - [StakedOracle](#stakedoracle)
- [Adapters](#adapters)
  - [MorphoAdapter](#morphoadapter)

---

## Core Contracts

### SyntheticSplitter

Central protocol contract for minting/burning synthetic plDXY tokens. Accepts USDC collateral to mint equal amounts of plDXY-BEAR + plDXY-BULL tokens. Maintains 10% liquidity buffer locally, 90% deployed to yield adapters. Three lifecycle states: ACTIVE → PAUSED → SETTLED (liquidated).

#### State Variables

| Name | Type | Description |
|------|------|-------------|
| `TOKEN_A` | `SyntheticToken` | plDXY-BEAR token |
| `TOKEN_B` | `SyntheticToken` | plDXY-BULL token |
| `USDC` | `IERC20` | USDC collateral token |
| `ORACLE` | `AggregatorV3Interface` | Price oracle |
| `CAP` | `uint256` | Maximum plDXY price (8 decimals). Triggers liquidation when breached |
| `BUFFER_PERCENT` | `uint256` | 10% liquidity buffer |
| `yieldAdapter` | `IERC4626` | Current yield adapter |
| `treasury` | `address` | Treasury for fee distribution |
| `staking` | `address` | Staking contract for yield distribution |
| `isLiquidated` | `bool` | True when price >= CAP |
| `ORACLE_TIMEOUT` | `uint256` | 8 hours staleness timeout |
| `TIMELOCK_DELAY` | `uint256` | 7 days for governance changes |
| `HARVEST_REWARD_BPS` | `uint256` | 10 bps (0.1%) caller incentive |
| `MIN_SURPLUS_THRESHOLD` | `uint256` | 50 USDC minimum for harvest |

#### Functions

##### `previewMint(uint256 mintAmount) → (uint256 usdcRequired, uint256 depositToAdapter, uint256 keptInBuffer)`
Simulates a mint to see required USDC input.

##### `mint(uint256 amount)`
Mint plDXY-BEAR and plDXY-BULL tokens by depositing USDC collateral.
- `amount`: The amount of token pairs to mint (18 decimals)

##### `previewBurn(uint256 burnAmount) → (uint256 usdcRefund, uint256 withdrawnFromAdapter)`
Simulates a burn to see USDC return.

##### `burn(uint256 amount)`
Burn plDXY-BEAR and plDXY-BULL tokens to redeem USDC collateral.
- `amount`: The amount of token pairs to burn (18 decimals)

##### `triggerLiquidation()`
Locks the protocol into liquidated state when price >= CAP. Permissionless.

##### `emergencyRedeem(uint256 amount)`
Emergency redemption when protocol is liquidated. Only burns plDXY-BEAR tokens at CAP price. plDXY-BULL becomes worthless.
- `amount`: The amount of plDXY-BEAR tokens to redeem (18 decimals)

##### `previewHarvest() → (bool canHarvest, uint256 totalSurplus, uint256 callerReward, uint256 treasuryShare, uint256 stakingShare)`
Previews yield harvest amounts and eligibility.

##### `harvestYield()`
Permissionless yield harvesting. Distributes surplus: 0.1% to caller, 20% to treasury, 79.9% to staking.

##### `currentStatus() → Status`
Returns the current protocol lifecycle status (ACTIVE, PAUSED, or SETTLED).

##### `getSystemStatus() → SystemStatus`
Returns comprehensive system metrics for dashboards.

##### `ejectLiquidity()` (owner)
Emergency exit: withdraws all funds from yield adapter. Bypasses timelock. Auto-pauses protocol.

##### `withdrawFromAdapter(uint256 amount)` (owner)
Withdraws a specific amount from yield adapter while paused.

##### `proposeAdapter(address _newAdapter)` (owner)
Propose a new yield adapter (7-day timelock).

##### `finalizeAdapter()` (owner)
Finalize adapter migration after timelock.

##### `proposeFeeReceivers(address _treasury, address _staking)` (owner)
Propose new fee receiver addresses (7-day timelock).

##### `finalizeFeeReceivers()` (owner)
Finalize pending fee receiver change after timelock expires.

##### `pause()` / `unpause()` (owner)
Pause/unpause the protocol.

##### `rescueToken(address token, address to)` (owner)
Rescue accidentally sent tokens. Cannot rescue core assets.

#### Enums

```solidity
enum Status { ACTIVE, PAUSED, SETTLED }
```

---

### SyntheticToken

ERC20 token with flash mint and permit capability, controlled by SyntheticSplitter. Used for plDXY-BEAR and plDXY-BULL tokens. Inherits ERC20FlashMint for fee-free flash loans used by routers.

#### State Variables

| Name | Type | Description |
|------|------|-------------|
| `SPLITTER` | `address` | The SyntheticSplitter that controls minting/burning |

#### Functions

##### `mint(address to, uint256 amount)` (splitter only)
Mint tokens to an address.

##### `burn(address from, uint256 amount)` (splitter only)
Burn tokens from an address.

*Inherits ERC20, ERC20Permit, ERC20FlashMint*

---

### StakedToken

ERC4626 vault for staking plDXY-BEAR or plDXY-BULL tokens. Used as Morpho collateral. Exchange rate increases via yield donations. Implements 1000x virtual share offset to prevent inflation attacks.

#### Functions

##### `donateYield(uint256 amount)`
Allows anyone to inject yield into the vault. Increases the share price for all stakers immediately.

##### `depositWithPermit(uint256 assets, address receiver, uint256 deadline, uint8 v, bytes32 r, bytes32 s) → uint256 shares`
Deposit assets with a permit signature (gasless approval).

*Inherits ERC4626 (deposit, withdraw, redeem, etc.)*

---

## Routers

### ZapRouter

Efficient router for acquiring plDXY-BULL tokens using flash mints. Flash mints plDXY-BEAR → swaps to USDC via Curve → mints pairs → keeps plDXY-BULL. For plDXY-BEAR, users should swap directly on Curve instead.

#### Constants

| Name | Value | Description |
|------|-------|-------------|
| `MAX_SLIPPAGE_BPS` | 100 | Maximum 1% slippage |
| `SAFETY_BUFFER_BPS` | 50 | 0.5% safety buffer |
| `USDC_INDEX` | 0 | Curve pool index |
| `PLDXY_BEAR_INDEX` | 1 | Curve pool index |

#### State Variables

| Name | Type | Description |
|------|------|-------------|
| `SPLITTER` | `ISyntheticSplitter` | SyntheticSplitter contract |
| `PLDXY_BEAR` | `IERC20` | plDXY-BEAR token |
| `PLDXY_BULL` | `IERC20` | plDXY-BULL token |
| `USDC` | `IERC20` | USDC stablecoin |
| `CURVE_POOL` | `ICurvePool` | Curve pool for swaps |
| `CAP` | `uint256` | Protocol CAP price |

#### Functions

##### `zapMint(uint256 usdcAmount, uint256 minAmountOut, uint256 maxSlippageBps, uint256 deadline)`
Buy plDXY-BULL using USDC with flash mint efficiency.
- `usdcAmount`: Amount of USDC to send
- `minAmountOut`: Minimum plDXY-BULL tokens to receive
- `maxSlippageBps`: Max slippage (capped at 1%)
- `deadline`: Unix timestamp deadline

##### `zapBurn(uint256 bullAmount, uint256 minUsdcOut, uint256 deadline)`
Sell plDXY-BULL tokens for USDC.

##### `zapBurnWithPermit(uint256 bullAmount, uint256 minUsdcOut, uint256 deadline, uint8 v, bytes32 r, bytes32 s)`
Sell plDXY-BULL with permit signature (gasless approval).

##### `previewZapMint(uint256 usdcAmount) → (uint256 flashAmount, uint256 expectedSwapOut, uint256 totalUSDC, uint256 expectedTokensOut, uint256 flashFee)`
Preview the result of a zapMint operation.

##### `previewZapBurn(uint256 bullAmount) → (uint256 expectedUsdcFromBurn, uint256 usdcForBearBuyback, uint256 expectedUsdcOut, uint256 flashFee)`
Preview the result of a zapBurn operation.

##### `pause()` / `unpause()` (owner)
Pause/unpause the router.

---

### LeverageRouter

Leverage router for plDXY-BEAR positions via Morpho Blue. Flash loans USDC from Morpho → swaps to plDXY-BEAR on Curve → stakes → deposits to Morpho as collateral. Requires user to authorize this contract in Morpho before use.

#### Constants

| Name | Value | Description |
|------|-------|-------------|
| `MAX_SLIPPAGE_BPS` | 100 | Maximum 1% slippage |
| `EXCHANGE_RATE_BUFFER_BPS` | 100 | 1% buffer for exchange rate drift |

#### State Variables

| Name | Type | Description |
|------|------|-------------|
| `MORPHO` | `IMorpho` | Morpho Blue protocol |
| `CURVE_POOL` | `ICurvePool` | Curve pool for swaps |
| `USDC` | `IERC20` | USDC token |
| `PLDXY_BEAR` | `IERC20` | plDXY-BEAR token |
| `STAKED_PLDXY_BEAR` | `IERC4626` | Staked plDXY-BEAR vault |
| `marketParams` | `MarketParams` | Morpho market configuration |

#### Functions

##### `openLeverage(uint256 principal, uint256 leverage, uint256 maxSlippageBps, uint256 deadline)`
Open a leveraged plDXY-BEAR position in one transaction.
- `principal`: Amount of USDC user sends
- `leverage`: Multiplier (e.g. 3x = 3e18)
- `maxSlippageBps`: Max slippage (capped at 1%)
- `deadline`: Unix timestamp deadline

**Prerequisite**: User must call `morpho.setAuthorization(leverageRouter, true)` first.

##### `closeLeverage(uint256 collateralToWithdraw, uint256 maxSlippageBps, uint256 deadline)`
Close a leveraged position in one transaction.
- `collateralToWithdraw`: Amount of splDXY-BEAR **shares** to withdraw (not underlying amount)

##### `getActualDebt(address user) → uint256 debt`
Returns the user's current debt including accrued interest.

##### `previewOpenLeverage(uint256 principal, uint256 leverage) → (uint256 loanAmount, uint256 totalUSDC, uint256 expectedPlDxyBear, uint256 expectedDebt)`
Preview opening a leveraged position.

##### `previewCloseLeverage(uint256 debtToRepay, uint256 collateralToWithdraw) → (uint256 expectedUSDC, uint256 flashFee, uint256 expectedReturn)`
Preview closing a leveraged position.

##### `pause()` / `unpause()` (owner)
Pause/unpause the router.

---

### BullLeverageRouter

Leverage router for plDXY-BULL positions via Morpho Blue. Uses Morpho flash loans + Splitter minting to acquire plDXY-BULL, then deposits as Morpho collateral.

**Open Flow**: Flash loan USDC → mint pairs via Splitter → sell plDXY-BEAR on Curve → stake plDXY-BULL → deposit splDXY-BULL to Morpho → borrow to repay flash loan

**Close Flow**: Flash mint plDXY-BEAR → sell extra for USDC → repay Morpho debt → withdraw splDXY-BULL → unstake → redeem pairs → buy back plDXY-BEAR → return surplus to user

#### State Variables

| Name | Type | Description |
|------|------|-------------|
| `SPLITTER` | `ISyntheticSplitter` | SyntheticSplitter contract |
| `PLDXY_BULL` | `IERC20` | plDXY-BULL token |
| `STAKED_PLDXY_BULL` | `IERC4626` | Staked plDXY-BULL vault |
| `CAP` | `uint256` | Protocol CAP price |
| `EXCHANGE_RATE_BUFFER_BPS` | `uint256` | 1% buffer for exchange rate drift |

#### Functions

##### `openLeverage(uint256 principal, uint256 leverage, uint256 maxSlippageBps, uint256 deadline)`
Open a leveraged plDXY-BULL position in one transaction.

**Prerequisite**: User must call `morpho.setAuthorization(bullLeverageRouter, true)` first.

##### `closeLeverage(uint256 collateralToWithdraw, uint256 maxSlippageBps, uint256 deadline)`
Close a leveraged plDXY-BULL position in one transaction.
- `collateralToWithdraw`: Amount of splDXY-BULL **shares** to withdraw

##### `getActualDebt(address user) → uint256 debt`
Returns the user's current debt including accrued interest.

##### `previewOpenLeverage(uint256 principal, uint256 leverage) → (uint256 loanAmount, uint256 totalUSDC, uint256 expectedPlDxyBull, uint256 expectedDebt)`
Preview opening a leveraged position.

##### `previewCloseLeverage(uint256 debtToRepay, uint256 collateralToWithdraw) → (uint256 expectedUSDC, uint256 usdcForBearBuyback, uint256 expectedReturn)`
Preview closing a leveraged position.

##### `pause()` / `unpause()` (owner)
Pause/unpause the router.

---

## Oracles

### BasketOracle

Aggregates multiple Chainlink feeds into a normalized weighted plDXY basket price. Price = Sum(Weight_i * Price_i / BasePrice_i). Implements AggregatorV3Interface for compatibility.

#### Constants

| Name | Value | Description |
|------|-------|-------------|
| `DECIMALS` | 8 | Chainlink standard decimals |
| `TIMELOCK_DELAY` | 7 days | For Curve pool updates |

#### State Variables

| Name | Type | Description |
|------|------|-------------|
| `components` | `Component[]` | Array of currency components (EUR, JPY, GBP, CAD, SEK, CHF) |
| `curvePool` | `ICurvePool` | Curve pool for deviation validation |
| `MAX_DEVIATION_BPS` | `uint256` | Maximum deviation from Curve spot |
| `CAP` | `uint256` | Protocol CAP price (8 decimals) |

#### Functions

##### `latestRoundData() → (uint80 roundId, int256 answer, uint256 startedAt, uint256 updatedAt, uint80 answeredInRound)`
Returns the aggregated basket price from all component feeds.
- `answer`: The calculated basket price in 8 decimals

##### `decimals() → uint8`
Returns 8.

##### `description() → string`
Returns "plDXY Fixed Basket (Bounded)".

##### `setCurvePool(address _curvePool)` (owner)
Sets the Curve pool for deviation validation (initial setup only).

##### `proposeCurvePool(address _newPool)` (owner)
Proposes a new Curve pool (requires 7-day timelock).

##### `finalizeCurvePool()` (owner)
Finalizes the Curve pool update after timelock expires.

---

### MorphoOracle

Adapts BasketOracle price to Morpho Blue's 1e36 scale format. Supports both plDXY-BEAR (direct) and plDXY-BULL (inverse) pricing.

#### State Variables

| Name | Type | Description |
|------|------|-------------|
| `BASKET_ORACLE` | `AggregatorV3Interface` | Source price feed |
| `CAP` | `uint256` | Protocol CAP price (8 decimals) |
| `IS_INVERSE` | `bool` | True for plDXY-BULL (CAP - Price) |
| `STALENESS_TIMEOUT` | `uint256` | 8 hours max age |

#### Functions

##### `price() → uint256`
Returns collateral price scaled to 1e36.
- For plDXY-BEAR: direct price
- For plDXY-BULL: CAP - price

---

### StakedOracle

Prices ERC4626 vault shares by combining underlying price with exchange rate. Price = UnderlyingPrice * ExchangeRate. Used for splDXY-BEAR/splDXY-BULL in Morpho.

#### State Variables

| Name | Type | Description |
|------|------|-------------|
| `VAULT` | `IERC4626` | The staking vault |
| `UNDERLYING_ORACLE` | `IOracle` | Oracle for the underlying plDXY token |

#### Functions

##### `price() → uint256`
Returns price of 1 vault share including accrued yield. Scaled to 1e36.

---

## Adapters

### MorphoAdapter

ERC4626-compliant wrapper for Morpho Blue lending. Interchangeable with other yield adapters. Only accepts deposits from SyntheticSplitter.

#### State Variables

| Name | Type | Description |
|------|------|-------------|
| `MORPHO` | `IMorpho` | Morpho Blue protocol |
| `marketParams` | `MarketParams` | Morpho market parameters |
| `MARKET_ID` | `bytes32` | Computed market ID |
| `SPLITTER` | `address` | Authorized depositor |
| `urd` | `address` | Universal Rewards Distributor |

#### Functions

##### `totalAssets() → uint256`
Returns total USDC value including accrued interest.

##### `rescueToken(address token, address to)` (owner)
Recovers stuck tokens (excluding the underlying asset).

##### `setUrd(address _urd)` (owner)
Sets the Universal Rewards Distributor address.

##### `claimRewards(address reward, uint256 claimable, bytes32[] proof, address to) → uint256 claimed` (owner)
Claims Morpho rewards and transfers to specified address.

##### `claimRewardsToSelf(address reward, uint256 claimable, bytes32[] proof) → uint256 claimed` (owner)
Claims rewards to this contract for compounding.

*Inherits ERC4626 (deposit, withdraw, redeem - restricted to SPLITTER)*

---

## Common Errors

### SyntheticSplitter Errors
- `Splitter__ZeroAddress` - Zero address provided
- `Splitter__ZeroAmount` - Zero amount specified
- `Splitter__LiquidationActive` - Protocol is liquidated
- `Splitter__NotLiquidated` - Not in liquidation state
- `Splitter__TimelockActive` - Timelock not expired
- `Splitter__NoSurplus` - No yield to harvest
- `Splitter__GovernanceLocked` - 7-day cooldown after unpause
- `Splitter__Insolvent` - Assets < liabilities

### Router Errors
- `ZapRouter__Expired` / `LeverageRouterBase__Expired` - Deadline passed
- `ZapRouter__SlippageExceedsMax` / `LeverageRouterBase__SlippageExceedsMax` - Slippage > 1%
- `LeverageRouterBase__NotAuthorized` - User hasn't authorized router in Morpho
- `LeverageRouterBase__LeverageTooLow` - Leverage <= 1x
- `ZapRouter__InsufficientOutput` / `LeverageRouterBase__InsufficientOutput` - Swap output too low

### Oracle Errors
- `BasketOracle__InvalidPrice` - Component feed returned invalid price
- `BasketOracle__PriceDeviation` - Price deviates too far from Curve spot
- `MorphoOracle__StalePrice` - Oracle data is stale (> 8 hours)
- `MorphoOracle__PriceExceedsCap` - Basket price exceeds CAP

---

## Decimal Reference

| Asset | Decimals |
|-------|----------|
| USDC | 6 |
| plDXY-BEAR / plDXY-BULL | 18 |
| splDXY-BEAR / splDXY-BULL | 18 |
| Oracle prices | 8 |
| Morpho oracle prices | 36 |
| Leverage multiplier | 18 (e.g., 3x = 3e18) |
| Slippage basis points | 0 (e.g., 100 = 1%) |
