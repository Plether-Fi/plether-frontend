# Plether Protocol Error Selectors

Error selectors for decoding reverts. Use `cast 4byte <selector>` or search this file.

## Quick Lookup by Selector

| Selector | Error |
|----------|-------|
| `0x02e83603` | `LeverageRouterBase__SplitterNotActive()` |
| `0x08744fae` | `MorphoOracle__StalePrice()` |
| `0x090bc858` | `OracleLib__SequencerGracePeriod()` |
| `0x0d6ff717` | `Splitter__LiquidationActive()` |
| `0x0fe775ae` | `LeverageRouterBase__SlippageExceedsMax()` |
| `0x10a713ca` | `MorphoAdapter__InvalidAddress()` |
| `0x118cdaa7` | `OwnableUnauthorizedAccount(address)` |
| `0x1b18b763` | `ZapRouter__InvalidCurvePrice()` |
| `0x1d2a620e` | `Splitter__ZeroRefund()` |
| `0x1e4fbdf7` | `OwnableInvalidOwner(address)` |
| `0x1e82378b` | `ZapRouter__InsufficientOutput()` |
| `0x238f1686` | `ZapRouter__BearPriceAboveCap()` |
| `0x284ff667` | `ERC4626ExceededMaxMint(address,uint256,uint256)` |
| `0x2bb5b1b3` | `BasketOracle__InvalidPrice(address)` |
| `0x2e686365` | `Splitter__AdapterInsufficientLiquidity()` |
| `0x305a27a9` | `StringTooLong(string)` |
| `0x31473b82` | `FlashLoan__InvalidOperation()` |
| `0x3431b67c` | `LeverageRouterBase__ZeroPrincipal()` |
| `0x3ee5aeb5` | `ReentrancyGuardReentrantCall()` |
| `0x3f84e836` | `SyntheticToken__Unauthorized()` |
| `0x4a4a2ca4` | `Splitter__NotLiquidated()` |
| `0x4b800e46` | `ERC2612InvalidSigner(address,address)` |
| `0x4e630da3` | `BasketOracle__InvalidProposal()` |
| `0x4ea8ea0b` | `FlashLoan__InvalidInitiator()` |
| `0x4f2ab8e9` | `Splitter__NoSurplus()` |
| `0x50285b92` | `LeverageRouterBase__LeverageTooLow()` |
| `0x5274afe7` | `SafeERC20FailedOperation(address)` |
| `0x55e52988` | `MorphoAdapter__OnlySplitter()` |
| `0x583fe571` | `Splitter__ZeroAmount()` |
| `0x593fc1f4` | `BasketOracle__InvalidDeviation()` |
| `0x5ea9fe39` | `LeverageRouterBase__Expired()` |
| `0x5f0ad3d9` | `StakedOracle__ZeroAddress()` |
| `0x62791302` | `ERC2612ExpiredSignature(uint256)` |
| `0x678c5b00` | `ERC3156InvalidReceiver(address)` |
| `0x6eb3202d` | `BasketOracle__LengthMismatch()` |
| `0x71e7e49a` | `Splitter__InvalidProposal()` |
| `0x752d88c0` | `InvalidAccountNonce(address,uint256)` |
| `0x79012fb2` | `ERC4626ExceededMaxDeposit(address,uint256,uint256)` |
| `0x7b63ec3e` | `SyntheticToken__ZeroAddress()` |
| `0x7ec11d1d` | `ZapRouter__SplitterNotActive()` |
| `0x80fd91ce` | `LeverageRouterBase__InsufficientOutput()` |
| `0x82793d5a` | `LeverageRouterBase__InvalidCurvePrice()` |
| `0x8b871441` | `Splitter__InvalidCap()` |
| `0x8dfc202b` | `ExpectedPause()` |
| `0x92d64d06` | `MorphoOracle__InvalidPrice()` |
| `0x94280d62` | `ERC20InvalidSpender(address)` |
| `0x96c6fd1e` | `ERC20InvalidSender(address)` |
| `0x970faf44` | `LeverageRouterBase__ZeroCollateral()` |
| `0x97a66649` | `ZapRouter__Expired()` |
| `0x9e0bffb6` | `BasketOracle__TimelockActive()` |
| `0x9f4269de` | `StakedOracle__InvalidPrice()` |
| `0xa573750e` | `Splitter__AdapterNotSet()` |
| `0xaaf6a5c7` | `ZapRouter__SlippageExceedsMax()` |
| `0xac2d6075` | `MorphoAdapter__CannotRescueUnderlying()` |
| `0xad473ba8` | `Splitter__InsufficientHarvest()` |
| `0xafc4799e` | `Splitter__AdapterWithdrawFailed()` |
| `0xb0e1b4f9` | `MorphoAdapter__InvalidMarket()` |
| `0xb1596697` | `BasketOracle__InvalidBasePrice()` |
| `0xb3512b0c` | `InvalidShortString()` |
| `0xb5a7db92` | `ERC3156UnsupportedToken(address)` |
| `0xb94abeec` | `ERC4626ExceededMaxRedeem(address,uint256,uint256)` |
| `0xbb02674c` | `MorphoOracle__PriceExceedsCap()` |
| `0xbcc16562` | `LeverageRouterBase__ZeroAddress()` |
| `0xc17870e6` | `Splitter__TimelockActive()` |
| `0xc43f1901` | `Splitter__CannotRescueCoreAsset()` |
| `0xc4a1093a` | `OracleLib__StalePrice()` |
| `0xcf144b7e` | `Splitter__ZeroAddress()` |
| `0xd536a664` | `MorphoOracle__ZeroAddress()` |
| `0xd67540f3` | `OracleLib__InvalidPrice()` |
| `0xd78bce0c` | `ECDSAInvalidSignatureS(bytes32)` |
| `0xd7f35b86` | `ZapRouter__SolvencyBreach()` |
| `0xd93c0665` | `EnforcedPause()` |
| `0xdfe9bde3` | `OracleLib__SequencerDown()` |
| `0xe0146c26` | `Splitter__MigrationLostFunds()` |
| `0xe0479991` | `ZapRouter__ZeroAddress()` |
| `0xe2cbebc9` | `Splitter__Insolvent()` |
| `0xe450d38c` | `ERC20InsufficientBalance(address,uint256,uint256)` |
| `0xe602df05` | `ERC20InvalidApprover(address)` |
| `0xe901888d` | `Splitter__GovernanceLocked()` |
| `0xe4b0ed2b` | `BasketOracle__AlreadySet()` |
| `0xeb9e1f78` | `FlashLoan__InvalidLender()` |
| `0xec442f05` | `ERC20InvalidReceiver(address)` |
| `0xf471c493` | `ZapRouter__ZeroAmount()` |
| `0xf4d5ee4b` | `LeverageRouterBase__NotAuthorized()` |
| `0xf645eedf` | `ECDSAInvalidSignature()` |
| `0xfbe44393` | `BasketOracle__PriceDeviation(uint256,uint256)` |
| `0xfb8f41b2` | `ERC20InsufficientAllowance(address,uint256,uint256)` |
| `0xfce698f7` | `ECDSAInvalidSignatureLength(uint256)` |
| `0xfd9a7609` | `ERC3156ExceededMaxLoan(uint256)` |
| `0xfe41b1b8` | `Splitter__NotPaused()` |
| `0xfe9cceec` | `ERC4626ExceededMaxWithdraw(address,uint256,uint256)` |

---

## Errors by Contract

### SyntheticSplitter

| Selector | Error | Description |
|----------|-------|-------------|
| `0x583fe571` | `Splitter__ZeroAmount()` | Amount is zero |
| `0xcf144b7e` | `Splitter__ZeroAddress()` | Address is zero |
| `0x1d2a620e` | `Splitter__ZeroRefund()` | Burn would return zero USDC |
| `0x8b871441` | `Splitter__InvalidCap()` | Invalid CAP value |
| `0xa573750e` | `Splitter__AdapterNotSet()` | Yield adapter not configured |
| `0x2e686365` | `Splitter__AdapterInsufficientLiquidity()` | Adapter lacks liquidity |
| `0xafc4799e` | `Splitter__AdapterWithdrawFailed()` | Adapter withdrawal failed |
| `0x0d6ff717` | `Splitter__LiquidationActive()` | Protocol is liquidated |
| `0x4a4a2ca4` | `Splitter__NotLiquidated()` | Protocol not in liquidation |
| `0xc17870e6` | `Splitter__TimelockActive()` | Timelock not expired |
| `0x71e7e49a` | `Splitter__InvalidProposal()` | No pending proposal |
| `0x4f2ab8e9` | `Splitter__NoSurplus()` | No yield surplus to harvest |
| `0xad473ba8` | `Splitter__InsufficientHarvest()` | Harvest below threshold |
| `0xe901888d` | `Splitter__GovernanceLocked()` | 7-day cooldown after unpause |
| `0xe2cbebc9` | `Splitter__Insolvent()` | Assets < liabilities |
| `0xfe41b1b8` | `Splitter__NotPaused()` | Operation requires pause |
| `0xc43f1901` | `Splitter__CannotRescueCoreAsset()` | Cannot rescue USDC/tokens |
| `0xe0146c26` | `Splitter__MigrationLostFunds()` | Adapter migration lost funds |

### SyntheticToken

| Selector | Error | Description |
|----------|-------|-------------|
| `0x3f84e836` | `SyntheticToken__Unauthorized()` | Caller is not Splitter |
| `0x7b63ec3e` | `SyntheticToken__ZeroAddress()` | Zero address for splitter |

### ZapRouter

| Selector | Error | Description |
|----------|-------|-------------|
| `0xe0479991` | `ZapRouter__ZeroAddress()` | Address is zero |
| `0xf471c493` | `ZapRouter__ZeroAmount()` | Amount is zero |
| `0x97a66649` | `ZapRouter__Expired()` | Deadline passed |
| `0xaaf6a5c7` | `ZapRouter__SlippageExceedsMax()` | Slippage > 1% |
| `0x7ec11d1d` | `ZapRouter__SplitterNotActive()` | Splitter not active |
| `0x238f1686` | `ZapRouter__BearPriceAboveCap()` | BEAR price exceeds CAP |
| `0x1e82378b` | `ZapRouter__InsufficientOutput()` | Output below minAmountOut |
| `0x1b18b763` | `ZapRouter__InvalidCurvePrice()` | Curve price query failed |
| `0xd7f35b86` | `ZapRouter__SolvencyBreach()` | Solvency check failed |

### LeverageRouter / BullLeverageRouter

| Selector | Error | Description |
|----------|-------|-------------|
| `0xbcc16562` | `LeverageRouterBase__ZeroAddress()` | Address is zero |
| `0x3431b67c` | `LeverageRouterBase__ZeroPrincipal()` | Principal is zero |
| `0x970faf44` | `LeverageRouterBase__ZeroCollateral()` | Collateral is zero |
| `0x5ea9fe39` | `LeverageRouterBase__Expired()` | Deadline passed |
| `0x50285b92` | `LeverageRouterBase__LeverageTooLow()` | Leverage ≤ 1x |
| `0x0fe775ae` | `LeverageRouterBase__SlippageExceedsMax()` | Slippage > 1% |
| `0xf4d5ee4b` | `LeverageRouterBase__NotAuthorized()` | User not authorized in Morpho |
| `0x80fd91ce` | `LeverageRouterBase__InsufficientOutput()` | Swap output too low |
| `0x82793d5a` | `LeverageRouterBase__InvalidCurvePrice()` | Curve price query failed |
| `0x02e83603` | `LeverageRouterBase__SplitterNotActive()` | Splitter not active |

### FlashLoan (Base)

| Selector | Error | Description |
|----------|-------|-------------|
| `0x4ea8ea0b` | `FlashLoan__InvalidInitiator()` | Flash loan not self-initiated |
| `0xeb9e1f78` | `FlashLoan__InvalidLender()` | Invalid flash loan lender |
| `0x31473b82` | `FlashLoan__InvalidOperation()` | Unknown operation type |

### BasketOracle

| Selector | Error | Description |
|----------|-------|-------------|
| `0x2bb5b1b3` | `BasketOracle__InvalidPrice(address)` | Component feed invalid |
| `0x6eb3202d` | `BasketOracle__LengthMismatch()` | Array lengths don't match |
| `0xfbe44393` | `BasketOracle__PriceDeviation(uint256,uint256)` | Exceeds Curve deviation |
| `0xe4b0ed2b` | `BasketOracle__AlreadySet()` | Curve pool already set |
| `0x9e0bffb6` | `BasketOracle__TimelockActive()` | Timelock not expired |
| `0x4e630da3` | `BasketOracle__InvalidProposal()` | No pending proposal |
| `0x593fc1f4` | `BasketOracle__InvalidDeviation()` | Max deviation is zero |
| `0xb1596697` | `BasketOracle__InvalidBasePrice()` | Base price is zero |

### MorphoOracle

| Selector | Error | Description |
|----------|-------|-------------|
| `0x92d64d06` | `MorphoOracle__InvalidPrice()` | Source oracle invalid |
| `0x08744fae` | `MorphoOracle__StalePrice()` | Price older than 8 hours |
| `0xbb02674c` | `MorphoOracle__PriceExceedsCap()` | Price >= CAP (liquidation) |
| `0xd536a664` | `MorphoOracle__ZeroAddress()` | Zero address in constructor |

### StakedOracle

| Selector | Error | Description |
|----------|-------|-------------|
| `0x9f4269de` | `StakedOracle__InvalidPrice()` | Underlying oracle invalid |
| `0x5f0ad3d9` | `StakedOracle__ZeroAddress()` | Zero address in constructor |

### MorphoAdapter

| Selector | Error | Description |
|----------|-------|-------------|
| `0x55e52988` | `MorphoAdapter__OnlySplitter()` | Caller is not Splitter |
| `0x10a713ca` | `MorphoAdapter__InvalidAddress()` | Zero address provided |
| `0xb0e1b4f9` | `MorphoAdapter__InvalidMarket()` | Market loan token mismatch |
| `0xac2d6075` | `MorphoAdapter__CannotRescueUnderlying()` | Cannot rescue USDC |

### OracleLib

| Selector | Error | Description |
|----------|-------|-------------|
| `0xd67540f3` | `OracleLib__InvalidPrice()` | Price ≤ 0 |
| `0xc4a1093a` | `OracleLib__StalePrice()` | Price older than timeout |
| `0xdfe9bde3` | `OracleLib__SequencerDown()` | L2 sequencer is down |
| `0x090bc858` | `OracleLib__SequencerGracePeriod()` | Sequencer grace period |

### OpenZeppelin (Common)

| Selector | Error | Description |
|----------|-------|-------------|
| `0xd93c0665` | `EnforcedPause()` | Contract is paused |
| `0x8dfc202b` | `ExpectedPause()` | Contract should be paused |
| `0x118cdaa7` | `OwnableUnauthorizedAccount(address)` | Not owner |
| `0x1e4fbdf7` | `OwnableInvalidOwner(address)` | Invalid owner address |
| `0x3ee5aeb5` | `ReentrancyGuardReentrantCall()` | Reentrancy detected |
| `0x5274afe7` | `SafeERC20FailedOperation(address)` | Token transfer failed |
