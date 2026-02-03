# API Migration Guide

This guide explains how to migrate from direct wagmi RPC calls to the new backend API client.

## Overview

The migration splits data access into two patterns:
1. **Read operations** → Backend API (caching, batching, aggregation)
2. **Write operations** → Direct wagmi (user wallet required for signing)

## Directory Structure

```
src/api/
├── index.ts     # Public exports
├── types.ts     # TypeScript types matching backend API
├── client.ts    # PlethApiClient class (HTTP + WebSocket)
└── hooks.ts     # React Query hooks wrapping the client
```

## Migration by Hook Category

### Protocol Data

**Before:**
```typescript
// Multiple separate hooks
const { data: status } = useProtocolStatus();  // useReadContract
const { data: prices } = useTokenPrices();     // useReadContract x2
const { data: apy } = useMorphoApy();          // useReadContract
```

**After:**
```typescript
import { useProtocolStatus } from '../api';

// Single aggregated call
const { data: protocol } = useProtocolStatus();
// protocol.data.prices, protocol.data.status, protocol.data.apy
```

### User Balances

**Before:**
```typescript
const { data: usdc } = useBalance({ token: USDC });
const { data: bear } = useBalance({ token: DXY_BEAR });
const { data: bull } = useBalance({ token: DXY_BULL });
const { data: stakedBear } = useStakedBalance('bear');
const { data: stakedBull } = useStakedBalance('bull');
```

**After:**
```typescript
import { useUserDashboard } from '../api';

const { data: user } = useUserDashboard(address);
// user.data.balances.usdc, .bear, .bull, .stakedBear, .stakedBull
```

### Leverage Positions

**Before:**
```typescript
const { data: bearPosition } = useLeveragePosition('bear', address);
const { data: bullPosition } = useLeveragePosition('bull', address);
```

**After:**
```typescript
import { useUserDashboard } from '../api';

const { data: user } = useUserDashboard(address);
// user.data.leverage.bear, user.data.leverage.bull
// Pre-computed: healthFactor, liquidationPrice, leverage ratio
```

### Quote Previews

**Before:**
```typescript
const { data: mintPreview } = usePreviewMint(amount);
const { data: zapQuote } = useZapQuote('buy', amount);
```

**After:**
```typescript
import { useMintQuote, useZapQuote } from '../api';

const { data: mint } = useMintQuote(amount);
const { data: zap } = useZapQuote('buy', amount);
```

### Transaction History

**Before:**
```typescript
// Mock data or no implementation
const transactions = useMockTransactionHistory();
```

**After:**
```typescript
import { useTransactionHistory } from '../api';

const {
  data,
  fetchNextPage,
  hasNextPage
} = useTransactionHistory(address, { limit: 20 });

// Infinite scroll support built-in
```

## Hooks That Stay with wagmi

These hooks require user wallet interaction and cannot move to the backend:

| Hook | Reason |
|------|--------|
| `useApprove` | Requires user signature |
| `useMint` | Transaction signing |
| `useBurn` | Transaction signing |
| `useStake` / `useUnstake` | Transaction signing |
| `useOpenLeverage` / `useCloseLeverage` | Transaction signing |
| `useSupply` / `useWithdraw` / `useBorrow` / `useRepay` | Transaction signing |
| `useCurveSwap` / `useZapSwap` | Transaction signing |
| `useAdjustCollateral` | Transaction signing |

## Environment Configuration

Add these to `.env`:

```bash
# API Configuration
VITE_API_URL=http://localhost:3001      # REST API
VITE_WS_URL=ws://localhost:3001         # WebSocket

# Production
# VITE_API_URL=https://api.plether.io
# VITE_WS_URL=wss://api.plether.io
```

## Real-Time Updates

### Price Streaming

```typescript
import { useWebSocketPrices } from '../api';

function PriceTicker() {
  const prices = useWebSocketPrices();

  if (!prices) return <Skeleton />;

  return (
    <div>
      <span>BEAR: {formatPrice(prices.bear)}</span>
      <span>BULL: {formatPrice(prices.bull)}</span>
    </div>
  );
}
```

### Custom WebSocket Handling

```typescript
import { useWebSocketConnection } from '../api';

function Dashboard() {
  const {
    isConnected,
    lastMessage,
    connect,
    disconnect,
    subscribe
  } = useWebSocketConnection(address);

  useEffect(() => {
    connect();
    return () => disconnect();
  }, []);

  // Handle messages
  useEffect(() => {
    if (lastMessage?.type === 'position') {
      // Invalidate relevant queries
      queryClient.invalidateQueries(['user', address, 'positions']);
    }
  }, [lastMessage]);
}
```

## Query Cache Invalidation

After a successful transaction, invalidate relevant API caches:

```typescript
import { useQueryClient } from '@tanstack/react-query';
import { apiQueryKeys } from '../api';

function useMintWithInvalidation() {
  const queryClient = useQueryClient();
  const { mint } = useMint();

  const mintAndInvalidate = async (amount: bigint) => {
    const result = await mint(amount);

    if (Result.isOk(result)) {
      // Invalidate user data after successful mint
      queryClient.invalidateQueries({
        queryKey: apiQueryKeys.user.all(address)
      });
    }

    return result;
  };

  return { mint: mintAndInvalidate };
}
```

## Error Handling

API errors are typed and can be handled consistently:

```typescript
import { PlethApiError } from '../api';

const { data, error } = useUserDashboard(address);

if (error) {
  if (error instanceof PlethApiError) {
    switch (error.code) {
      case 'RATE_LIMITED':
        // Show rate limit message
        break;
      case 'RPC_ERROR':
        // Show RPC failure message, data may be stale
        break;
      case 'NETWORK_ERROR':
        // Show offline message
        break;
    }
  }
}
```

## Stale Data Handling

The API can return stale data when RPCs are unavailable:

```typescript
const { data } = useUserDashboard(address);

if (data?.meta.stale) {
  // Show warning that data may be outdated
  toast.warning('Data may be outdated. Last updated: ' +
    new Date(data.meta.cachedAt * 1000).toLocaleTimeString()
  );
}
```

## Testing with Mocked API

```typescript
import { vi } from 'vitest';
import { plethApi } from '../api';

vi.mock('../api/client', () => ({
  plethApi: {
    getUserDashboard: vi.fn().mockResolvedValue({
      ok: true,
      value: {
        data: mockUserDashboard,
        meta: { cached: false, blockNumber: 12345, chainId: 1 }
      }
    })
  }
}));
```

## Migration Checklist

- [ ] Add environment variables for API URLs
- [ ] Replace `useProtocolStatus` usages with API hook
- [ ] Replace balance fetching with `useUserDashboard`
- [ ] Replace position fetching with `useUserDashboard`
- [ ] Replace quote hooks with API quote hooks
- [ ] Add WebSocket connection for real-time prices
- [ ] Implement cache invalidation after transactions
- [ ] Add transaction history using API (when indexer deployed)
- [ ] Remove unused direct RPC hooks
- [ ] Test stale data handling
- [ ] Test error handling for API failures
