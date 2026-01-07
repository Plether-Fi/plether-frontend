// Placeholder ABI for BasketOracle - replace with actual ABI
export const BASKET_ORACLE_ABI = [
  {
    type: 'function',
    name: 'latestPrice',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ type: 'uint256', name: 'price' }],
  },
  {
    type: 'function',
    name: 'decimals',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ type: 'uint8' }],
  },
  {
    type: 'function',
    name: 'lastUpdated',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ type: 'uint256' }],
  },
] as const
