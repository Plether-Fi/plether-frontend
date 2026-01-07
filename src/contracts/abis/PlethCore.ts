// Placeholder ABI for PlethCore (mint/burn) - replace with actual ABI
export const PLETH_CORE_ABI = [
  {
    type: 'function',
    name: 'status',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ type: 'uint8', name: 'status' }], // 0=Active, 1=Paused, 2=Settled
  },
  {
    type: 'function',
    name: 'mint',
    stateMutability: 'nonpayable',
    inputs: [{ name: 'usdcAmount', type: 'uint256' }],
    outputs: [{ type: 'uint256', name: 'pairAmount' }],
  },
  {
    type: 'function',
    name: 'burn',
    stateMutability: 'nonpayable',
    inputs: [{ name: 'pairAmount', type: 'uint256' }],
    outputs: [{ type: 'uint256', name: 'usdcAmount' }],
  },
  {
    type: 'function',
    name: 'getMintOutput',
    stateMutability: 'view',
    inputs: [{ name: 'usdcAmount', type: 'uint256' }],
    outputs: [{ type: 'uint256', name: 'pairAmount' }],
  },
  {
    type: 'function',
    name: 'getBurnOutput',
    stateMutability: 'view',
    inputs: [{ name: 'pairAmount', type: 'uint256' }],
    outputs: [{ type: 'uint256', name: 'usdcAmount' }],
  },
  {
    type: 'event',
    name: 'Mint',
    inputs: [
      { name: 'user', type: 'address', indexed: true },
      { name: 'usdcAmount', type: 'uint256', indexed: false },
      { name: 'pairAmount', type: 'uint256', indexed: false },
    ],
  },
  {
    type: 'event',
    name: 'Burn',
    inputs: [
      { name: 'user', type: 'address', indexed: true },
      { name: 'pairAmount', type: 'uint256', indexed: false },
      { name: 'usdcAmount', type: 'uint256', indexed: false },
    ],
  },
] as const
