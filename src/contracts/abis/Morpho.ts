// Morpho Blue ABI - Position queries and authorization
export const MORPHO_ABI = [
  {
    type: 'function',
    name: 'position',
    stateMutability: 'view',
    inputs: [
      { name: 'id', type: 'bytes32' },
      { name: 'user', type: 'address' },
    ],
    outputs: [
      { type: 'uint256', name: 'supplyShares' },
      { type: 'uint128', name: 'borrowShares' },
      { type: 'uint128', name: 'collateral' },
    ],
  },
  {
    type: 'function',
    name: 'isAuthorized',
    stateMutability: 'view',
    inputs: [
      { name: 'authorizer', type: 'address' },
      { name: 'authorized', type: 'address' },
    ],
    outputs: [{ type: 'bool' }],
  },
  {
    type: 'function',
    name: 'setAuthorization',
    stateMutability: 'nonpayable',
    inputs: [
      { name: 'authorized', type: 'address' },
      { name: 'newIsAuthorized', type: 'bool' },
    ],
    outputs: [],
  },
] as const
