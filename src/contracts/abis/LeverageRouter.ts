// LeverageRouter ABI - Open/close leveraged positions
export const LEVERAGE_ROUTER_ABI = [
  {
    type: 'function',
    name: 'openLeverage',
    stateMutability: 'nonpayable',
    inputs: [
      { name: 'principal', type: 'uint256' },
      { name: 'leverage', type: 'uint256' },
      { name: 'maxSlippageBps', type: 'uint256' },
      { name: 'deadline', type: 'uint256' },
    ],
    outputs: [
      { type: 'uint256', name: 'positionSize' },
      { type: 'uint256', name: 'debt' },
    ],
  },
  {
    type: 'function',
    name: 'closeLeverage',
    stateMutability: 'nonpayable',
    inputs: [
      { name: 'collateralToWithdraw', type: 'uint256' },
      { name: 'maxSlippageBps', type: 'uint256' },
      { name: 'deadline', type: 'uint256' },
    ],
    outputs: [{ type: 'uint256', name: 'usdcReturned' }],
  },
  {
    type: 'function',
    name: 'addCollateral',
    stateMutability: 'nonpayable',
    inputs: [{ name: 'amount', type: 'uint256' }],
    outputs: [],
  },
  {
    type: 'function',
    name: 'removeCollateral',
    stateMutability: 'nonpayable',
    inputs: [{ name: 'amount', type: 'uint256' }],
    outputs: [],
  },
  {
    type: 'function',
    name: 'previewOpenLeverage',
    stateMutability: 'view',
    inputs: [
      { name: 'principal', type: 'uint256' },
      { name: 'leverage', type: 'uint256' },
    ],
    outputs: [
      { type: 'uint256', name: 'loanAmount' },
      { type: 'uint256', name: 'totalUSDC' },
      { type: 'uint256', name: 'expectedPlDxyBear' },
      { type: 'uint256', name: 'expectedDebt' },
    ],
  },
  {
    type: 'function',
    name: 'previewCloseLeverage',
    stateMutability: 'view',
    inputs: [
      { name: 'debtToRepay', type: 'uint256' },
      { name: 'collateralToWithdraw', type: 'uint256' },
    ],
    outputs: [
      { type: 'uint256', name: 'expectedUSDC' },
      { type: 'uint256', name: 'flashFee' },
      { type: 'uint256', name: 'expectedReturn' },
    ],
  },
  {
    type: 'function',
    name: 'getActualDebt',
    stateMutability: 'view',
    inputs: [{ name: 'user', type: 'address' }],
    outputs: [{ type: 'uint256', name: 'debt' }],
  },
  {
    type: 'function',
    name: 'MORPHO',
    stateMutability: 'view',
    inputs: [],
    outputs: [{ type: 'address' }],
  },
  {
    type: 'function',
    name: 'marketParams',
    stateMutability: 'view',
    inputs: [],
    outputs: [
      { type: 'address', name: 'loanToken' },
      { type: 'address', name: 'collateralToken' },
      { type: 'address', name: 'oracle' },
      { type: 'address', name: 'irm' },
      { type: 'uint256', name: 'lltv' },
    ],
  },
  {
    type: 'event',
    name: 'PositionOpened',
    inputs: [
      { name: 'user', type: 'address', indexed: true },
      { name: 'principal', type: 'uint256', indexed: false },
      { name: 'leverage', type: 'uint256', indexed: false },
      { name: 'positionSize', type: 'uint256', indexed: false },
      { name: 'debt', type: 'uint256', indexed: false },
    ],
  },
  {
    type: 'event',
    name: 'PositionClosed',
    inputs: [
      { name: 'user', type: 'address', indexed: true },
      { name: 'collateralReturned', type: 'uint256', indexed: false },
      { name: 'debtRepaid', type: 'uint256', indexed: false },
    ],
  },
  {
    type: 'event',
    name: 'CollateralAdded',
    inputs: [
      { name: 'user', type: 'address', indexed: true },
      { name: 'amount', type: 'uint256', indexed: false },
    ],
  },
  {
    type: 'event',
    name: 'CollateralRemoved',
    inputs: [
      { name: 'user', type: 'address', indexed: true },
      { name: 'amount', type: 'uint256', indexed: false },
    ],
  },
] as const
