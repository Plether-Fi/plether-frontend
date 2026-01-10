import { parseUnits } from 'viem'

export const USDC_DECIMALS = 6
export const PAIR_DECIMALS = 18

export function calculatePairAmount(inputAmount: string, mode: 'mint' | 'redeem'): bigint {
  if (!inputAmount || isNaN(parseFloat(inputAmount))) return 0n
  try {
    if (mode === 'mint') {
      const pairAmount = parseFloat(inputAmount) / 2
      return parseUnits(pairAmount.toString(), PAIR_DECIMALS)
    } else {
      return parseUnits(inputAmount, PAIR_DECIMALS)
    }
  } catch {
    return 0n
  }
}

export function calculateUsdcAmount(inputAmount: string): bigint {
  if (!inputAmount || isNaN(parseFloat(inputAmount))) return 0n
  try {
    return parseUnits(inputAmount, USDC_DECIMALS)
  } catch {
    return 0n
  }
}

export function calculateOutputDisplay(inputAmount: string, mode: 'mint' | 'redeem'): string {
  const inputNum = parseFloat(inputAmount) || 0
  if (mode === 'mint') {
    return (inputNum / 2).toFixed(4)
  } else {
    return (inputNum * 2).toFixed(2)
  }
}

export function getMinBalance(bearBalance: bigint, bullBalance: bigint): bigint {
  return bearBalance < bullBalance ? bearBalance : bullBalance
}
