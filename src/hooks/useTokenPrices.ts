import { useAccount, useReadContract } from 'wagmi'
import { PLETH_CORE_ABI, BASKET_ORACLE_ABI } from '../contracts/abis'
import { getAddresses } from '../contracts/addresses'

export function useTokenPrices() {
  const { chainId } = useAccount()
  const addresses = chainId ? getAddresses(chainId) : null

  const { data: roundData, isLoading: priceLoading } = useReadContract({
    address: addresses?.BASKET_ORACLE,
    abi: BASKET_ORACLE_ABI,
    functionName: 'latestRoundData',
    query: {
      enabled: !!addresses,
    },
  })

  const { data: cap, isLoading: capLoading } = useReadContract({
    address: addresses?.SYNTHETIC_SPLITTER,
    abi: PLETH_CORE_ABI,
    functionName: 'CAP',
    query: {
      enabled: !!addresses,
    },
  })

  const oraclePrice = roundData?.[1] ?? 0n
  const capValue = cap ?? 0n

  // Basket oracle returns BEAR price directly (foreign currency basket value)
  // BULL price = CAP - BEAR (inverse exposure)
  const bearPrice = oraclePrice > 0n ? oraclePrice : 0n
  const bullPrice = capValue > oraclePrice ? capValue - oraclePrice : 0n

  return {
    bearPrice,  // 8 decimals
    bullPrice,  // 8 decimals
    oraclePrice,
    cap: capValue,
    isLoading: priceLoading || capLoading,
  }
}
