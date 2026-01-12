import { describe, it, expect, vi, beforeEach } from 'vitest'
import { renderHook, act, waitFor } from '@testing-library/react'
import { useTransactionStore } from '../../stores/transactionStore'

const mockWriteContract = vi.fn()
const mockReset = vi.fn()

const mockUseAccount = vi.fn()
const mockUseWriteContract = vi.fn()
const mockUseWaitForTransactionReceipt = vi.fn()

vi.mock('wagmi', () => ({
  useAccount: () => mockUseAccount(),
  useWriteContract: () => mockUseWriteContract(),
  useWaitForTransactionReceipt: () => mockUseWaitForTransactionReceipt(),
}))

import { useZapSwap } from '../useTrading'

describe('useZapSwap', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    useTransactionStore.setState({ pendingTransactions: [] })

    mockUseAccount.mockReturnValue({
      chainId: 11155111,
    })

    mockUseWriteContract.mockReturnValue({
      writeContract: mockWriteContract,
      data: undefined,
      isPending: false,
      error: null,
      reset: mockReset,
    })

    mockUseWaitForTransactionReceipt.mockReturnValue({
      isLoading: false,
      isSuccess: false,
      isError: false,
    })
  })

  describe('zapSell', () => {
    it('adds a pending transaction when zapSell is called', async () => {
      const { result } = renderHook(() => useZapSwap())

      await act(async () => {
        await result.current.zapSell(
          100000000000000000000n, // 100 BULL (18 decimals)
          85000000n, // min 85 USDC out (6 decimals)
          1800000000n // deadline
        )
      })

      const { pendingTransactions } = useTransactionStore.getState()
      expect(pendingTransactions).toHaveLength(1)
      expect(pendingTransactions[0].type).toBe('swap')
      expect(pendingTransactions[0].status).toBe('pending')
      expect(pendingTransactions[0].description).toBe('Swapping DXY-BULL for USDC')
    })

    it('calls writeContract with correct arguments for zapSell', async () => {
      const { result } = renderHook(() => useZapSwap())
      const bullAmount = 100000000000000000000n
      const minUsdcOut = 85000000n
      const deadline = 1800000000n

      await act(async () => {
        await result.current.zapSell(bullAmount, minUsdcOut, deadline)
      })

      expect(mockWriteContract).toHaveBeenCalledTimes(1)
      const callArgs = mockWriteContract.mock.calls[0][0]
      expect(callArgs.functionName).toBe('zapBurn')
      expect(callArgs.args[0]).toBe(bullAmount)
      expect(callArgs.args[1]).toBe(minUsdcOut)
      expect(callArgs.args[2]).toBe(deadline)
    })

    it('updates transaction to confirming when writeContract succeeds', async () => {
      mockWriteContract.mockImplementation((_, callbacks) => {
        callbacks.onSuccess('0xzapsell123hash')
      })

      const { result } = renderHook(() => useZapSwap())

      await act(async () => {
        await result.current.zapSell(100000000000000000000n, 85000000n, 1800000000n)
      })

      const { pendingTransactions } = useTransactionStore.getState()
      expect(pendingTransactions[0].status).toBe('confirming')
      expect(pendingTransactions[0].hash).toBe('0xzapsell123hash')
    })

    it('updates transaction to failed when writeContract errors', async () => {
      mockWriteContract.mockImplementation((_, callbacks) => {
        callbacks.onError(new Error('User rejected'))
      })

      const { result } = renderHook(() => useZapSwap())

      await act(async () => {
        await result.current.zapSell(100000000000000000000n, 85000000n, 1800000000n)
      })

      const { pendingTransactions } = useTransactionStore.getState()
      expect(pendingTransactions[0].status).toBe('failed')
    })

    it('updates transaction to success when isSuccess becomes true', async () => {
      mockWriteContract.mockImplementation((_, callbacks) => {
        callbacks.onSuccess('0xzapsell123hash')
      })

      const { result, rerender } = renderHook(() => useZapSwap())

      await act(async () => {
        await result.current.zapSell(100000000000000000000n, 85000000n, 1800000000n)
      })

      expect(useTransactionStore.getState().pendingTransactions[0].status).toBe('confirming')

      mockUseWaitForTransactionReceipt.mockReturnValue({
        isLoading: false,
        isSuccess: true,
        isError: false,
      })
      rerender()

      await waitFor(() => {
        const { pendingTransactions } = useTransactionStore.getState()
        expect(pendingTransactions[0].status).toBe('success')
      })
    })

    it('handles writeContract throwing an exception', async () => {
      mockWriteContract.mockImplementation(() => {
        throw new Error('Network error')
      })

      const { result } = renderHook(() => useZapSwap())

      await act(async () => {
        await result.current.zapSell(100000000000000000000n, 85000000n, 1800000000n)
      })

      const { pendingTransactions } = useTransactionStore.getState()
      expect(pendingTransactions[0].status).toBe('failed')
    })

    it('does nothing when chainId is not available', async () => {
      mockUseAccount.mockReturnValue({
        chainId: undefined,
      })

      const { result } = renderHook(() => useZapSwap())

      await act(async () => {
        await result.current.zapSell(100000000000000000000n, 85000000n, 1800000000n)
      })

      expect(mockWriteContract).not.toHaveBeenCalled()
      const { pendingTransactions } = useTransactionStore.getState()
      expect(pendingTransactions).toHaveLength(0)
    })
  })

  describe('zapBuy', () => {
    it('calls writeContract with correct arguments for zapBuy', async () => {
      const { result } = renderHook(() => useZapSwap())
      const usdcAmount = 100000000n // 100 USDC
      const minBullOut = 95000000000000000000n // min 95 BULL
      const maxSlippageBps = 100n
      const deadline = 1800000000n

      await act(async () => {
        await result.current.zapBuy(usdcAmount, minBullOut, maxSlippageBps, deadline)
      })

      expect(mockWriteContract).toHaveBeenCalledTimes(1)
      const callArgs = mockWriteContract.mock.calls[0][0]
      expect(callArgs.functionName).toBe('zapMint')
      expect(callArgs.args[0]).toBe(usdcAmount)
      expect(callArgs.args[1]).toBe(minBullOut)
      expect(callArgs.args[2]).toBe(maxSlippageBps)
      expect(callArgs.args[3]).toBe(deadline)
    })
  })
})
