import { useState, useEffect, useRef, useCallback } from 'react'
import { type Address } from 'viem'
import { useAllowance } from './useAllowance'
import { useApprove } from './useApprove'

type FlowState = 'idle' | 'approving' | 'executing'

interface UseApprovalFlowOptions {
  tokenAddress: Address
  spenderAddress: Address
}

export function useApprovalFlow({
  tokenAddress,
  spenderAddress,
}: UseApprovalFlowOptions) {
  const { allowance, refetch: refetchAllowance } = useAllowance(tokenAddress, spenderAddress)
  const {
    approve,
    isPending: approvePending,
    isConfirming: approveConfirming,
    isSuccess: approveSuccess,
    error: approveError,
    reset: resetApprove,
  } = useApprove(tokenAddress, spenderAddress)

  const [flowState, setFlowState] = useState<FlowState>('idle')
  const pendingAmountRef = useRef<bigint>(0n)
  const actionRef = useRef<((amount: bigint) => Promise<void>) | null>(null)
  const approveHandledRef = useRef(false)

  useEffect(() => {
    if (approveSuccess && flowState === 'approving' && !approveHandledRef.current) {
      approveHandledRef.current = true
      void refetchAllowance()

      if (actionRef.current && pendingAmountRef.current > 0n) {
        setFlowState('executing')
        void actionRef.current(pendingAmountRef.current).finally(() => {
          pendingAmountRef.current = 0n
          actionRef.current = null
        })
      }
    }
  }, [approveSuccess, flowState, refetchAllowance])

  const execute = useCallback(async (
    amount: bigint,
    action: (amount: bigint) => Promise<void>,
  ) => {
    const needsApproval = allowance < amount

    approveHandledRef.current = false
    pendingAmountRef.current = amount
    actionRef.current = action

    if (needsApproval) {
      setFlowState('approving')
      await approve(amount)
    } else {
      setFlowState('executing')
      await action(amount)
      pendingAmountRef.current = 0n
      actionRef.current = null
    }
  }, [allowance, approve])

  const reset = useCallback(() => {
    setFlowState('idle')
    pendingAmountRef.current = 0n
    actionRef.current = null
    approveHandledRef.current = false
    resetApprove()
  }, [resetApprove])

  const checkNeedsApproval = useCallback((amount: bigint) => amount > 0n && allowance < amount, [allowance])

  return {
    execute,
    reset,
    needsApproval: checkNeedsApproval,
    flowState,
    isApproving: flowState === 'approving',
    isExecuting: flowState === 'executing',
    isPending: flowState === 'approving' || flowState === 'executing',
    approvePending,
    approveConfirming,
    approveError,
  }
}
