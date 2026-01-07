import { create } from 'zustand'
import { persist } from 'zustand/middleware'
import { STORAGE_KEYS } from '../config/constants'

export type TransactionStatus = 'pending' | 'success' | 'failed'

export interface PendingTransaction {
  hash: string
  type: 'mint' | 'burn' | 'swap' | 'stake' | 'unstake' | 'leverage' | 'lend' | 'approve'
  description: string
  timestamp: number
  chainId: number
}

interface TransactionState {
  pendingTransactions: PendingTransaction[]
  addTransaction: (tx: Omit<PendingTransaction, 'timestamp'>) => void
  removeTransaction: (hash: string) => void
  clearTransactions: () => void
}

export const useTransactionStore = create<TransactionState>()(
  persist(
    (set) => ({
      pendingTransactions: [],

      addTransaction: (tx) =>
        set((state) => ({
          pendingTransactions: [
            ...state.pendingTransactions,
            { ...tx, timestamp: Date.now() },
          ],
        })),

      removeTransaction: (hash) =>
        set((state) => ({
          pendingTransactions: state.pendingTransactions.filter(
            (tx) => tx.hash !== hash
          ),
        })),

      clearTransactions: () => set({ pendingTransactions: [] }),
    }),
    {
      name: STORAGE_KEYS.PENDING_TXS,
    }
  )
)
