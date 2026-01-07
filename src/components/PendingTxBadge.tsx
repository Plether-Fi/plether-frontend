import { useTransactionStore } from '../stores/transactionStore'
import { Spinner } from './ui'

export function PendingTxBadge() {
  const { pendingTransactions } = useTransactionStore()
  const count = pendingTransactions.length

  if (count === 0) return null

  return (
    <div className="flex items-center gap-2 px-3 py-1.5 bg-yellow-900/30 border border-yellow-800 rounded-full">
      <Spinner size="sm" className="text-yellow-500" />
      <span className="text-sm text-yellow-500 font-medium">
        {count} pending
      </span>
    </div>
  )
}
