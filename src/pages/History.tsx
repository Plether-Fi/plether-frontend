import { useState } from 'react'
import { useAccount } from 'wagmi'
import { Card, Button, Badge } from '../components/ui'
import { formatAmount, formatDate } from '../utils/formatters'
import type { HistoricalTransaction, TransactionType } from '../types'

// Mock transaction data
const mockTransactions: HistoricalTransaction[] = [
  {
    id: '1',
    hash: '0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef',
    type: 'swap_buy_bear',
    timestamp: Date.now() / 1000 - 3600,
    amount: 1000n * 10n ** 6n,
    tokenSymbol: 'USDC',
    status: 'success',
  },
  {
    id: '2',
    hash: '0xabcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890',
    type: 'stake_bear',
    timestamp: Date.now() / 1000 - 7200,
    amount: 500n * 10n ** 18n,
    tokenSymbol: 'DXY-BEAR',
    status: 'success',
  },
  {
    id: '3',
    hash: '0x567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234',
    type: 'leverage_open',
    timestamp: Date.now() / 1000 - 86400,
    amount: 2000n * 10n ** 6n,
    tokenSymbol: 'USDC',
    status: 'success',
  },
  {
    id: '4',
    hash: '0x890abcdef1234567890abcdef1234567890abcdef1234567890abcdef123456',
    type: 'mint',
    timestamp: Date.now() / 1000 - 172800,
    amount: 5000n * 10n ** 6n,
    tokenSymbol: 'USDC',
    status: 'success',
  },
]

const filterOptions = [
  { id: 'all', label: 'All' },
  { id: 'mint', label: 'Mint/Burn' },
  { id: 'swap', label: 'Swaps' },
  { id: 'stake', label: 'Staking' },
  { id: 'leverage', label: 'Leverage' },
  { id: 'morpho', label: 'Lending' },
]

export function History() {
  const { isConnected } = useAccount()
  const [filter, setFilter] = useState('all')

  // TODO: Replace with actual hook using TheGraph
  const transactions = mockTransactions

  const filteredTransactions = transactions.filter((tx) => {
    if (filter === 'all') return true
    if (filter === 'mint') return tx.type === 'mint' || tx.type === 'burn'
    if (filter === 'swap') return tx.type.startsWith('swap_')
    if (filter === 'stake') return tx.type.startsWith('stake_') || tx.type.startsWith('unstake_')
    if (filter === 'leverage') return tx.type.startsWith('leverage_')
    if (filter === 'morpho') return tx.type.startsWith('morpho_')
    return true
  })

  return (
    <div className="space-y-6">
      <div>
        <h1 className="text-2xl font-bold text-white">Transaction History</h1>
        <p className="text-gray-400">View your past transactions</p>
      </div>

      {/* Filters */}
      <div className="flex flex-wrap gap-2">
        {filterOptions.map((option) => (
          <Button
            key={option.id}
            variant={filter === option.id ? 'primary' : 'secondary'}
            size="sm"
            onClick={() => setFilter(option.id)}
          >
            {option.label}
          </Button>
        ))}
      </div>

      {/* Transaction list */}
      {isConnected ? (
        filteredTransactions.length > 0 ? (
          <Card padding="none">
            <div className="divide-y divide-gray-800">
              {filteredTransactions.map((tx) => (
                <TransactionRow key={tx.id} transaction={tx} />
              ))}
            </div>
          </Card>
        ) : (
          <Card className="text-center py-12">
            <p className="text-gray-400">No transactions found</p>
          </Card>
        )
      ) : (
        <Card className="text-center py-12">
          <p className="text-gray-400">Connect your wallet to view transaction history</p>
        </Card>
      )}
    </div>
  )
}

interface TransactionRowProps {
  transaction: HistoricalTransaction
}

function TransactionRow({ transaction }: TransactionRowProps) {
  const typeLabels: Record<TransactionType, string> = {
    mint: 'Mint Pairs',
    burn: 'Burn Pairs',
    swap_buy_bear: 'Buy DXY-BEAR',
    swap_sell_bear: 'Sell DXY-BEAR',
    swap_buy_bull: 'Buy DXY-BULL',
    swap_sell_bull: 'Sell DXY-BULL',
    stake_bear: 'Stake DXY-BEAR',
    stake_bull: 'Stake DXY-BULL',
    unstake_bear: 'Unstake DXY-BEAR',
    unstake_bull: 'Unstake DXY-BULL',
    leverage_open: 'Open Leverage',
    leverage_close: 'Close Leverage',
    leverage_adjust: 'Adjust Leverage',
    morpho_supply: 'Supply to Morpho',
    morpho_withdraw: 'Withdraw from Morpho',
    morpho_borrow: 'Borrow from Morpho',
    morpho_repay: 'Repay Morpho',
  }

  const getTypeColor = (type: TransactionType): string => {
    if (type.includes('bear')) return 'text-bear'
    if (type.includes('bull')) return 'text-bull'
    if (type === 'mint') return 'text-primary-500'
    if (type === 'burn') return 'text-yellow-500'
    if (type.includes('leverage')) return 'text-blue-500'
    if (type.includes('morpho')) return 'text-purple-500'
    return 'text-white'
  }

  const truncatedHash = `${transaction.hash.slice(0, 10)}...${transaction.hash.slice(-8)}`
  const decimals = transaction.tokenSymbol === 'USDC' ? 6 : 18

  return (
    <div className="flex items-center justify-between px-4 py-3 hover:bg-surface-50/50 transition-colors">
      <div className="flex items-center gap-4">
        <div>
          <p className={`font-medium ${getTypeColor(transaction.type)}`}>
            {typeLabels[transaction.type]}
          </p>
          <p className="text-sm text-gray-500">
            {formatDate(transaction.timestamp)}
          </p>
        </div>
      </div>

      <div className="text-right">
        <p className="text-white">
          {formatAmount(transaction.amount, decimals)} {transaction.tokenSymbol}
        </p>
        <a
          href={`https://etherscan.io/tx/${transaction.hash}`}
          target="_blank"
          rel="noopener noreferrer"
          className="text-sm text-primary-500 hover:text-primary-400"
        >
          {truncatedHash} ↗
        </a>
      </div>

      <Badge variant={transaction.status === 'success' ? 'success' : 'danger'}>
        {transaction.status}
      </Badge>
    </div>
  )
}
