import type { Meta, StoryObj } from '@storybook/react-vite'

const meta: Meta = {
  title: 'Components/TransactionRow',
  tags: ['autodocs'],
}

export default meta
type Story = StoryObj

type TransactionType =
  | 'mint' | 'burn'
  | 'swap_buy_bear' | 'swap_sell_bear'
  | 'swap_buy_bull' | 'swap_sell_bull'
  | 'stake_bear' | 'stake_bull'
  | 'unstake_bear' | 'unstake_bull'
  | 'leverage_open' | 'leverage_close' | 'leverage_adjust'
  | 'morpho_supply' | 'morpho_withdraw' | 'morpho_borrow' | 'morpho_repay'

interface HistoricalTransaction {
  hash: string
  type: TransactionType
  amount: bigint
  tokenSymbol: string
  timestamp: Date
}

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

const typeIcons: Record<TransactionType, string> = {
  mint: 'add_circle',
  burn: 'remove_circle',
  swap_buy_bear: 'shopping_cart',
  swap_sell_bear: 'sell',
  swap_buy_bull: 'shopping_cart',
  swap_sell_bull: 'sell',
  stake_bear: 'lock',
  stake_bull: 'lock',
  unstake_bear: 'lock_open',
  unstake_bull: 'lock_open',
  leverage_open: 'trending_up',
  leverage_close: 'trending_down',
  leverage_adjust: 'tune',
  morpho_supply: 'savings',
  morpho_withdraw: 'output',
  morpho_borrow: 'request_quote',
  morpho_repay: 'paid',
}

function getTypeColor(type: TransactionType): string {
  if (type.includes('bear')) return 'text-cyber-electric-fuchsia'
  if (type.includes('bull')) return 'text-cyber-neon-green'
  if (type === 'mint') return 'text-cyber-neon-green'
  if (type === 'burn') return 'text-cyber-warning-text'
  if (type.includes('leverage')) return 'text-cyber-bright-blue'
  if (type.includes('morpho')) return 'text-cyber-electric-fuchsia'
  return 'text-cyber-text-primary'
}

function getIconBg(type: TransactionType): string {
  if (type.includes('bear')) return 'bg-cyber-electric-fuchsia/20'
  if (type.includes('bull')) return 'bg-cyber-neon-green/20'
  if (type === 'mint') return 'bg-cyber-neon-green/20'
  if (type === 'burn') return 'bg-cyber-warning-bg'
  if (type.includes('leverage')) return 'bg-cyber-bright-blue/20'
  if (type.includes('morpho')) return 'bg-cyber-electric-fuchsia/20'
  return 'bg-cyber-surface-light'
}

function TransactionRow({ transaction }: { transaction: HistoricalTransaction }) {
  const truncatedHash = `${transaction.hash.slice(0, 10)}...${transaction.hash.slice(-8)}`
  const decimals = transaction.tokenSymbol === 'USDC' ? 6 : 18
  const amount = (Number(transaction.amount) / 10 ** decimals).toFixed(decimals === 6 ? 2 : 4)

  return (
    <div className="flex items-center justify-between px-6 py-4 hover:bg-cyber-surface-light/50 transition-colors">
      <div className="flex items-center gap-4">
        <div className={`w-10 h-10 ${getIconBg(transaction.type)} flex items-center justify-center`}>
          <span className={`material-symbols-outlined ${getTypeColor(transaction.type)}`}>
            {typeIcons[transaction.type]}
          </span>
        </div>
        <div>
          <p className={`font-semibold ${getTypeColor(transaction.type)}`}>
            {typeLabels[transaction.type]}
          </p>
          <p className="text-xs text-cyber-text-secondary">
            {transaction.timestamp.toLocaleDateString()} {transaction.timestamp.toLocaleTimeString()}
          </p>
        </div>
      </div>
      <div className="text-right">
        <p className="font-medium text-cyber-text-primary">
          {amount} {transaction.tokenSymbol}
        </p>
        <a
          href={`https://etherscan.io/tx/${transaction.hash}`}
          target="_blank"
          rel="noopener noreferrer"
          className="text-xs text-cyber-bright-blue hover:underline"
        >
          {truncatedHash}
        </a>
      </div>
    </div>
  )
}

const mockTransactions: HistoricalTransaction[] = [
  {
    hash: '0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef',
    type: 'swap_buy_bear',
    amount: BigInt(1000 * 1e18),
    tokenSymbol: 'DXY-BEAR',
    timestamp: new Date('2024-01-15T10:30:00'),
  },
  {
    hash: '0xabcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890',
    type: 'swap_buy_bull',
    amount: BigInt(500 * 1e18),
    tokenSymbol: 'DXY-BULL',
    timestamp: new Date('2024-01-14T15:45:00'),
  },
  {
    hash: '0x9876543210fedcba9876543210fedcba9876543210fedcba9876543210fedcba',
    type: 'stake_bear',
    amount: BigInt(250 * 1e18),
    tokenSymbol: 'DXY-BEAR',
    timestamp: new Date('2024-01-13T09:00:00'),
  },
  {
    hash: '0xfedcba9876543210fedcba9876543210fedcba9876543210fedcba9876543210',
    type: 'leverage_open',
    amount: BigInt(5000 * 1e6),
    tokenSymbol: 'USDC',
    timestamp: new Date('2024-01-12T14:20:00'),
  },
  {
    hash: '0x5555555555555555555555555555555555555555555555555555555555555555',
    type: 'mint',
    amount: BigInt(10000 * 1e6),
    tokenSymbol: 'USDC',
    timestamp: new Date('2024-01-11T11:00:00'),
  },
]

export const BuyBear: Story = {
  render: () => (
    <div className="bg-cyber-surface-dark border border-cyber-border-glow/30">
      <TransactionRow transaction={mockTransactions[0]} />
    </div>
  ),
}

export const BuyBull: Story = {
  render: () => (
    <div className="bg-cyber-surface-dark border border-cyber-border-glow/30">
      <TransactionRow transaction={mockTransactions[1]} />
    </div>
  ),
}

export const StakeBear: Story = {
  render: () => (
    <div className="bg-cyber-surface-dark border border-cyber-border-glow/30">
      <TransactionRow transaction={mockTransactions[2]} />
    </div>
  ),
}

export const LeverageOpen: Story = {
  render: () => (
    <div className="bg-cyber-surface-dark border border-cyber-border-glow/30">
      <TransactionRow transaction={mockTransactions[3]} />
    </div>
  ),
}

export const Mint: Story = {
  render: () => (
    <div className="bg-cyber-surface-dark border border-cyber-border-glow/30">
      <TransactionRow transaction={mockTransactions[4]} />
    </div>
  ),
}

export const TransactionList: Story = {
  render: () => (
    <div className="bg-cyber-surface-dark border border-cyber-border-glow/30 divide-y divide-cyber-border-glow/30">
      {mockTransactions.map((tx) => (
        <TransactionRow key={tx.hash} transaction={tx} />
      ))}
    </div>
  ),
}
