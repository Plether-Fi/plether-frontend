import { useState } from 'react'
import { useAccount } from 'wagmi'
import { Card, CardHeader, Button, Tabs } from '../components/ui'
import { TokenInput } from '../components/TokenInput'
import { formatUsd, formatPercent } from '../utils/formatters'

export function Lend() {
  const { isConnected } = useAccount()

  // Mock data - replace with actual hooks
  const usdcBalance = 10000n * 10n ** 6n
  const suppliedAmount = 5000n * 10n ** 6n
  const borrowedAmount = 1000n * 10n ** 6n
  const supplyApy = 3.5
  const borrowApy = 5.2
  const availableToBorrow = 3000n * 10n ** 6n

  return (
    <div className="space-y-6">
      <div>
        <h1 className="text-2xl font-bold text-white">Morpho Lending</h1>
        <p className="text-gray-400">Supply liquidity and borrow against your collateral</p>
      </div>

      {/* Overview cards */}
      <div className="grid md:grid-cols-3 gap-4">
        <Card>
          <p className="text-sm text-gray-400">Total Supplied</p>
          <p className="text-2xl font-bold text-white">{formatUsd(suppliedAmount)}</p>
          <p className="text-sm text-green-500">Earning {formatPercent(supplyApy)} APY</p>
        </Card>
        <Card>
          <p className="text-sm text-gray-400">Total Borrowed</p>
          <p className="text-2xl font-bold text-white">{formatUsd(borrowedAmount)}</p>
          <p className="text-sm text-yellow-500">Paying {formatPercent(borrowApy)} APY</p>
        </Card>
        <Card>
          <p className="text-sm text-gray-400">Available to Borrow</p>
          <p className="text-2xl font-bold text-white">{formatUsd(availableToBorrow)}</p>
          <p className="text-sm text-gray-500">Based on your collateral</p>
        </Card>
      </div>

      <div className="grid md:grid-cols-2 gap-6">
        {/* Supply Section */}
        <SupplyCard
          balance={usdcBalance}
          supplied={suppliedAmount}
          apy={supplyApy}
          isConnected={isConnected}
        />

        {/* Borrow Section */}
        <BorrowCard
          borrowed={borrowedAmount}
          available={availableToBorrow}
          apy={borrowApy}
          isConnected={isConnected}
        />
      </div>
    </div>
  )
}

interface SupplyCardProps {
  balance: bigint
  supplied: bigint
  apy: number
  isConnected: boolean
}

function SupplyCard({ balance, supplied, apy, isConnected }: SupplyCardProps) {
  const [mode, setMode] = useState<'supply' | 'withdraw'>('supply')
  const [amount, setAmount] = useState('')

  const handleSupply = async () => {
    console.log('Supply:', { amount })
  }

  const handleWithdraw = async () => {
    console.log('Withdraw:', { amount })
  }

  return (
    <Card>
      <CardHeader
        title="Supply"
        subtitle={`Earn ${formatPercent(apy)} APY on your USDC`}
      />

      <div className="bg-green-900/20 border border-green-800 rounded-lg p-3 mb-4">
        <div className="flex justify-between items-center">
          <span className="text-gray-400 text-sm">Currently Supplied</span>
          <span className="text-white font-medium">{formatUsd(supplied)}</span>
        </div>
      </div>

      <Tabs
        tabs={[
          { id: 'supply', label: 'Supply' },
          { id: 'withdraw', label: 'Withdraw' },
        ]}
        activeTab={mode}
        onChange={(id) => {
          setMode(id as 'supply' | 'withdraw')
          setAmount('')
        }}
      />

      <div className="mt-4 space-y-4">
        <TokenInput
          value={amount}
          onChange={setAmount}
          token={{ symbol: 'USDC', decimals: 6 }}
          balance={isConnected ? (mode === 'supply' ? balance : supplied) : undefined}
          label={mode === 'supply' ? 'Amount to supply' : 'Amount to withdraw'}
        />

        {isConnected ? (
          <Button
            variant="primary"
            className="w-full"
            disabled={!amount || parseFloat(amount) <= 0}
            onClick={mode === 'supply' ? handleSupply : handleWithdraw}
          >
            {mode === 'supply' ? 'Supply USDC' : 'Withdraw USDC'}
          </Button>
        ) : (
          <Button variant="secondary" className="w-full" disabled>
            Connect Wallet
          </Button>
        )}
      </div>
    </Card>
  )
}

interface BorrowCardProps {
  borrowed: bigint
  available: bigint
  apy: number
  isConnected: boolean
}

function BorrowCard({ borrowed, available, apy, isConnected }: BorrowCardProps) {
  const [mode, setMode] = useState<'borrow' | 'repay'>('borrow')
  const [amount, setAmount] = useState('')

  const handleBorrow = async () => {
    console.log('Borrow:', { amount })
  }

  const handleRepay = async () => {
    console.log('Repay:', { amount })
  }

  return (
    <Card>
      <CardHeader
        title="Borrow"
        subtitle={`${formatPercent(apy)} APY on borrowed USDC`}
      />

      <div className="bg-yellow-900/20 border border-yellow-800 rounded-lg p-3 mb-4">
        <div className="flex justify-between items-center">
          <span className="text-gray-400 text-sm">Current Debt</span>
          <span className="text-white font-medium">{formatUsd(borrowed)}</span>
        </div>
      </div>

      <Tabs
        tabs={[
          { id: 'borrow', label: 'Borrow' },
          { id: 'repay', label: 'Repay' },
        ]}
        activeTab={mode}
        onChange={(id) => {
          setMode(id as 'borrow' | 'repay')
          setAmount('')
        }}
      />

      <div className="mt-4 space-y-4">
        <TokenInput
          value={amount}
          onChange={setAmount}
          token={{ symbol: 'USDC', decimals: 6 }}
          balance={isConnected ? (mode === 'borrow' ? available : borrowed) : undefined}
          label={mode === 'borrow' ? 'Amount to borrow' : 'Amount to repay'}
        />

        {mode === 'borrow' && (
          <p className="text-xs text-gray-500">
            Borrowing requires staked collateral (sDXY-BEAR or sDXY-BULL)
          </p>
        )}

        {isConnected ? (
          <Button
            variant="primary"
            className="w-full"
            disabled={!amount || parseFloat(amount) <= 0}
            onClick={mode === 'borrow' ? handleBorrow : handleRepay}
          >
            {mode === 'borrow' ? 'Borrow USDC' : 'Repay USDC'}
          </Button>
        ) : (
          <Button variant="secondary" className="w-full" disabled>
            Connect Wallet
          </Button>
        )}
      </div>
    </Card>
  )
}
