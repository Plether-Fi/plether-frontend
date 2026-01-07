import { useState } from 'react'
import { useAccount } from 'wagmi'
import { Card, Button, Tabs, TabPanel, InfoTooltip } from '../components/ui'
import { TokenInput } from '../components/TokenInput'
import { formatAmount } from '../utils/formatters'

type MintMode = 'mint' | 'burn'

export function Mint() {
  const { isConnected } = useAccount()
  const [mode, setMode] = useState<MintMode>('mint')
  const [inputAmount, setInputAmount] = useState('')

  // Mock balances
  const usdcBalance = 10000n * 10n ** 6n
  const bearBalance = 500n * 10n ** 18n
  const bullBalance = 500n * 10n ** 18n

  // Mock output calculation
  const outputAmount = inputAmount ? parseFloat(inputAmount).toFixed(4) : '0'

  const handleMint = async () => {
    console.log('Mint:', { amount: inputAmount })
  }

  const handleBurn = async () => {
    console.log('Burn:', { amount: inputAmount })
  }

  return (
    <div className="space-y-6 max-w-lg mx-auto">
      <div>
        <h1 className="text-2xl font-bold text-white">Mint & Redeem</h1>
        <p className="text-gray-400">Create or redeem DXY-BEAR + DXY-BULL pairs</p>
      </div>

      <Card>
        {/* Mint/Burn tabs */}
        <Tabs
          tabs={[
            { id: 'mint', label: 'Mint Pairs' },
            { id: 'burn', label: 'Redeem' },
          ]}
          activeTab={mode}
          onChange={(id) => {
            setMode(id as MintMode)
            setInputAmount('')
          }}
        />

        <TabPanel isActive={mode === 'mint'}>
          <div className="mt-6 space-y-4">
            {/* Info box */}
            <div className="bg-blue-900/20 border border-blue-800 rounded-lg p-4">
              <div className="flex items-start gap-2">
                <InfoTooltip content="Minting creates equal amounts of DXY-BEAR and DXY-BULL tokens from your USDC" />
                <p className="text-sm text-blue-300">
                  Mint equal amounts of DXY-BEAR and DXY-BULL from USDC.
                  You'll receive both tokens in a 1:1 ratio.
                </p>
              </div>
            </div>

            {/* Input */}
            <TokenInput
              label="USDC to deposit"
              value={inputAmount}
              onChange={setInputAmount}
              token={{ symbol: 'USDC', decimals: 6 }}
              balance={isConnected ? usdcBalance : undefined}
            />

            {/* Output preview */}
            <div className="bg-surface-200 rounded-lg p-4 space-y-3">
              <p className="text-sm text-gray-400">You will receive:</p>
              <div className="flex justify-between items-center">
                <span className="text-bear">DXY-BEAR</span>
                <span className="text-white font-medium">{outputAmount}</span>
              </div>
              <div className="flex justify-between items-center">
                <span className="text-bull">DXY-BULL</span>
                <span className="text-white font-medium">{outputAmount}</span>
              </div>
            </div>

            {/* Action button */}
            {isConnected ? (
              <Button
                variant="primary"
                className="w-full"
                disabled={!inputAmount || parseFloat(inputAmount) <= 0}
                onClick={handleMint}
              >
                Mint Pairs
              </Button>
            ) : (
              <Button variant="secondary" className="w-full" disabled>
                Connect Wallet to Mint
              </Button>
            )}
          </div>
        </TabPanel>

        <TabPanel isActive={mode === 'burn'}>
          <div className="mt-6 space-y-4">
            {/* Info box */}
            <div className="bg-yellow-900/20 border border-yellow-800 rounded-lg p-4">
              <div className="flex items-start gap-2">
                <InfoTooltip content="Redeeming burns equal amounts of DXY-BEAR and DXY-BULL to get back USDC" />
                <p className="text-sm text-yellow-300">
                  Redeem equal amounts of DXY-BEAR and DXY-BULL to get back USDC.
                  You need equal amounts of both tokens.
                </p>
              </div>
            </div>

            {/* Your balances */}
            <div className="bg-surface-200 rounded-lg p-4 space-y-2">
              <p className="text-sm text-gray-400">Your balances:</p>
              <div className="flex justify-between">
                <span className="text-bear">DXY-BEAR</span>
                <span className="text-white">{formatAmount(bearBalance, 18)}</span>
              </div>
              <div className="flex justify-between">
                <span className="text-bull">DXY-BULL</span>
                <span className="text-white">{formatAmount(bullBalance, 18)}</span>
              </div>
            </div>

            {/* Input */}
            <TokenInput
              label="Amount to redeem (of each token)"
              value={inputAmount}
              onChange={setInputAmount}
              token={{ symbol: 'PAIR', decimals: 18 }}
              balance={isConnected ? (bearBalance < bullBalance ? bearBalance : bullBalance) : undefined}
            />

            {/* Output preview */}
            <div className="bg-surface-200 rounded-lg p-4">
              <div className="flex justify-between items-center">
                <span className="text-gray-400">You will receive</span>
                <span className="text-white font-medium">{outputAmount} USDC</span>
              </div>
            </div>

            {/* Action button */}
            {isConnected ? (
              <Button
                variant="primary"
                className="w-full"
                disabled={!inputAmount || parseFloat(inputAmount) <= 0}
                onClick={handleBurn}
              >
                Redeem for USDC
              </Button>
            ) : (
              <Button variant="secondary" className="w-full" disabled>
                Connect Wallet to Redeem
              </Button>
            )}
          </div>
        </TabPanel>
      </Card>
    </div>
  )
}
