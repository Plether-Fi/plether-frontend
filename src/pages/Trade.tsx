import { useState } from 'react'
import { useAccount } from 'wagmi'
import { Card, Button, Tabs } from '../components/ui'
import { TokenInput } from '../components/TokenInput'
import { SlippageSelector } from '../components/SlippageSelector'

type TradeMode = 'buy' | 'sell'
type TokenSide = 'BEAR' | 'BULL'

export function Trade() {
  const { isConnected } = useAccount()
  const [mode, setMode] = useState<TradeMode>('buy')
  const [selectedToken, setSelectedToken] = useState<TokenSide>('BEAR')
  const [inputAmount, setInputAmount] = useState('')
  const [showDetails, setShowDetails] = useState(false)

  // Mock balances - replace with actual hooks
  const usdcBalance = 10000n * 10n ** 6n
  const bearBalance = 500n * 10n ** 18n
  const bullBalance = 500n * 10n ** 18n

  const inputToken = mode === 'buy'
    ? { symbol: 'USDC', decimals: 6 }
    : { symbol: `DXY-${selectedToken}`, decimals: 18 }

  const outputToken = mode === 'buy'
    ? { symbol: `DXY-${selectedToken}`, decimals: 18 }
    : { symbol: 'USDC', decimals: 6 }

  const inputBalance = mode === 'buy'
    ? usdcBalance
    : selectedToken === 'BEAR' ? bearBalance : bullBalance

  // Mock output calculation
  const outputAmount = inputAmount ? (parseFloat(inputAmount) * 0.98).toFixed(4) : '0'

  const handleSwap = async () => {
    // TODO: Implement swap logic
    console.log('Swap:', { mode, selectedToken, inputAmount })
  }

  return (
    <div className="space-y-6 max-w-lg mx-auto">
      <div>
        <h1 className="text-2xl font-bold text-white">Trade</h1>
        <p className="text-gray-400">Buy or sell DXY-BEAR and DXY-BULL</p>
      </div>

      <Card>
        {/* Buy/Sell tabs */}
        <Tabs
          tabs={[
            { id: 'buy', label: 'Buy' },
            { id: 'sell', label: 'Sell' },
          ]}
          activeTab={mode}
          onChange={(id) => setMode(id as TradeMode)}
        />

        <div className="mt-6 space-y-4">
          {/* Token selector */}
          <div>
            <label className="block text-sm text-gray-400 mb-2">Select Token</label>
            <div className="flex gap-2">
              <button
                onClick={() => setSelectedToken('BEAR')}
                className={`flex-1 px-4 py-3 rounded-lg border transition-colors ${
                  selectedToken === 'BEAR'
                    ? 'bg-bear/10 border-bear text-bear'
                    : 'bg-surface-200 border-gray-700 text-gray-300 hover:border-gray-600'
                }`}
              >
                <span className="font-medium">DXY-BEAR</span>
                <span className="block text-xs opacity-70">Bearish on USD</span>
              </button>
              <button
                onClick={() => setSelectedToken('BULL')}
                className={`flex-1 px-4 py-3 rounded-lg border transition-colors ${
                  selectedToken === 'BULL'
                    ? 'bg-bull/10 border-bull text-bull'
                    : 'bg-surface-200 border-gray-700 text-gray-300 hover:border-gray-600'
                }`}
              >
                <span className="font-medium">DXY-BULL</span>
                <span className="block text-xs opacity-70">Bullish on USD</span>
              </button>
            </div>
          </div>

          {/* Input amount */}
          <TokenInput
            label={mode === 'buy' ? 'You pay' : 'You sell'}
            value={inputAmount}
            onChange={setInputAmount}
            token={inputToken}
            balance={isConnected ? inputBalance : undefined}
          />

          {/* Arrow divider */}
          <div className="flex justify-center">
            <div className="w-10 h-10 rounded-full bg-surface-200 flex items-center justify-center">
              <svg className="w-5 h-5 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M19 14l-7 7m0 0l-7-7m7 7V3" />
              </svg>
            </div>
          </div>

          {/* Output display */}
          <div className="bg-surface-200 rounded-lg p-4">
            <div className="flex justify-between items-center">
              <span className="text-sm text-gray-400">
                {mode === 'buy' ? 'You receive' : 'You receive'}
              </span>
              <span className="text-gray-400">{outputToken.symbol}</span>
            </div>
            <p className="text-2xl font-semibold text-white mt-1">
              {outputAmount}
            </p>
          </div>

          {/* Slippage settings */}
          <div className="flex justify-end">
            <SlippageSelector />
          </div>

          {/* Swap details (collapsible) */}
          <button
            onClick={() => setShowDetails(!showDetails)}
            className="w-full flex items-center justify-between text-sm text-gray-400 hover:text-white transition-colors"
          >
            <span>Swap details</span>
            <svg
              className={`w-4 h-4 transition-transform ${showDetails ? 'rotate-180' : ''}`}
              fill="none"
              stroke="currentColor"
              viewBox="0 0 24 24"
            >
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M19 9l-7 7-7-7" />
            </svg>
          </button>

          {showDetails && (
            <div className="bg-surface-200 rounded-lg p-3 space-y-2 text-sm">
              <div className="flex justify-between">
                <span className="text-gray-400">Route</span>
                <span className="text-white">
                  {selectedToken === 'BEAR'
                    ? 'USDC → Curve → DXY-BEAR'
                    : 'USDC → ZapRouter → DXY-BULL'}
                </span>
              </div>
              <div className="flex justify-between">
                <span className="text-gray-400">Price Impact</span>
                <span className="text-white">~0.1%</span>
              </div>
              <div className="flex justify-between">
                <span className="text-gray-400">Estimated Gas</span>
                <span className="text-white">~$2.50</span>
              </div>
            </div>
          )}

          {/* Action button */}
          {isConnected ? (
            <Button
              variant="primary"
              className="w-full"
              disabled={!inputAmount || parseFloat(inputAmount) <= 0}
              onClick={handleSwap}
            >
              {mode === 'buy' ? 'Buy' : 'Sell'} DXY-{selectedToken}
            </Button>
          ) : (
            <Button variant="secondary" className="w-full" disabled>
              Connect Wallet to Trade
            </Button>
          )}
        </div>
      </Card>
    </div>
  )
}
