import { useState } from 'react'
import { TokenInput } from './TokenInput'

type TradeMode = 'buy' | 'sell'
type TokenSide = 'BEAR' | 'BULL'

export interface TradeCardProps {
  usdcBalance: bigint
  bearBalance: bigint
  bullBalance: bigint
}

export function TradeCard({ usdcBalance, bearBalance, bullBalance }: TradeCardProps) {
  const [mode, setMode] = useState<TradeMode>('buy')
  const [selectedToken, setSelectedToken] = useState<TokenSide>('BEAR')
  const [inputAmount, setInputAmount] = useState('')
  const [showDetails, setShowDetails] = useState(false)

  const inputToken = mode === 'buy'
    ? { symbol: 'USDC', decimals: 6 }
    : { symbol: `DXY-${selectedToken}`, decimals: 18 }

  const outputToken = mode === 'buy'
    ? { symbol: `DXY-${selectedToken}`, decimals: 18 }
    : { symbol: 'USDC', decimals: 6 }

  const inputBalance = mode === 'buy'
    ? usdcBalance
    : selectedToken === 'BEAR' ? bearBalance : bullBalance

  const outputAmount = inputAmount ? (parseFloat(inputAmount) * 0.98).toFixed(4) : '0'

  const handleSwap = async () => {
    console.log('Swap:', { mode, selectedToken, inputAmount })
  }

  return (
    <div className="max-w-xl mx-auto space-y-6">
      <div className="bg-cyber-surface-light p-1 flex text-sm font-medium mb-8 border border-cyber-border-glow/30">
        <button
          onClick={() => { setMode('buy'); setInputAmount('') }}
          className={`flex-1 py-2 px-4 transition-all ${
            mode === 'buy'
              ? 'bg-cyber-surface-dark text-cyber-bright-blue shadow-sm shadow-cyber-bright-blue/10 border border-cyber-bright-blue/50'
              : 'text-cyber-text-secondary hover:text-cyber-bright-blue'
          }`}
        >
          Buy
        </button>
        <button
          onClick={() => { setMode('sell'); setInputAmount('') }}
          className={`flex-1 py-2 px-4 transition-all ${
            mode === 'sell'
              ? 'bg-cyber-surface-dark text-cyber-bright-blue shadow-sm shadow-cyber-bright-blue/10 border border-cyber-bright-blue/50'
              : 'text-cyber-text-secondary hover:text-cyber-bright-blue'
          }`}
        >
          Sell
        </button>
      </div>

      <div className="space-y-2">
        <label className="text-sm font-medium text-cyber-text-secondary">Select Token</label>
        <div className="grid grid-cols-2 gap-4">
          <button
            onClick={() => setSelectedToken('BEAR')}
            className={`relative p-4 text-center transition-all ${
              selectedToken === 'BEAR'
                ? 'border-2 border-cyber-electric-fuchsia bg-cyber-electric-fuchsia/10 shadow-md shadow-cyber-electric-fuchsia/20'
                : 'border border-cyber-border-glow/30 bg-cyber-surface-dark hover:border-cyber-electric-fuchsia/50 opacity-60 hover:opacity-100'
            }`}
          >
            <div className={`font-semibold ${selectedToken === 'BEAR' ? 'text-cyber-electric-fuchsia' : 'text-cyber-text-primary'}`}>DXY-BEAR</div>
            <div className={`text-xs mt-1 ${selectedToken === 'BEAR' ? 'text-cyber-electric-fuchsia/70' : 'text-cyber-text-secondary'}`}>Bearish on USD</div>
          </button>
          <button
            onClick={() => setSelectedToken('BULL')}
            className={`relative p-4 text-center transition-all ${
              selectedToken === 'BULL'
                ? 'border-2 border-cyber-neon-green bg-cyber-neon-green/10 shadow-md shadow-cyber-neon-green/20'
                : 'border border-cyber-border-glow/30 bg-cyber-surface-dark hover:border-cyber-neon-green/50 opacity-60 hover:opacity-100'
            }`}
          >
            <div className={`font-semibold ${selectedToken === 'BULL' ? 'text-cyber-neon-green' : 'text-cyber-text-primary'}`}>DXY-BULL</div>
            <div className={`text-xs mt-1 ${selectedToken === 'BULL' ? 'text-cyber-neon-green/70' : 'text-cyber-text-secondary'}`}>Bullish on USD</div>
          </button>
        </div>
      </div>

      <div className="space-y-4">
        <TokenInput
          label={mode === 'buy' ? 'You pay' : 'You sell'}
          value={inputAmount}
          onChange={setInputAmount}
          token={inputToken}
          balance={inputBalance}
        />

        <div className="flex justify-center -my-2 z-10 relative">
          <div className="bg-cyber-surface-light p-2 rounded-full border border-cyber-border-glow/30 shadow-sm shadow-cyber-border-glow/10">
            <span className="material-symbols-outlined text-cyber-bright-blue text-lg block">arrow_downward</span>
          </div>
        </div>

        <div className="space-y-2">
          <div className="flex justify-between text-sm">
            <span className="text-cyber-text-secondary">You receive</span>
          </div>
          <div className="relative">
            <div className="w-full bg-cyber-surface-dark border border-cyber-border-glow/30 py-4 pl-4 pr-24 text-xl font-medium text-cyber-text-primary flex items-center h-[62px] shadow-sm shadow-cyber-border-glow/10">
              {outputAmount}
            </div>
            <div className="absolute right-4 top-1/2 -translate-y-1/2 flex items-center gap-2">
              <span className="font-medium text-cyber-text-secondary">{outputToken.symbol}</span>
            </div>
          </div>
        </div>
      </div>

      <div className="flex items-center justify-end gap-2 text-xs text-cyber-text-secondary">
        <span className="material-symbols-outlined text-[14px]">settings</span>
        <span>1% slippage</span>
      </div>

      <div className="border-t border-cyber-border-glow/30 pt-4">
        <button
          onClick={() => setShowDetails(!showDetails)}
          className="w-full flex justify-between items-center text-sm text-cyber-text-secondary hover:text-cyber-bright-blue"
        >
          <span>Swap details</span>
          <span className="material-symbols-outlined text-lg">{showDetails ? 'expand_less' : 'expand_more'}</span>
        </button>
      </div>

      {showDetails && (
        <div className="bg-cyber-surface-light p-3 space-y-2 text-sm border border-cyber-border-glow/30">
          <div className="flex justify-between">
            <span className="text-cyber-text-secondary">Route</span>
            <span className="text-cyber-text-primary">
              {selectedToken === 'BEAR' ? 'USDC → Curve → DXY-BEAR' : 'USDC → ZapRouter → DXY-BULL'}
            </span>
          </div>
          <div className="flex justify-between">
            <span className="text-cyber-text-secondary">Price Impact</span>
            <span className="text-cyber-text-primary">~0.1%</span>
          </div>
          <div className="flex justify-between">
            <span className="text-cyber-text-secondary">Estimated Gas</span>
            <span className="text-cyber-text-primary">~$2.50</span>
          </div>
        </div>
      )}

      <button
        onClick={handleSwap}
        disabled={!inputAmount || parseFloat(inputAmount) <= 0}
        className="w-full bg-cyber-bright-blue hover:bg-cyber-bright-blue/90 text-cyber-bg font-semibold py-4 px-6 shadow-lg shadow-cyber-bright-blue/40 transition-all transform hover:-translate-y-0.5 active:translate-y-0 text-lg disabled:opacity-50 disabled:cursor-not-allowed disabled:transform-none disabled:shadow-none"
      >
        {mode === 'buy' ? 'Buy' : 'Sell'} DXY-{selectedToken}
      </button>
    </div>
  )
}
