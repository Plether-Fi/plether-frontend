import { useState } from 'react'
import { useAccount } from 'wagmi'
import { Card, CardHeader, Button, InfoTooltip } from '../components/ui'
import { TokenInput } from '../components/TokenInput'
import { SlippageSelector } from '../components/SlippageSelector'
import { formatUsd } from '../utils/formatters'
import { Link } from 'react-router-dom'

type PositionSide = 'BEAR' | 'BULL'

export function Leverage() {
  const { isConnected } = useAccount()
  const [selectedSide, setSelectedSide] = useState<PositionSide>('BEAR')
  const [collateralAmount, setCollateralAmount] = useState('')
  const [leverage, setLeverage] = useState(2)

  // Mock balance
  const usdcBalance = 10000n * 10n ** 6n

  // Calculate preview values
  const collateralNum = parseFloat(collateralAmount) || 0
  const positionSize = collateralNum * leverage
  const liquidationPrice = collateralNum > 0 ? (103.45 * (1 - 1 / leverage)).toFixed(2) : '0.00'

  const handleOpenPosition = async () => {
    console.log('Open position:', { selectedSide, collateralAmount, leverage })
  }

  return (
    <div className="space-y-6 max-w-lg mx-auto">
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-2xl font-bold text-white">Leverage Trading</h1>
          <p className="text-gray-400">Open leveraged positions</p>
        </div>
        <Link to="/positions">
          <Button variant="secondary" size="sm">
            View Positions
          </Button>
        </Link>
      </div>

      <Card>
        <CardHeader title="Open Position" />

        <div className="space-y-6">
          {/* Side selector */}
          <div>
            <label className="block text-sm text-gray-400 mb-2">Position Side</label>
            <div className="flex gap-2">
              <button
                onClick={() => setSelectedSide('BEAR')}
                className={`flex-1 px-4 py-3 rounded-lg border transition-colors ${
                  selectedSide === 'BEAR'
                    ? 'bg-bear/10 border-bear text-bear'
                    : 'bg-surface-200 border-gray-700 text-gray-300 hover:border-gray-600'
                }`}
              >
                <span className="font-medium">BEAR</span>
                <span className="block text-xs opacity-70">Short USD</span>
              </button>
              <button
                onClick={() => setSelectedSide('BULL')}
                className={`flex-1 px-4 py-3 rounded-lg border transition-colors ${
                  selectedSide === 'BULL'
                    ? 'bg-bull/10 border-bull text-bull'
                    : 'bg-surface-200 border-gray-700 text-gray-300 hover:border-gray-600'
                }`}
              >
                <span className="font-medium">BULL</span>
                <span className="block text-xs opacity-70">Long USD</span>
              </button>
            </div>
          </div>

          {/* Collateral input */}
          <TokenInput
            label="Collateral (USDC)"
            value={collateralAmount}
            onChange={setCollateralAmount}
            token={{ symbol: 'USDC', decimals: 6 }}
            balance={isConnected ? usdcBalance : undefined}
          />

          {/* Leverage slider */}
          <div>
            <div className="flex items-center justify-between mb-2">
              <label className="text-sm text-gray-400 flex items-center gap-1">
                Leverage
                <InfoTooltip content="Higher leverage increases both potential profits and liquidation risk" />
              </label>
              <span className="text-white font-medium">{leverage}x</span>
            </div>
            <input
              type="range"
              min="1.1"
              max="5"
              step="0.1"
              value={leverage}
              onChange={(e) => setLeverage(parseFloat(e.target.value))}
              className="w-full h-2 bg-surface-200 rounded-lg appearance-none cursor-pointer accent-primary-500"
            />
            <div className="flex justify-between text-xs text-gray-500 mt-1">
              <span>1.1x</span>
              <span>5x</span>
            </div>
          </div>

          {/* Position preview */}
          <div className="bg-surface-200 rounded-lg p-4 space-y-3">
            <h4 className="text-sm font-medium text-gray-400">Position Preview</h4>
            <div className="flex justify-between">
              <span className="text-gray-400 text-sm">Position Size</span>
              <span className="text-white">{formatUsd(BigInt(Math.floor(positionSize * 1e6)))}</span>
            </div>
            <div className="flex justify-between">
              <span className="text-gray-400 text-sm">Collateral</span>
              <span className="text-white">{formatUsd(BigInt(Math.floor(collateralNum * 1e6)))}</span>
            </div>
            <div className="flex justify-between">
              <span className="text-gray-400 text-sm flex items-center gap-1">
                Liquidation Price
                <InfoTooltip content="If DXY reaches this price, your position will be liquidated" />
              </span>
              <span className="text-yellow-500">${liquidationPrice}</span>
            </div>
          </div>

          {/* Slippage */}
          <div className="flex justify-end">
            <SlippageSelector />
          </div>

          {/* Action button */}
          {isConnected ? (
            <Button
              variant="primary"
              className="w-full"
              disabled={!collateralAmount || parseFloat(collateralAmount) <= 0}
              onClick={handleOpenPosition}
            >
              Open {selectedSide} Position
            </Button>
          ) : (
            <Button variant="secondary" className="w-full" disabled>
              Connect Wallet to Trade
            </Button>
          )}

          {/* Risk warning */}
          <p className="text-xs text-gray-500 text-center">
            Leverage trading carries significant risk. You may lose your entire collateral.
          </p>
        </div>
      </Card>
    </div>
  )
}
