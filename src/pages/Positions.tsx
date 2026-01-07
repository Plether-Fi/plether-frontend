import { useState } from 'react'
import { useAccount } from 'wagmi'
import { Card, Button, Badge, Modal, InfoTooltip } from '../components/ui'
import { TokenInput } from '../components/TokenInput'
import { formatUsd, formatPercent, getHealthFactorColor } from '../utils/formatters'
import { HEALTH_FACTOR_WARNING, HEALTH_FACTOR_DANGER } from '../config/constants'
import type { LeveragePosition } from '../types'
import { Link } from 'react-router-dom'

// Mock positions data
const mockPositions: LeveragePosition[] = [
  {
    id: '1',
    side: 'BEAR',
    size: 5000n * 10n ** 6n,
    collateral: 2500n * 10n ** 6n,
    leverage: 2,
    entryPrice: 103n * 10n ** 6n,
    liquidationPrice: 95n * 10n ** 6n,
    healthFactor: 1.8,
    pnl: 250n * 10n ** 6n,
    pnlPercentage: 10,
  },
  {
    id: '2',
    side: 'BULL',
    size: 3000n * 10n ** 6n,
    collateral: 1000n * 10n ** 6n,
    leverage: 3,
    entryPrice: 102n * 10n ** 6n,
    liquidationPrice: 110n * 10n ** 6n,
    healthFactor: 1.3,
    pnl: -100n * 10n ** 6n,
    pnlPercentage: -10,
  },
]

export function Positions() {
  const { isConnected } = useAccount()
  const [selectedPosition, setSelectedPosition] = useState<LeveragePosition | null>(null)
  const [adjustModalOpen, setAdjustModalOpen] = useState(false)

  // TODO: Replace with actual hook
  const positions = mockPositions

  // Check for low health positions
  const hasLowHealth = positions.some((p) => p.healthFactor < HEALTH_FACTOR_WARNING)

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-2xl font-bold text-white">Positions</h1>
          <p className="text-gray-400">Manage your leverage positions</p>
        </div>
        <Link to="/leverage">
          <Button variant="primary" size="sm">
            Open New Position
          </Button>
        </Link>
      </div>

      {/* Health warning banner */}
      {hasLowHealth && (
        <div className="bg-yellow-900/30 border border-yellow-800 rounded-lg p-4 flex items-center gap-3">
          <svg className="w-6 h-6 text-yellow-500 flex-shrink-0" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z" />
          </svg>
          <div>
            <p className="text-yellow-400 font-medium">Low Health Factor Warning</p>
            <p className="text-yellow-300/70 text-sm">
              One or more positions have low health factors and may be at risk of liquidation.
            </p>
          </div>
        </div>
      )}

      {/* Positions list */}
      {isConnected ? (
        positions.length > 0 ? (
          <div className="grid gap-4">
            {positions.map((position) => (
              <PositionCard
                key={position.id}
                position={position}
                onAdjust={() => {
                  setSelectedPosition(position)
                  setAdjustModalOpen(true)
                }}
              />
            ))}
          </div>
        ) : (
          <Card className="text-center py-12">
            <p className="text-gray-400 mb-4">You don't have any open positions</p>
            <Link to="/leverage">
              <Button variant="primary">Open a Position</Button>
            </Link>
          </Card>
        )
      ) : (
        <Card className="text-center py-12">
          <p className="text-gray-400">Connect your wallet to view positions</p>
        </Card>
      )}

      {/* Adjust Position Modal */}
      {selectedPosition && (
        <AdjustPositionModal
          isOpen={adjustModalOpen}
          onClose={() => {
            setAdjustModalOpen(false)
            setSelectedPosition(null)
          }}
          position={selectedPosition}
        />
      )}
    </div>
  )
}

interface PositionCardProps {
  position: LeveragePosition
  onAdjust: () => void
}

function PositionCard({ position, onAdjust }: PositionCardProps) {
  const sideColor = position.side === 'BEAR' ? 'text-bear' : 'text-bull'
  const sideBg = position.side === 'BEAR' ? 'bg-bear/10' : 'bg-bull/10'
  const pnlColor = position.pnl >= 0n ? 'text-green-500' : 'text-red-500'
  const healthColor = getHealthFactorColor(position.healthFactor)

  const isWarning = position.healthFactor < HEALTH_FACTOR_WARNING
  const isDanger = position.healthFactor < HEALTH_FACTOR_DANGER

  return (
    <Card className={isDanger ? 'border-red-800' : isWarning ? 'border-yellow-800' : ''}>
      <div className="flex flex-col md:flex-row md:items-center justify-between gap-4">
        {/* Position info */}
        <div className="flex items-center gap-4">
          <div className={`w-12 h-12 rounded-lg ${sideBg} flex items-center justify-center`}>
            <span className={`font-bold ${sideColor}`}>{position.side[0]}</span>
          </div>
          <div>
            <div className="flex items-center gap-2">
              <span className={`font-semibold ${sideColor}`}>DXY-{position.side}</span>
              <Badge variant="default">{position.leverage}x</Badge>
            </div>
            <p className="text-sm text-gray-400">
              Size: {formatUsd(position.size)} | Collateral: {formatUsd(position.collateral)}
            </p>
          </div>
        </div>

        {/* Stats */}
        <div className="flex flex-wrap gap-6 text-sm">
          <div>
            <p className="text-gray-400">PnL</p>
            <p className={`font-medium ${pnlColor}`}>
              {formatUsd(position.pnl)} ({position.pnlPercentage > 0 ? '+' : ''}{formatPercent(position.pnlPercentage)})
            </p>
          </div>
          <div>
            <p className="text-gray-400">Liq. Price</p>
            <p className="text-white">${(Number(position.liquidationPrice) / 1e6).toFixed(2)}</p>
          </div>
          <div>
            <p className="text-gray-400 flex items-center gap-1">
              Health
              <InfoTooltip content="Health factor indicates position safety. Below 1.2 is high risk." />
            </p>
            <p className={`font-medium ${healthColor}`}>{position.healthFactor.toFixed(2)}</p>
          </div>
        </div>

        {/* Actions */}
        <div className="flex gap-2">
          <Button variant="secondary" size="sm" onClick={onAdjust}>
            Adjust
          </Button>
          <Button variant="danger" size="sm">
            Close
          </Button>
        </div>
      </div>
    </Card>
  )
}

interface AdjustPositionModalProps {
  isOpen: boolean
  onClose: () => void
  position: LeveragePosition
}

function AdjustPositionModal({ isOpen, onClose, position }: AdjustPositionModalProps) {
  const [action, setAction] = useState<'add' | 'remove' | 'adjust'>('add')
  const [amount, setAmount] = useState('')

  return (
    <Modal isOpen={isOpen} onClose={onClose} title={`Adjust ${position.side} Position`}>
      <div className="space-y-4">
        {/* Action selector */}
        <div className="flex gap-2">
          <Button
            variant={action === 'add' ? 'primary' : 'secondary'}
            size="sm"
            onClick={() => setAction('add')}
          >
            Add Collateral
          </Button>
          <Button
            variant={action === 'remove' ? 'primary' : 'secondary'}
            size="sm"
            onClick={() => setAction('remove')}
          >
            Remove Collateral
          </Button>
          <Button
            variant={action === 'adjust' ? 'primary' : 'secondary'}
            size="sm"
            onClick={() => setAction('adjust')}
          >
            Adjust Leverage
          </Button>
        </div>

        {/* Input based on action */}
        {action === 'add' && (
          <TokenInput
            label="Add USDC Collateral"
            value={amount}
            onChange={setAmount}
            token={{ symbol: 'USDC', decimals: 6 }}
          />
        )}

        {action === 'remove' && (
          <TokenInput
            label="Remove USDC Collateral"
            value={amount}
            onChange={setAmount}
            token={{ symbol: 'USDC', decimals: 6 }}
          />
        )}

        {action === 'adjust' && (
          <div>
            <label className="block text-sm text-gray-400 mb-2">New Leverage</label>
            <input
              type="range"
              min="1.1"
              max="5"
              step="0.1"
              defaultValue={position.leverage}
              className="w-full"
            />
          </div>
        )}

        {/* Preview */}
        <div className="bg-surface-200 rounded-lg p-3 space-y-2 text-sm">
          <div className="flex justify-between">
            <span className="text-gray-400">New Liquidation Price</span>
            <span className="text-yellow-500">$92.50</span>
          </div>
          <div className="flex justify-between">
            <span className="text-gray-400">New Health Factor</span>
            <span className="text-green-500">2.1</span>
          </div>
        </div>

        <Button variant="primary" className="w-full">
          Confirm {action === 'add' ? 'Add' : action === 'remove' ? 'Remove' : 'Adjust'}
        </Button>
      </div>
    </Modal>
  )
}
