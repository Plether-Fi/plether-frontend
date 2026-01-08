import type { Meta, StoryObj } from '@storybook/react-vite'
import { TokenIcon } from '../components/ui'

const meta: Meta = {
  title: 'Components/PositionCard',
  tags: ['autodocs'],
}

export default meta
type Story = StoryObj

const HEALTH_FACTOR_WARNING = 1.5
const HEALTH_FACTOR_DANGER = 1.2

function formatUsd(value: bigint): string {
  const num = Number(value) / 1e6
  return new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD' }).format(num)
}

function formatPercent(value: number): string {
  return `${value.toFixed(2)}%`
}

interface LeveragePosition {
  id: string
  side: 'BEAR' | 'BULL'
  leverage: number
  size: bigint
  collateral: bigint
  pnl: bigint
  pnlPercentage: number
  liquidationPrice: bigint
  healthFactor: number
}

interface PositionCardProps {
  position: LeveragePosition
  onAdjust?: () => void
}

function PositionCard({ position, onAdjust }: PositionCardProps) {
  const sideColor = position.side === 'BEAR' ? 'text-cyber-electric-fuchsia' : 'text-cyber-neon-green'
  const pnlColor = position.pnl >= 0n ? 'text-cyber-neon-green' : 'text-cyber-electric-fuchsia'
  const healthColor = position.healthFactor >= HEALTH_FACTOR_WARNING
    ? 'text-cyber-neon-green'
    : position.healthFactor >= HEALTH_FACTOR_DANGER
      ? 'text-cyber-warning-text'
      : 'text-cyber-electric-fuchsia'

  return (
    <div className="bg-cyber-surface-dark p-4 border border-cyber-border-glow/30 hover:border-cyber-bright-blue/50 transition-all shadow-md">
      <div className="flex flex-col md:flex-row md:items-center justify-between gap-4">
        <div className="flex items-center gap-4">
          <TokenIcon side={position.side} />
          <div>
            <div className="flex items-center gap-2">
              <span className={`font-semibold ${sideColor}`}>DXY-{position.side}</span>
              <span className="px-1.5 py-0.5 bg-cyber-surface-light text-xs text-cyber-text-secondary font-medium border border-cyber-border-glow/30">
                {position.leverage}x
              </span>
            </div>
            <div className="text-xs text-cyber-text-secondary mt-1">
              Size: {formatUsd(position.size)} | Collateral: {formatUsd(position.collateral)}
            </div>
          </div>
        </div>

        <div className="flex flex-wrap items-center gap-6 lg:gap-12 flex-1 md:justify-end">
          <div className="flex flex-col">
            <span className="text-xs text-cyber-text-secondary mb-1">PnL</span>
            <span className={`text-sm font-semibold ${pnlColor}`}>
              {formatUsd(position.pnl)} ({position.pnlPercentage > 0 ? '+' : ''}{formatPercent(position.pnlPercentage)})
            </span>
          </div>
          <div className="flex flex-col">
            <span className="text-xs text-cyber-text-secondary mb-1">Liq. Price</span>
            <span className="text-sm font-semibold text-cyber-text-primary">
              ${(Number(position.liquidationPrice) / 1e6).toFixed(2)}
            </span>
          </div>
          <div className="flex flex-col">
            <div className="flex items-center gap-1 text-xs text-cyber-text-secondary mb-1">
              Health
              <span className="material-symbols-outlined text-[10px] text-cyber-text-secondary">help</span>
            </div>
            <span className={`text-sm font-semibold ${healthColor}`}>
              {position.healthFactor.toFixed(2)}
            </span>
          </div>

          <div className="flex items-center gap-2 mt-2 md:mt-0">
            <button
              onClick={onAdjust}
              className="px-3 py-1.5 text-sm border border-cyber-border-glow/30 text-cyber-text-secondary hover:bg-cyber-surface-light hover:text-cyber-bright-blue transition-colors"
            >
              Adjust
            </button>
            <button className="px-3 py-1.5 text-sm bg-cyber-electric-fuchsia hover:bg-cyber-electric-fuchsia/80 text-cyber-text-primary transition-colors shadow-md shadow-cyber-electric-fuchsia/20">
              Close
            </button>
          </div>
        </div>
      </div>
    </div>
  )
}

const bearPosition: LeveragePosition = {
  id: '1',
  side: 'BEAR',
  leverage: 3,
  size: BigInt(15000 * 1e6),
  collateral: BigInt(5000 * 1e6),
  pnl: BigInt(1250 * 1e6),
  pnlPercentage: 25,
  liquidationPrice: BigInt(115 * 1e6),
  healthFactor: 2.1,
}

const bullPosition: LeveragePosition = {
  id: '2',
  side: 'BULL',
  leverage: 2,
  size: BigInt(10000 * 1e6),
  collateral: BigInt(5000 * 1e6),
  pnl: BigInt(-750 * 1e6),
  pnlPercentage: -7.5,
  liquidationPrice: BigInt(95 * 1e6),
  healthFactor: 1.8,
}

const lowHealthPosition: LeveragePosition = {
  id: '3',
  side: 'BEAR',
  leverage: 5,
  size: BigInt(25000 * 1e6),
  collateral: BigInt(5000 * 1e6),
  pnl: BigInt(-2000 * 1e6),
  pnlPercentage: -40,
  liquidationPrice: BigInt(110 * 1e6),
  healthFactor: 1.15,
}

export const BearPosition: Story = {
  render: () => <PositionCard position={bearPosition} onAdjust={() => alert('Adjust clicked')} />,
}

export const BullPosition: Story = {
  render: () => <PositionCard position={bullPosition} onAdjust={() => alert('Adjust clicked')} />,
}

export const LowHealthPosition: Story = {
  render: () => <PositionCard position={lowHealthPosition} onAdjust={() => alert('Adjust clicked')} />,
}

export const AllPositions: Story = {
  render: () => (
    <div className="space-y-4">
      <div>
        <p className="text-cyber-text-secondary text-sm mb-2">Bear Position (Profitable):</p>
        <PositionCard position={bearPosition} onAdjust={() => {}} />
      </div>
      <div>
        <p className="text-cyber-text-secondary text-sm mb-2">Bull Position (Loss):</p>
        <PositionCard position={bullPosition} onAdjust={() => {}} />
      </div>
      <div>
        <p className="text-cyber-text-secondary text-sm mb-2">Low Health (Danger):</p>
        <PositionCard position={lowHealthPosition} onAdjust={() => {}} />
      </div>
    </div>
  ),
}
