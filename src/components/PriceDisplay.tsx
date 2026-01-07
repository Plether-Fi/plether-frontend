import { Badge, Skeleton } from './ui'
import type { ProtocolStatus } from '../config/constants'

interface PriceDisplayProps {
  price?: bigint
  status?: ProtocolStatus
  isLoading?: boolean
  variant?: 'compact' | 'detailed'
}

// Mock data - replace with actual hook
const mockPrice = 103.45
const mockStatus: ProtocolStatus = 'Active'

export function PriceDisplay({
  isLoading = false,
  variant = 'compact',
}: PriceDisplayProps) {
  // TODO: Replace with actual useDXYPrice() and useProtocolStatus() hooks
  const price = mockPrice
  const status = mockStatus

  const statusVariant = {
    Active: 'success',
    Paused: 'warning',
    Settled: 'info',
  } as const

  if (variant === 'compact') {
    return (
      <div className="flex items-center gap-3">
        <div className="flex items-center gap-2">
          <span className="text-sm text-gray-400">DXY</span>
          {isLoading ? (
            <Skeleton width={60} height={20} />
          ) : (
            <span className="text-white font-semibold">${price.toFixed(2)}</span>
          )}
        </div>
        <Badge variant={statusVariant[status]}>{status}</Badge>
      </div>
    )
  }

  // Detailed variant for dashboard
  return (
    <div className="bg-surface-100 rounded-xl border border-gray-800 p-4">
      <div className="flex items-center justify-between mb-3">
        <h3 className="text-gray-400 text-sm">DXY Index Price</h3>
        <Badge variant={statusVariant[status]}>{status}</Badge>
      </div>
      {isLoading ? (
        <Skeleton width={120} height={36} />
      ) : (
        <div className="flex items-baseline gap-2">
          <span className="text-3xl font-bold text-white">${price.toFixed(2)}</span>
          {/* TODO: Add 24h change */}
          {/* <span className="text-green-500 text-sm">+0.5%</span> */}
        </div>
      )}
      <p className="text-xs text-gray-500 mt-2">
        Updated from BasketOracle
      </p>
    </div>
  )
}
