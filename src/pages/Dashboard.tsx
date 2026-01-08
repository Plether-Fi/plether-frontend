import { useState } from 'react'
import { useAccount } from 'wagmi'
import { Alert } from '../components/ui'
import { PortfolioCard } from '../components/PortfolioCard'
import { PositionCard } from '../components/PositionCard'
import { AdjustPositionModal } from '../components/AdjustPositionModal'
import { TradeCard } from '../components/TradeCard'
import { YieldCard } from '../components/YieldCard'
import { LeverageCard } from '../components/LeverageCard'
import { MainTabNav } from '../components/MainTabNav'
import { ConnectWalletPrompt } from '../components/ConnectWalletPrompt'
import { Link, useLocation, useNavigate } from 'react-router-dom'
import { HEALTH_FACTOR_WARNING } from '../config/constants'
import type { LeveragePosition } from '../types'

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

export function Dashboard() {
  const { isConnected } = useAccount()
  const location = useLocation()
  const navigate = useNavigate()

  type MainTab = 'trade' | 'leverage' | 'yield'

  const mainTab: MainTab =
    location.pathname === '/leverage' ? 'leverage' :
    location.pathname === '/yield' ? 'yield' : 'trade'

  const handleTabChange = (tab: MainTab) => {
    if (tab === 'leverage') navigate('/leverage')
    else if (tab === 'yield') navigate('/yield')
    else navigate('/')
  }

  const [selectedPosition, setSelectedPosition] = useState<LeveragePosition | null>(null)
  const [adjustModalOpen, setAdjustModalOpen] = useState(false)
  const positions = mockPositions
  const hasLowHealth = positions.some((p) => p.healthFactor < HEALTH_FACTOR_WARNING)

  const usdcBalance = 10000n * 10n ** 6n
  const bearBalance = 500n * 10n ** 18n
  const bullBalance = 500n * 10n ** 18n

  const isLoading = false
  const spotValue = 5000n * 10n ** 6n
  const stakedValue = 3000n * 10n ** 6n
  const leverageValue = 1500n * 10n ** 6n
  const lendingValue = 500n * 10n ** 6n

  return (
    <div className="space-y-10">
      {/* Page title */}
      <div className="mb-8">
        <h1 className="text-3xl font-semibold text-cyber-text-primary mb-1">Dashboard</h1>
        <p className="text-cyber-text-secondary font-light">Your portfolio overview</p>
      </div>

      {isConnected ? (
        <>
          {/* Portfolio breakdown */}
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-10">
            <PortfolioCard
              title="Spot Holdings"
              value={spotValue}
              description="USDC, DXY-BEAR, DXY-BULL"
              link="/"
              isLoading={isLoading}
              colorClass="text-cyber-bright-blue"
            />
            <PortfolioCard
              title="Staked"
              value={stakedValue}
              description="sDXY-BEAR, sDXY-BULL"
              link="/stake"
              isLoading={isLoading}
              colorClass="text-cyber-bright-blue"
            />
            <PortfolioCard
              title="Leverage"
              value={leverageValue}
              description="Open positions"
              link="/leverage"
              isLoading={isLoading}
              colorClass="text-cyber-electric-fuchsia"
            />
            <PortfolioCard
              title="Lending"
              value={lendingValue}
              description="Morpho supplied"
              link="/yield"
              isLoading={isLoading}
              colorClass="text-cyber-neon-green"
            />
          </div>

          {/* Positions section */}
          {positions.length > 0 && (
            <div className="mb-12">
              <h2 className="text-xl font-semibold text-cyber-text-primary mb-4">Open Positions</h2>

              {hasLowHealth && (
                <Alert variant="warning" title="Low Health Factor Warning" className="mb-6 shadow-lg shadow-cyber-warning-text/10">
                  One or more positions have low health factors and may be at risk of liquidation.
                </Alert>
              )}

              <div className="space-y-4">
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
            </div>
          )}

          {/* Trade / Leverage / Yield widget */}
          <div className="bg-cyber-surface-dark border border-cyber-border-glow/30 overflow-hidden shadow-lg shadow-cyber-border-glow/10">
            <MainTabNav activeTab={mainTab} onTabChange={handleTabChange} />

            {/* Tab content */}
            <div className="p-6 md:p-8 lg:p-12">
              {mainTab === 'trade' && (
                <TradeCard
                  usdcBalance={usdcBalance}
                  bearBalance={bearBalance}
                  bullBalance={bullBalance}
                />
              )}

              {mainTab === 'leverage' && (
                <LeverageCard usdcBalance={usdcBalance} />
              )}

              {mainTab === 'yield' && (
                <YieldCard
                  suppliedAmount={5000n * 10n ** 6n}
                  borrowedAmount={1000n * 10n ** 6n}
                  availableToBorrow={3000n * 10n ** 6n}
                  supplyApy={3.5}
                  borrowApy={5.2}
                  usdcBalance={usdcBalance}
                  suppliedBalance={5000n * 10n ** 6n}
                />
              )}
            </div>
          </div>

          {/* Transaction History link */}
          <div className="mt-8 text-center pb-8">
            <Link
              to="/history"
              className="inline-flex items-center gap-2 text-cyber-electric-fuchsia hover:text-cyber-electric-fuchsia/80 font-medium text-sm transition-colors"
            >
              View Transaction History
              <span className="material-symbols-outlined text-sm">arrow_forward</span>
            </Link>
          </div>

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
        </>
      ) : (
        <ConnectWalletPrompt />
      )}
    </div>
  )
}
