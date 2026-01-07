import { useAccount } from 'wagmi'
import { Card, CardHeader, Button, SkeletonCard } from '../components/ui'
import { PriceDisplay } from '../components/PriceDisplay'
import { formatUsd } from '../utils/formatters'
import { Link } from 'react-router-dom'

export function Dashboard() {
  const { isConnected } = useAccount()

  // TODO: Replace with actual hooks
  const isLoading = false
  const portfolioValue = 10000n * 10n ** 6n // $10,000 mock
  const spotValue = 5000n * 10n ** 6n
  const stakedValue = 3000n * 10n ** 6n
  const leverageValue = 1500n * 10n ** 6n
  const lendingValue = 500n * 10n ** 6n

  return (
    <div className="space-y-6">
      {/* Page title */}
      <div>
        <h1 className="text-2xl font-bold text-white">Dashboard</h1>
        <p className="text-gray-400">Your portfolio overview</p>
      </div>

      {/* DXY Price widget (detailed) */}
      <PriceDisplay variant="detailed" />

      {/* Portfolio overview */}
      {isConnected ? (
        <>
          {/* Total Portfolio Value */}
          <Card className="bg-gradient-to-r from-primary-900/20 to-blue-900/20 border-primary-800/50">
            <div className="text-center py-4">
              <p className="text-gray-400 mb-2">Total Portfolio Value</p>
              {isLoading ? (
                <div className="h-10 w-40 mx-auto bg-surface-50 rounded animate-pulse" />
              ) : (
                <p className="text-4xl font-bold text-white">
                  {formatUsd(portfolioValue)}
                </p>
              )}
            </div>
          </Card>

          {/* Portfolio breakdown */}
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
            <PortfolioCard
              title="Spot Holdings"
              value={spotValue}
              description="USDC, DXY-BEAR, DXY-BULL"
              link="/trade"
              isLoading={isLoading}
            />
            <PortfolioCard
              title="Staked"
              value={stakedValue}
              description="sDXY-BEAR, sDXY-BULL"
              link="/stake"
              isLoading={isLoading}
            />
            <PortfolioCard
              title="Leverage"
              value={leverageValue}
              description="Open positions"
              link="/positions"
              isLoading={isLoading}
            />
            <PortfolioCard
              title="Lending"
              value={lendingValue}
              description="Morpho supplied"
              link="/lend"
              isLoading={isLoading}
            />
          </div>

          {/* Quick actions */}
          <Card>
            <CardHeader title="Quick Actions" />
            <div className="grid grid-cols-2 md:grid-cols-4 gap-3">
              <Link to="/trade">
                <Button variant="secondary" className="w-full">
                  Trade
                </Button>
              </Link>
              <Link to="/leverage">
                <Button variant="secondary" className="w-full">
                  Open Leverage
                </Button>
              </Link>
              <Link to="/stake">
                <Button variant="secondary" className="w-full">
                  Stake
                </Button>
              </Link>
              <Link to="/mint">
                <Button variant="secondary" className="w-full">
                  Mint Pairs
                </Button>
              </Link>
            </div>
          </Card>

          {/* Transaction History link */}
          <div className="text-center">
            <Link
              to="/history"
              className="text-primary-500 hover:text-primary-400 text-sm"
            >
              View Transaction History →
            </Link>
          </div>
        </>
      ) : (
        /* Not connected state */
        <Card className="text-center py-12">
          <div className="w-16 h-16 mx-auto mb-4 rounded-full bg-surface-50 flex items-center justify-center">
            <svg className="w-8 h-8 text-gray-400" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 15v2m-6 4h12a2 2 0 002-2v-6a2 2 0 00-2-2H6a2 2 0 00-2 2v6a2 2 0 002 2zm10-10V7a4 4 0 00-8 0v4h8z" />
            </svg>
          </div>
          <h2 className="text-xl font-semibold text-white mb-2">Connect Your Wallet</h2>
          <p className="text-gray-400 mb-6 max-w-md mx-auto">
            Connect your wallet to view your portfolio, trade DXY-BEAR and DXY-BULL,
            and access all Plether features.
          </p>
          <p className="text-sm text-gray-500">
            You can browse prices and protocol stats without connecting.
          </p>
        </Card>
      )}
    </div>
  )
}

interface PortfolioCardProps {
  title: string
  value: bigint
  description: string
  link: string
  isLoading: boolean
}

function PortfolioCard({ title, value, description, link, isLoading }: PortfolioCardProps) {
  if (isLoading) {
    return <SkeletonCard />
  }

  return (
    <Link to={link}>
      <Card className="hover:border-gray-700 transition-colors cursor-pointer h-full">
        <p className="text-sm text-gray-400 mb-1">{title}</p>
        <p className="text-2xl font-bold text-white mb-2">{formatUsd(value)}</p>
        <p className="text-xs text-gray-500">{description}</p>
      </Card>
    </Link>
  )
}
