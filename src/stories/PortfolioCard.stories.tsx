import type { Meta, StoryObj } from '@storybook/react-vite'
import { MemoryRouter, Link } from 'react-router-dom'
import { Skeleton } from '../components/ui'

const meta: Meta = {
  title: 'Components/PortfolioCard',
  tags: ['autodocs'],
}

export default meta
type Story = StoryObj

function formatUsd(value: bigint): string {
  const num = Number(value) / 1e6
  return new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD' }).format(num)
}

function SkeletonCard() {
  return (
    <div className="bg-cyber-surface-dark p-5 border border-cyber-border-glow/30 shadow-md h-full">
      <Skeleton className="h-3 w-24 mb-3" />
      <Skeleton className="h-8 w-32 mb-2" />
      <Skeleton className="h-3 w-40" />
    </div>
  )
}

interface PortfolioCardProps {
  title: string
  value: bigint
  description: string
  link: string
  isLoading?: boolean
  colorClass: string
}

function PortfolioCard({ title, value, description, link, isLoading, colorClass }: PortfolioCardProps) {
  if (isLoading) {
    return <SkeletonCard />
  }

  return (
    <Link to={link}>
      <div className="bg-cyber-surface-dark p-5 border border-cyber-border-glow/30 shadow-md hover:border-cyber-bright-blue/50 transition-colors cursor-pointer h-full">
        <p className="text-xs text-cyber-text-secondary uppercase tracking-wider font-medium mb-2">{title}</p>
        <div className={`text-2xl font-bold mb-1 ${colorClass}`}>{formatUsd(value)}</div>
        <p className="text-xs text-cyber-text-secondary truncate">{description}</p>
      </div>
    </Link>
  )
}

export const SpotHoldings: Story = {
  render: () => (
    <MemoryRouter>
      <div className="w-64">
        <PortfolioCard
          title="Spot Holdings"
          value={BigInt(12500 * 1e6)}
          description="5,000 DXY-BEAR • 7,500 DXY-BULL"
          link="/"
          colorClass="text-cyber-bright-blue"
        />
      </div>
    </MemoryRouter>
  ),
}

export const StakedTokens: Story = {
  render: () => (
    <MemoryRouter>
      <div className="w-64">
        <PortfolioCard
          title="Staked"
          value={BigInt(8000 * 1e6)}
          description="3,000 sDXY-BEAR • 5,000 sDXY-BULL"
          link="/stake"
          colorClass="text-cyber-bright-blue"
        />
      </div>
    </MemoryRouter>
  ),
}

export const LeveragePositions: Story = {
  render: () => (
    <MemoryRouter>
      <div className="w-64">
        <PortfolioCard
          title="Leverage"
          value={BigInt(25000 * 1e6)}
          description="2 active positions"
          link="/leverage"
          colorClass="text-cyber-electric-fuchsia"
        />
      </div>
    </MemoryRouter>
  ),
}

export const Loading: Story = {
  render: () => (
    <MemoryRouter>
      <div className="w-64">
        <PortfolioCard
          title="Spot Holdings"
          value={BigInt(0)}
          description=""
          link="/"
          isLoading
          colorClass="text-cyber-bright-blue"
        />
      </div>
    </MemoryRouter>
  ),
}

export const AllCards: Story = {
  render: () => (
    <MemoryRouter>
      <div className="grid grid-cols-3 gap-4">
        <PortfolioCard
          title="Spot Holdings"
          value={BigInt(12500 * 1e6)}
          description="5,000 DXY-BEAR • 7,500 DXY-BULL"
          link="/"
          colorClass="text-cyber-bright-blue"
        />
        <PortfolioCard
          title="Staked"
          value={BigInt(8000 * 1e6)}
          description="3,000 sDXY-BEAR • 5,000 sDXY-BULL"
          link="/stake"
          colorClass="text-cyber-bright-blue"
        />
        <PortfolioCard
          title="Leverage"
          value={BigInt(25000 * 1e6)}
          description="2 active positions"
          link="/leverage"
          colorClass="text-cyber-electric-fuchsia"
        />
      </div>
    </MemoryRouter>
  ),
}
