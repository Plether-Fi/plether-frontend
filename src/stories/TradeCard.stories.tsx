import type { Meta, StoryObj } from '@storybook/react-vite'
import { TradeCard } from '../components/TradeCard'

interface TradeCardArgs {
  usdcBalance: number
  bearBalance: number
  bullBalance: number
}

const meta: Meta<TradeCardArgs> = {
  title: 'Components/TradeCard',
  tags: ['autodocs'],
  argTypes: {
    usdcBalance: {
      control: { type: 'number', min: 0 },
      description: 'USDC wallet balance',
    },
    bearBalance: {
      control: { type: 'number', min: 0 },
      description: 'DXY-BEAR wallet balance',
    },
    bullBalance: {
      control: { type: 'number', min: 0 },
      description: 'DXY-BULL wallet balance',
    },
  },
}

export default meta
type Story = StoryObj<TradeCardArgs>

function toUsdcBigint(value: number): bigint {
  return BigInt(Math.floor(value * 1e6))
}

function toTokenBigint(value: number): bigint {
  return BigInt(Math.floor(value * 1e18))
}

export const Default: Story = {
  args: {
    usdcBalance: 10000,
    bearBalance: 500,
    bullBalance: 500,
  },
  render: (args) => (
    <TradeCard
      usdcBalance={toUsdcBigint(args.usdcBalance)}
      bearBalance={toTokenBigint(args.bearBalance)}
      bullBalance={toTokenBigint(args.bullBalance)}
    />
  ),
}

export const HighBalances: Story = {
  args: {
    usdcBalance: 100000,
    bearBalance: 5000,
    bullBalance: 7500,
  },
  render: (args) => (
    <TradeCard
      usdcBalance={toUsdcBigint(args.usdcBalance)}
      bearBalance={toTokenBigint(args.bearBalance)}
      bullBalance={toTokenBigint(args.bullBalance)}
    />
  ),
}

export const LowBalances: Story = {
  args: {
    usdcBalance: 100,
    bearBalance: 10,
    bullBalance: 25,
  },
  render: (args) => (
    <TradeCard
      usdcBalance={toUsdcBigint(args.usdcBalance)}
      bearBalance={toTokenBigint(args.bearBalance)}
      bullBalance={toTokenBigint(args.bullBalance)}
    />
  ),
}
