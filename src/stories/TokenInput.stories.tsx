import { useState } from 'react'
import type { Meta, StoryObj } from '@storybook/react-vite'
import { TokenInput } from '../components/TokenInput'

const meta: Meta<typeof TokenInput> = {
  title: 'Components/TokenInput',
  component: TokenInput,
  tags: ['autodocs'],
}

export default meta
type Story = StoryObj<typeof meta>

function TokenInputWithState(props: Partial<Parameters<typeof TokenInput>[0]>) {
  const [value, setValue] = useState('')
  return (
    <TokenInput
      value={value}
      onChange={setValue}
      token={{ symbol: 'USDC', decimals: 6 }}
      {...props}
    />
  )
}

export const Default: Story = {
  render: () => <TokenInputWithState />,
}

export const WithLabel: Story = {
  render: () => <TokenInputWithState label="Amount to deposit" />,
}

export const WithBalance: Story = {
  render: () => (
    <TokenInputWithState
      label="Amount"
      balance={1000000000n}
    />
  ),
}

export const WithError: Story = {
  render: () => (
    <TokenInputWithState
      label="Amount"
      balance={1000000000n}
      error="Insufficient balance"
    />
  ),
}

export const Disabled: Story = {
  render: () => (
    <TokenInputWithState
      label="Amount"
      balance={1000000000n}
      disabled
    />
  ),
}

export const DifferentTokens: Story = {
  render: () => (
    <div className="space-y-4">
      <TokenInputWithState
        label="USDC Amount"
        token={{ symbol: 'USDC', decimals: 6 }}
        balance={1000000000n}
      />
      <TokenInputWithState
        label="DXY-BEAR Amount"
        token={{ symbol: 'DXY-BEAR', decimals: 18 }}
        balance={500000000000000000000n}
      />
      <TokenInputWithState
        label="DXY-BULL Amount"
        token={{ symbol: 'DXY-BULL', decimals: 18 }}
        balance={750000000000000000000n}
      />
    </div>
  ),
}
