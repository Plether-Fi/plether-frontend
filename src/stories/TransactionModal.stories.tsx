import type { Meta, StoryObj } from '@storybook/react-vite'
import { useEffect } from 'react'
import { WagmiProvider } from 'wagmi'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import { config } from '../config/wagmi'
import { LoadingScreen, type LoadingStep } from '../components/ui/LoadingScreen'
import { TransactionModal } from '../components/TransactionModal'
import { useTransactionModal } from '../hooks/useTransactionModal'

const queryClient = new QueryClient()

const meta: Meta<typeof LoadingScreen> = {
  title: 'Components/TransactionModal',
  component: LoadingScreen,
  decorators: [
    (Story) => (
      <div className="bg-cyber-surface-dark border border-cyber-border-glow/50 max-w-md mx-auto">
        <Story />
      </div>
    ),
  ],
}

export default meta
type Story = StoryObj<typeof meta>

const buySteps: LoadingStep[] = [
  { label: 'Approve USDC', status: 'completed' },
  { label: 'Confirming approval', status: 'completed' },
  { label: 'Buy DXY-BEAR', status: 'in_progress' },
  { label: 'Awaiting confirmation', status: 'pending' },
]

export const InProgress: Story = {
  render: () => (
    <LoadingScreen
      title="Buying DXY-BEAR"
      steps={buySteps}
    />
  ),
}

export const SwapInProgress: Story = {
  render: () => (
    <LoadingScreen
      title="Selling DXY-BULL"
      steps={[
        { label: 'Sell DXY-BULL', status: 'in_progress' },
        { label: 'Awaiting confirmation', status: 'pending' },
      ]}
    />
  ),
}

export const Success: Story = {
  render: () => (
    <LoadingScreen
      title="Transaction complete!"
      steps={[
        { label: 'Approve USDC', status: 'completed' },
        { label: 'Confirming approval', status: 'completed' },
        { label: 'Buy DXY-BEAR', status: 'completed' },
        { label: 'Awaiting confirmation', status: 'completed' },
      ]}
      transactionUrl="https://sepolia.etherscan.io/tx/0x1234567890abcdef"
      onClose={() => {}}
    />
  ),
}

export const Error: Story = {
  render: () => (
    <LoadingScreen
      title="Transaction failed"
      steps={[
        { label: 'Approve USDC', status: 'completed' },
        { label: 'Confirming approval', status: 'completed' },
        { label: 'Buy DXY-BEAR', status: 'error' },
        { label: 'Awaiting confirmation', status: 'pending' },
      ]}
      errorMessage="Transaction reverted: insufficient liquidity in the pool."
      onClose={() => {}}
      onRetry={() => {}}
    />
  ),
}

export const StakingFlow: Story = {
  render: () => (
    <LoadingScreen
      title="Staking DXY-BEAR"
      steps={[
        { label: 'Sign permit', status: 'completed' },
        { label: 'Stake DXY-BEAR', status: 'in_progress' },
        { label: 'Awaiting confirmation', status: 'pending' },
      ]}
    />
  ),
}

export const MintFlow: Story = {
  render: () => (
    <LoadingScreen
      title="Minting token pairs"
      steps={[
        { label: 'Approve USDC', status: 'completed' },
        { label: 'Confirming approval', status: 'completed' },
        { label: 'Mint pairs', status: 'in_progress' },
        { label: 'Awaiting confirmation', status: 'pending' },
      ]}
    />
  ),
}

export const RedeemFlow: Story = {
  render: () => (
    <LoadingScreen
      title="Redeeming token pairs"
      steps={[
        { label: 'Approve DXY-BEAR', status: 'completed' },
        { label: 'Confirming approval', status: 'completed' },
        { label: 'Approve DXY-BULL', status: 'in_progress' },
        { label: 'Confirming approval', status: 'pending' },
        { label: 'Redeem pairs', status: 'pending' },
        { label: 'Awaiting confirmation', status: 'pending' },
      ]}
    />
  ),
}

function AnimatedDemo() {
  const modal = useTransactionModal()

  useEffect(() => {
    modal.reset()
    return () => modal.reset()
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  return <TransactionModal />
}

const sleep = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms))

const interactiveDecorators = [
  (Story: React.ComponentType) => (
    <WagmiProvider config={config}>
      <QueryClientProvider client={queryClient}>
        <Story />
      </QueryClientProvider>
    </WagmiProvider>
  ),
]

export const AnimatedSuccess: Story = {
  decorators: interactiveDecorators,
  render: () => <AnimatedDemo />,
  play: async ({ step }) => {
    const modal = useTransactionModal.getState()

    await step('Open modal', async () => {
      modal.open({
        title: 'Buying DXY-BEAR',
        steps: ['Approve USDC', 'Confirming approval', 'Buy DXY-BEAR', 'Awaiting confirmation'],
      })
      await sleep(500)
    })

    await step('Approve USDC', async () => {
      modal.setStepInProgress(0)
      await sleep(1000)
    })

    await step('Confirming approval', async () => {
      modal.setStepInProgress(1)
      await sleep(1500)
    })

    await step('Buy DXY-BEAR', async () => {
      modal.setStepInProgress(2)
      await sleep(1000)
    })

    await step('Awaiting confirmation', async () => {
      modal.setStepInProgress(3)
      await sleep(1500)
    })

    await step('Transaction success', async () => {
      modal.setSuccess('0xabc123def456789abc123def456789abc123def456789abc123def456789abcd')
    })
  },
}

export const AnimatedError: Story = {
  decorators: interactiveDecorators,
  render: () => <AnimatedDemo />,
  play: async ({ step }) => {
    const modal = useTransactionModal.getState()

    await step('Open modal', async () => {
      modal.open({
        title: 'Buying DXY-BEAR',
        steps: ['Approve USDC', 'Confirming approval', 'Buy DXY-BEAR', 'Awaiting confirmation'],
      })
      await sleep(500)
    })

    await step('Approve USDC', async () => {
      modal.setStepInProgress(0)
      await sleep(1000)
    })

    await step('Confirming approval', async () => {
      modal.setStepInProgress(1)
      await sleep(1500)
    })

    await step('Transaction rejected', async () => {
      modal.setError(1, 'User rejected the transaction')
    })
  },
}
