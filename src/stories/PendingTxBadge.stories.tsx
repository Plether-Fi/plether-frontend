import type { Meta, StoryObj } from '@storybook/react-vite'

const meta: Meta = {
  title: 'Components/PendingTxBadge',
  tags: ['autodocs'],
}

export default meta
type Story = StoryObj

function MockPendingBadge({ count }: { count: number }) {
  if (count === 0) return <p className="text-cyber-text-secondary text-sm">No pending transactions</p>

  return (
    <div className="flex items-center gap-2 px-3 py-1.5 bg-cyber-bright-blue/20 border border-cyber-bright-blue/50 rounded-full shadow-sm shadow-cyber-bright-blue/20">
      <div className="w-4 h-4 relative">
        <div className="absolute inset-0 rounded-full border-2 border-cyber-bright-blue/30 border-t-cyber-bright-blue animate-spin" />
      </div>
      <span className="text-sm text-cyber-bright-blue font-medium">
        {count} pending
      </span>
    </div>
  )
}

export const NoPending: Story = {
  render: () => <MockPendingBadge count={0} />,
}

export const OnePending: Story = {
  render: () => <MockPendingBadge count={1} />,
}

export const MultiplePending: Story = {
  render: () => <MockPendingBadge count={3} />,
}

export const AllStates: Story = {
  render: () => (
    <div className="space-y-4">
      <div>
        <p className="text-cyber-text-secondary text-sm mb-2">No pending:</p>
        <MockPendingBadge count={0} />
      </div>
      <div>
        <p className="text-cyber-text-secondary text-sm mb-2">1 pending:</p>
        <MockPendingBadge count={1} />
      </div>
      <div>
        <p className="text-cyber-text-secondary text-sm mb-2">5 pending:</p>
        <MockPendingBadge count={5} />
      </div>
    </div>
  ),
}
