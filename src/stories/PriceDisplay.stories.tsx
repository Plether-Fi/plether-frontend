import type { Meta, StoryObj } from '@storybook/react-vite'
import { PriceDisplay } from '../components/PriceDisplay'

const meta: Meta<typeof PriceDisplay> = {
  title: 'Components/PriceDisplay',
  component: PriceDisplay,
  tags: ['autodocs'],
  argTypes: {
    variant: {
      control: 'select',
      options: ['compact', 'detailed'],
    },
    isLoading: { control: 'boolean' },
  },
}

export default meta
type Story = StoryObj<typeof meta>

export const Compact: Story = {
  args: {
    variant: 'compact',
  },
}

export const Detailed: Story = {
  args: {
    variant: 'detailed',
  },
}

export const CompactLoading: Story = {
  args: {
    variant: 'compact',
    isLoading: true,
  },
}

export const DetailedLoading: Story = {
  args: {
    variant: 'detailed',
    isLoading: true,
  },
}

export const BothVariants: Story = {
  render: () => (
    <div className="space-y-6">
      <div>
        <p className="text-cyber-text-secondary text-sm mb-2">Compact:</p>
        <PriceDisplay variant="compact" />
      </div>
      <div>
        <p className="text-cyber-text-secondary text-sm mb-2">Detailed:</p>
        <PriceDisplay variant="detailed" />
      </div>
    </div>
  ),
}
