import { useSwitchChain, useChainId } from 'wagmi'
import { mainnet, sepolia } from 'wagmi/chains'
import { Button, Modal } from '../ui'

interface NetworkSwitcherProps {
  isOpen: boolean
  onClose: () => void
}

export function NetworkSwitcher({ isOpen, onClose }: NetworkSwitcherProps) {
  const chainId = useChainId()
  const { switchChain, isPending } = useSwitchChain()

  const networks = [
    { chain: mainnet, name: 'Ethereum Mainnet', icon: '🔷' },
    { chain: sepolia, name: 'Sepolia Testnet', icon: '🔶' },
  ]

  const handleSwitch = async (targetChainId: typeof mainnet.id | typeof sepolia.id) => {
    await switchChain({ chainId: targetChainId })
    onClose()
  }

  return (
    <Modal isOpen={isOpen} onClose={onClose} title="Select Network" size="sm">
      <div className="space-y-2">
        {networks.map(({ chain, name, icon }) => (
          <button
            key={chain.id}
            onClick={() => handleSwitch(chain.id)}
            disabled={isPending}
            className={`
              w-full flex items-center gap-3 px-4 py-3 rounded-lg transition-colors
              ${
                chainId === chain.id
                  ? 'bg-primary-600/20 border border-primary-600'
                  : 'bg-surface-200 hover:bg-surface-50 border border-transparent'
              }
            `}
          >
            <span className="text-2xl">{icon}</span>
            <div className="text-left">
              <p className="text-white font-medium">{name}</p>
              <p className="text-sm text-gray-400">Chain ID: {chain.id}</p>
            </div>
            {chainId === chain.id && (
              <span className="ml-auto text-primary-500">
                <svg className="w-5 h-5" fill="currentColor" viewBox="0 0 20 20">
                  <path
                    fillRule="evenodd"
                    d="M16.707 5.293a1 1 0 010 1.414l-8 8a1 1 0 01-1.414 0l-4-4a1 1 0 011.414-1.414L8 12.586l7.293-7.293a1 1 0 011.414 0z"
                    clipRule="evenodd"
                  />
                </svg>
              </span>
            )}
          </button>
        ))}
      </div>
    </Modal>
  )
}

// Component to show when on wrong network
export function WrongNetworkBanner() {
  const chainId = useChainId()
  const { switchChain, isPending } = useSwitchChain()

  const isWrongNetwork = chainId !== mainnet.id && chainId !== sepolia.id

  if (!isWrongNetwork) return null

  return (
    <div className="bg-red-900/50 border-b border-red-800 px-4 py-3">
      <div className="max-w-7xl mx-auto flex items-center justify-between">
        <p className="text-red-200">
          Please connect to Ethereum Mainnet or Sepolia to use Plether.
        </p>
        <Button
          variant="danger"
          size="sm"
          isLoading={isPending}
          onClick={() => switchChain({ chainId: mainnet.id })}
        >
          Switch to Mainnet
        </Button>
      </div>
    </div>
  )
}
