import { useAccount, useDisconnect, useChainId } from 'wagmi'
import { useWeb3Modal } from '@web3modal/wagmi/react'
import { mainnet, sepolia } from 'wagmi/chains'
import { Button, Badge } from '../ui'
import { formatAddress } from '../../utils/formatters'

export function ConnectButton() {
  const { address, isConnected } = useAccount()
  const { disconnect } = useDisconnect()
  const { open } = useWeb3Modal()
  const chainId = useChainId()

  const getNetworkName = () => {
    switch (chainId) {
      case mainnet.id:
        return 'Mainnet'
      case sepolia.id:
        return 'Sepolia'
      default:
        return 'Unknown'
    }
  }

  const isWrongNetwork = chainId !== mainnet.id && chainId !== sepolia.id

  if (!isConnected) {
    return (
      <Button variant="primary" onClick={() => open()}>
        Connect Wallet
      </Button>
    )
  }

  return (
    <div className="flex items-center gap-2">
      {/* Network badge */}
      <Badge variant={isWrongNetwork ? 'danger' : chainId === sepolia.id ? 'warning' : 'success'}>
        {isWrongNetwork ? 'Wrong Network' : getNetworkName()}
      </Badge>

      {/* Account button */}
      <button
        onClick={() => open({ view: 'Account' })}
        className="flex items-center gap-2 px-3 py-2 bg-surface-50 hover:bg-surface-100 rounded-lg border border-gray-700 transition-colors"
      >
        <div className="w-6 h-6 rounded-full bg-gradient-to-r from-primary-500 to-blue-500" />
        <span className="text-sm text-white font-medium">
          {formatAddress(address || '')}
        </span>
      </button>

      {/* Disconnect button */}
      <button
        onClick={() => disconnect()}
        className="p-2 text-gray-400 hover:text-white transition-colors"
        title="Disconnect"
      >
        <svg className="w-5 h-5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
          <path
            strokeLinecap="round"
            strokeLinejoin="round"
            strokeWidth={2}
            d="M17 16l4-4m0 0l-4-4m4 4H7m6 4v1a3 3 0 01-3 3H6a3 3 0 01-3-3V7a3 3 0 013-3h4a3 3 0 013 3v1"
          />
        </svg>
      </button>
    </div>
  )
}
