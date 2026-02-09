import { http, createConfig } from 'wagmi'
import { defineChain } from 'viem'
import { mainnet, sepolia } from 'wagmi/chains'
import { injected, walletConnect } from 'wagmi/connectors'
import { WagmiAdapter } from '@reown/appkit-adapter-wagmi'
import { mainnet as appkitMainnet, sepolia as appkitSepolia } from '@reown/appkit/networks'
import { transactionManager } from '../services/transactionManager'

export const WALLETCONNECT_PROJECT_ID = '1ac6ecffb101d037c113363688a6ef8e'

export const anvil = defineChain({
  id: 31337,
  name: 'Anvil',
  nativeCurrency: { name: 'Ether', symbol: 'ETH', decimals: 18 },
  rpcUrls: {
    default: { http: ['http://127.0.0.1:8545'] },
  },
})

export const appkitNetworks: [typeof appkitMainnet, typeof appkitSepolia] = [appkitMainnet, appkitSepolia]

const metadata = {
  name: 'Plether',
  description: 'plDXY-BEAR and plDXY-BULL trading protocol',
  url: window.location.origin,
  icons: [`${window.location.origin}/logo.png`],
}

export const config = createConfig({
  chains: [mainnet, sepolia, anvil],
  connectors: [
    injected(),
    walletConnect({ projectId: WALLETCONNECT_PROJECT_ID, metadata }),
  ],
  transports: {
    [mainnet.id]: http(),
    [sepolia.id]: http(),
    [anvil.id]: http('http://127.0.0.1:8545'),
  },
})

export const wagmiAdapter = new WagmiAdapter({
  networks: appkitNetworks,
  projectId: WALLETCONNECT_PROJECT_ID,
})
wagmiAdapter.wagmiConfig = config

declare module 'wagmi' {
  interface Register {
    config: typeof config
  }
}

transactionManager.setConfig(config)
