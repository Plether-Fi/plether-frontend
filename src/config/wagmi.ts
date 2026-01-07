import { http, createConfig } from 'wagmi'
import { mainnet, sepolia } from 'wagmi/chains'
import { injected, walletConnect } from 'wagmi/connectors'

// WalletConnect project ID - replace with your own from https://cloud.walletconnect.com
export const WALLETCONNECT_PROJECT_ID = '1ac6ecffb101d037c113363688a6ef8e'

export const config = createConfig({
  chains: [mainnet, sepolia],
  connectors: [
    injected(),
    walletConnect({
      projectId: WALLETCONNECT_PROJECT_ID,
      metadata: {
        name: 'Plether',
        description: 'DXY-BEAR and DXY-BULL trading protocol',
        url: 'https://app.plether.com',
        icons: ['https://app.plether.com/logo.png'],
      },
    }),
  ],
  transports: {
    [mainnet.id]: http(),
    [sepolia.id]: http(),
  },
})

declare module 'wagmi' {
  interface Register {
    config: typeof config
  }
}
