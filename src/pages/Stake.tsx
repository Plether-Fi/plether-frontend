import { useAccount } from 'wagmi'
import { StakingCard } from '../components/StakingCard'
import { ConnectWalletPrompt } from '../components/ConnectWalletPrompt'

export function Stake() {
  const { isConnected } = useAccount()

  return (
    <div className="space-y-10">
      <div className="mb-8">
        <h1 className="text-3xl font-semibold text-cyber-text-primary mb-1">Stake</h1>
        <p className="text-cyber-text-secondary font-light">Stake your tokens to use as collateral</p>
      </div>

      {isConnected ? (
        <div className="grid md:grid-cols-2 gap-6">
          <StakingCard
            side="BEAR"
            tokenBalance={500n * 10n ** 18n}
            stakedBalance={200n * 10n ** 18n}
          />
          <StakingCard
            side="BULL"
            tokenBalance={500n * 10n ** 18n}
            stakedBalance={150n * 10n ** 18n}
          />
        </div>
      ) : (
        <ConnectWalletPrompt description="Connect your wallet to stake DXY-BEAR and DXY-BULL tokens." />
      )}
    </div>
  )
}
