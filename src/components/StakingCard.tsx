import { useState, useEffect } from 'react'
import { TokenIcon } from './ui'
import { TokenInput } from './TokenInput'
import { formatAmount } from '../utils/formatters'
import { parseStakingAmount, getStakingDecimals, SHARE_DECIMALS } from '../utils/staking'
import { useStakeWithPermit, useUnstake, useStakedBalance, usePreviewDeposit, usePreviewRedeem } from '../hooks/useStaking'

type StakeMode = 'stake' | 'unstake'

export interface StakingCardProps {
  side: 'BEAR' | 'BULL'
  tokenBalance: bigint
}

export function StakingCard({ side, tokenBalance }: StakingCardProps) {
  const [mode, setMode] = useState<StakeMode>('stake')
  const [amount, setAmount] = useState('')

  const { shares: stakedBalance, refetch: refetchStaked } = useStakedBalance(side)
  const { stakeWithPermit, isPending: stakePending, isSigningPermit, isSuccess: stakeSuccess, reset: resetStake } = useStakeWithPermit(side)
  const { unstake, isPending: unstakePending, isSuccess: unstakeSuccess, reset: resetUnstake } = useUnstake(side)

  const decimals = getStakingDecimals(mode)
  const amountBigInt = parseStakingAmount(amount, mode)

  const { shares: previewShares, isLoading: previewDepositLoading } = usePreviewDeposit(side, mode === 'stake' ? amountBigInt : 0n)
  const { assets: previewAssets, isLoading: previewRedeemLoading } = usePreviewRedeem(side, mode === 'unstake' ? amountBigInt : 0n)

  useEffect(() => {
    if (stakeSuccess) {
      refetchStaked()
      setAmount('')
      resetStake()
    }
  }, [stakeSuccess, refetchStaked, resetStake])

  useEffect(() => {
    if (unstakeSuccess) {
      refetchStaked()
      setAmount('')
      resetUnstake()
    }
  }, [unstakeSuccess, refetchStaked, resetUnstake])

  const handleStake = async () => {
    await stakeWithPermit(amountBigInt)
  }

  const handleUnstake = async () => {
    await unstake(amountBigInt)
  }

  const handleAction = mode === 'stake' ? handleStake : handleUnstake

  const isBear = side === 'BEAR'
  const textColor = isBear ? 'text-cyber-electric-fuchsia' : 'text-cyber-neon-green'
  const bgColor = isBear ? 'bg-cyber-electric-fuchsia' : 'bg-cyber-neon-green'
  const shadowColor = isBear ? 'shadow-cyber-electric-fuchsia' : 'shadow-cyber-neon-green'

  const balance = mode === 'stake' ? tokenBalance : stakedBalance
  const insufficientBalance = amountBigInt > balance

  const getButtonText = () => {
    if (mode === 'stake') {
      if (isSigningPermit) return 'Sign Permit...'
      if (stakePending) return 'Staking...'
      if (insufficientBalance) return 'Insufficient Balance'
      return `Stake DXY-${side}`
    } else {
      if (unstakePending) return 'Unstaking...'
      if (insufficientBalance) return 'Insufficient Balance'
      return `Unstake sDXY-${side}`
    }
  }

  const isDisabled = !amount || parseFloat(amount) <= 0 ||
    stakePending || unstakePending || insufficientBalance

  return (
    <div className="bg-cyber-surface-dark border border-cyber-border-glow/30 shadow-lg overflow-hidden">
      <div className={`px-6 py-4 border-b border-cyber-border-glow/30 ${isBear ? 'bg-cyber-electric-fuchsia/10' : 'bg-cyber-neon-green/10'}`}>
        <div className="flex items-center gap-3">
          <TokenIcon side={side} />
          <div>
            <h3 className={`font-semibold ${textColor}`}>DXY-{side} Staking</h3>
            <p className="text-xs text-cyber-text-secondary">Stake to use as collateral</p>
          </div>
        </div>
      </div>

      <div className="p-6 space-y-6">
        <div className={`bg-cyber-surface-light p-4 border ${isBear ? 'border-cyber-electric-fuchsia/30' : 'border-cyber-neon-green/30'}`}>
          <div className="flex justify-between items-center">
            <span className="text-cyber-text-secondary text-sm">Staked Balance</span>
            <span className={`${textColor} font-semibold`}>
              {formatAmount(stakedBalance, SHARE_DECIMALS)} sDXY-{side}
            </span>
          </div>
        </div>

        <div className="bg-cyber-surface-light p-1 flex text-sm font-medium border border-cyber-border-glow/30">
          <button
            onClick={() => { setMode('stake'); setAmount('') }}
            className={`flex-1 py-2 px-4 transition-all ${
              mode === 'stake'
                ? `bg-cyber-surface-dark ${textColor} shadow-sm ${shadowColor}/10 border border-${isBear ? 'cyber-electric-fuchsia' : 'cyber-neon-green'}/50`
                : 'text-cyber-text-secondary hover:text-cyber-bright-blue'
            }`}
          >
            Stake
          </button>
          <button
            onClick={() => { setMode('unstake'); setAmount('') }}
            className={`flex-1 py-2 px-4 transition-all ${
              mode === 'unstake'
                ? `bg-cyber-surface-dark ${textColor} shadow-sm ${shadowColor}/10 border border-${isBear ? 'cyber-electric-fuchsia' : 'cyber-neon-green'}/50`
                : 'text-cyber-text-secondary hover:text-cyber-bright-blue'
            }`}
          >
            Unstake
          </button>
        </div>

        <TokenInput
          label={mode === 'stake' ? `DXY-${side} to stake` : `sDXY-${side} to unstake`}
          value={amount}
          onChange={setAmount}
          token={{ symbol: mode === 'stake' ? `DXY-${side}` : `sDXY-${side}`, decimals }}
          balance={balance}
        />

        {amountBigInt > 0n && (
          <div className={`bg-cyber-surface-light p-3 border ${isBear ? 'border-cyber-electric-fuchsia/30' : 'border-cyber-neon-green/30'}`}>
            <div className="flex justify-between items-center text-sm">
              <span className="text-cyber-text-secondary">You will receive</span>
              <span className={`${textColor} font-medium`}>
                {mode === 'stake'
                  ? (previewDepositLoading ? '...' : formatAmount(previewShares, SHARE_DECIMALS))
                  : (previewRedeemLoading ? '...' : formatAmount(previewAssets, 18))
                } {mode === 'stake' ? `sDXY-${side}` : `DXY-${side}`}
              </span>
            </div>
          </div>
        )}

        <button
          onClick={handleAction}
          disabled={isDisabled}
          className={`w-full ${bgColor} hover:opacity-90 ${isBear ? 'text-cyber-text-primary' : 'text-cyber-bg'} font-semibold py-4 px-6 shadow-lg ${shadowColor}/40 transition-all transform hover:-translate-y-0.5 active:translate-y-0 text-lg disabled:opacity-50 disabled:cursor-not-allowed disabled:transform-none disabled:shadow-none`}
        >
          {getButtonText()}
        </button>
      </div>
    </div>
  )
}
