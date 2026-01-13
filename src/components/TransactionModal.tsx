import { useAccount } from 'wagmi'
import { Modal } from './ui/Modal'
import { LoadingScreen } from './ui/LoadingScreen'
import { useTransactionModal } from '../hooks/useTransactionModal'
import { getExplorerTxUrl } from '../utils/explorer'

export function TransactionModal() {
  const { chainId } = useAccount()
  const {
    isOpen,
    title,
    steps,
    errorMessage,
    transactionHash,
    onRetry,
    close,
    reset,
  } = useTransactionModal()

  const handleRetry = () => {
    reset()
    onRetry?.()
  }

  const transactionUrl = transactionHash
    ? getExplorerTxUrl(chainId, transactionHash)
    : undefined

  return (
    <Modal isOpen={isOpen} onClose={close} size="md">
      <LoadingScreen
        title={title}
        steps={steps}
        errorMessage={errorMessage}
        transactionUrl={transactionUrl}
        onClose={close}
        onRetry={onRetry ? handleRetry : undefined}
      />
    </Modal>
  )
}
