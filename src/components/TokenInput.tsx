import { type ChangeEvent } from 'react'
import { formatAmount } from '../utils/formatters'
import { Button } from './ui'

interface TokenInputProps {
  value: string
  onChange: (value: string) => void
  token: {
    symbol: string
    decimals: number
  }
  balance?: bigint
  label?: string
  disabled?: boolean
  error?: string
}

export function TokenInput({
  value,
  onChange,
  token,
  balance,
  label,
  disabled,
  error,
}: TokenInputProps) {
  const handleChange = (e: ChangeEvent<HTMLInputElement>) => {
    const newValue = e.target.value
    // Only allow valid number inputs
    if (newValue === '' || /^\d*\.?\d*$/.test(newValue)) {
      onChange(newValue)
    }
  }

  const handleMax = () => {
    if (balance) {
      const formatted = formatAmount(balance, token.decimals, token.decimals)
      onChange(formatted.replace(/,/g, ''))
    }
  }

  return (
    <div className="w-full">
      {label && (
        <label className="block text-sm font-medium text-gray-300 mb-1.5">
          {label}
        </label>
      )}
      <div className="relative">
        <input
          type="text"
          inputMode="decimal"
          value={value}
          onChange={handleChange}
          disabled={disabled}
          placeholder="0.00"
          className={`
            w-full px-4 py-4 pr-32 bg-surface-200 border rounded-lg text-white text-xl
            placeholder-gray-500 focus:outline-none focus:ring-2
            focus:ring-primary-500 focus:border-transparent
            disabled:opacity-50 disabled:cursor-not-allowed
            ${error ? 'border-red-500' : 'border-gray-700'}
          `}
        />
        <div className="absolute right-3 top-1/2 -translate-y-1/2 flex items-center gap-2">
          {balance !== undefined && (
            <Button
              variant="ghost"
              size="sm"
              onClick={handleMax}
              disabled={disabled}
              className="text-primary-500 hover:text-primary-400"
            >
              MAX
            </Button>
          )}
          <span className="text-gray-300 font-medium">{token.symbol}</span>
        </div>
      </div>

      {/* Balance display */}
      {balance !== undefined && (
        <div className="flex justify-between mt-2 text-sm">
          <span className="text-gray-400">Balance:</span>
          <span className="text-gray-300">
            {formatAmount(balance, token.decimals)} {token.symbol}
          </span>
        </div>
      )}

      {error && <p className="mt-1 text-sm text-red-500">{error}</p>}
    </div>
  )
}
