import { useState } from 'react'
import { useSettingsStore } from '../stores/settingsStore'
import { SLIPPAGE_PRESETS, MAX_SLIPPAGE } from '../config/constants'
import { Button, Modal, Input } from './ui'

export function SlippageSelector() {
  const { slippage, setSlippage } = useSettingsStore()
  const [isOpen, setIsOpen] = useState(false)
  const [customValue, setCustomValue] = useState('')

  const handlePresetClick = (value: number) => {
    setSlippage(value)
    setCustomValue('')
  }

  const handleCustomChange = (value: string) => {
    setCustomValue(value)
    const num = parseFloat(value)
    if (!isNaN(num) && num > 0 && num <= MAX_SLIPPAGE) {
      setSlippage(num)
    }
  }

  const isCustom = !SLIPPAGE_PRESETS.includes(slippage as typeof SLIPPAGE_PRESETS[number])

  return (
    <>
      <button
        onClick={() => setIsOpen(true)}
        className="flex items-center gap-1 text-sm text-gray-400 hover:text-white transition-colors"
      >
        <svg className="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
          <path
            strokeLinecap="round"
            strokeLinejoin="round"
            strokeWidth={2}
            d="M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 002.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 001.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 00-1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 00-2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 00-2.573-1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 00-1.065-2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 001.066-2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z"
          />
          <path
            strokeLinecap="round"
            strokeLinejoin="round"
            strokeWidth={2}
            d="M15 12a3 3 0 11-6 0 3 3 0 016 0z"
          />
        </svg>
        <span>{slippage}% slippage</span>
      </button>

      <Modal
        isOpen={isOpen}
        onClose={() => setIsOpen(false)}
        title="Slippage Settings"
        size="sm"
      >
        <div className="space-y-4">
          <p className="text-sm text-gray-400">
            Your transaction will revert if the price changes unfavorably by more
            than this percentage.
          </p>

          {/* Preset buttons */}
          <div className="flex gap-2">
            {SLIPPAGE_PRESETS.map((preset) => (
              <Button
                key={preset}
                variant={slippage === preset ? 'primary' : 'secondary'}
                size="sm"
                onClick={() => handlePresetClick(preset)}
                className="flex-1"
              >
                {preset}%
              </Button>
            ))}
          </div>

          {/* Custom input */}
          <div>
            <label className="block text-sm text-gray-400 mb-1">
              Custom (max {MAX_SLIPPAGE}%)
            </label>
            <Input
              type="text"
              inputMode="decimal"
              value={customValue}
              onChange={(e) => handleCustomChange(e.target.value)}
              placeholder="0.5"
              rightElement={<span className="text-gray-400">%</span>}
              className={isCustom ? 'ring-2 ring-primary-500' : ''}
            />
          </div>

          <Button
            variant="primary"
            onClick={() => setIsOpen(false)}
            className="w-full"
          >
            Save
          </Button>
        </div>
      </Modal>
    </>
  )
}
