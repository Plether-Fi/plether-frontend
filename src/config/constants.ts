// Slippage settings
export const SLIPPAGE_PRESETS = [0.5, 1.0] as const
export const MAX_SLIPPAGE = 1.0 // 1% protocol max
export const DEFAULT_SLIPPAGE = 0.5

// Token decimals
export const USDC_DECIMALS = 6
export const TOKEN_DECIMALS = 18

// Health factor thresholds
export const HEALTH_FACTOR_WARNING = 1.5
export const HEALTH_FACTOR_DANGER = 1.2

// Protocol status
export type ProtocolStatus = 'Active' | 'Paused' | 'Settled'

// Local storage keys
export const STORAGE_KEYS = {
  SLIPPAGE: 'plether_slippage',
  PENDING_TXS: 'plether_pending_txs',
} as const
