import { Link, useLocation } from 'react-router-dom'
import { ConnectButton } from '../wallet/ConnectButton'
import { PendingTxBadge } from '../PendingTxBadge'
import { PriceDisplay } from '../PriceDisplay'

const navLinks = [
  { path: '/', label: 'Dashboard' },
  { path: '/trade', label: 'Trade' },
  { path: '/leverage', label: 'Leverage' },
  { path: '/positions', label: 'Positions' },
  { path: '/stake', label: 'Stake' },
  { path: '/lend', label: 'Lend' },
  { path: '/mint', label: 'Mint' },
]

export function Header() {
  const location = useLocation()

  return (
    <header className="sticky top-0 z-40 bg-surface-100/80 backdrop-blur-lg border-b border-gray-800">
      <div className="max-w-7xl mx-auto px-4">
        <div className="flex items-center justify-between h-16">
          {/* Logo */}
          <Link to="/" className="flex items-center gap-2">
            <img src="/icon.svg" alt="Plether" className="w-8 h-8" />
            <span className="text-xl font-bold text-white hidden sm:block">Plether</span>
          </Link>

          {/* Desktop Navigation */}
          <nav className="hidden lg:flex items-center gap-1">
            {navLinks.map(({ path, label }) => {
              const isActive = location.pathname === path
              return (
                <Link
                  key={path}
                  to={path}
                  className={`
                    px-3 py-2 rounded-lg text-sm font-medium transition-colors
                    ${
                      isActive
                        ? 'bg-surface-50 text-white'
                        : 'text-gray-400 hover:text-white hover:bg-surface-50/50'
                    }
                  `}
                >
                  {label}
                </Link>
              )
            })}
          </nav>

          {/* Right side: Price, Pending TX, Wallet */}
          <div className="flex items-center gap-4">
            <div className="hidden md:block">
              <PriceDisplay variant="compact" />
            </div>
            <PendingTxBadge />
            <ConnectButton />
          </div>
        </div>
      </div>
    </header>
  )
}
