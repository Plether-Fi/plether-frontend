import { Link, useLocation } from 'react-router-dom'

const navLinks = [
  { path: '/', label: 'Trade', icon: 'swap_horiz' },
  { path: '/stake', label: 'Stake', icon: 'paid' },
  { path: '/mint', label: 'Mint', icon: 'add' },
]

export function MobileNav() {
  const location = useLocation()

  return (
    <nav className="lg:hidden fixed bottom-0 left-0 right-0 z-40 bg-cyber-surface-dark border-t border-cyber-border-glow/30 safe-area-bottom shadow-lg shadow-cyber-border-glow/10">
      <div className="flex items-center justify-around h-16">
        {navLinks.map(({ path, label, icon }) => {
          const isActive = location.pathname === path ||
            (path === '/' && ['/', '/leverage', '/yield'].includes(location.pathname))
          return (
            <Link
              key={path}
              to={path}
              className={`
                flex flex-col items-center gap-1 px-4 py-2 rounded-lg transition-colors
                ${isActive
                  ? 'text-cyber-neon-green bg-cyber-neon-green/10'
                  : 'text-cyber-text-secondary hover:text-cyber-bright-blue'
                }
              `}
            >
              <span className="material-symbols-outlined text-xl">{icon}</span>
              <span className="text-xs font-medium">{label}</span>
            </Link>
          )
        })}
      </div>
    </nav>
  )
}
