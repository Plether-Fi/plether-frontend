interface SpinnerProps {
  size?: 'sm' | 'md' | 'lg'
  className?: string
}

const sizeStyles = {
  sm: 'h-4 w-4',
  md: 'h-6 w-6',
  lg: 'h-8 w-8',
}

export function Spinner({ size = 'md', className = '' }: SpinnerProps) {
  return (
    <div className={`${sizeStyles[size]} ${className} relative`}>
      <div className="absolute inset-0 rounded-full border-2 border-cyber-bright-blue/30 border-t-cyber-bright-blue animate-spin" />
    </div>
  )
}
