import { type ReactNode } from 'react'

interface Tab {
  id: string
  label: string
}

interface TabsProps {
  tabs: Tab[]
  activeTab: string
  onChange: (tabId: string) => void
}

export function Tabs({ tabs, activeTab, onChange }: TabsProps) {
  return (
    <div className="flex gap-1 p-1 bg-surface-200 rounded-lg">
      {tabs.map((tab) => (
        <button
          key={tab.id}
          onClick={() => onChange(tab.id)}
          className={`
            flex-1 px-4 py-2 text-sm font-medium rounded-md transition-all
            ${
              activeTab === tab.id
                ? 'bg-surface-50 text-white'
                : 'text-gray-400 hover:text-white'
            }
          `}
        >
          {tab.label}
        </button>
      ))}
    </div>
  )
}

interface TabPanelProps {
  children: ReactNode
  isActive: boolean
}

export function TabPanel({ children, isActive }: TabPanelProps) {
  if (!isActive) return null
  return <div>{children}</div>
}
