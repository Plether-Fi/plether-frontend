import { describe, it, expect, vi, beforeEach, type Mock } from 'vitest'
import { renderHook, act } from '@testing-library/react'
import { type ReactNode } from 'react'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'

// Mock wagmi before importing hooks
const mockWriteContract = vi.fn()
const mockUseWriteContract = vi.fn()
const mockUseWaitForTransactionReceipt = vi.fn()
const mockUseAccount = vi.fn()
const mockUseReadContract = vi.fn()

vi.mock('wagmi', () => ({
  useAccount: () => mockUseAccount(),
  useWriteContract: () => mockUseWriteContract(),
  useWaitForTransactionReceipt: () => mockUseWaitForTransactionReceipt(),
  useReadContract: () => mockUseReadContract(),
}))

// Mock the transaction store
const mockAddTransaction = vi.fn()
const mockUpdateTransaction = vi.fn()

vi.mock('../../stores/transactionStore', () => ({
  useTransactionStore: (selector: (state: { addTransaction: Mock; updateTransaction: Mock }) => unknown) =>
    selector({ addTransaction: mockAddTransaction, updateTransaction: mockUpdateTransaction }),
}))

// Mock crypto.randomUUID
vi.stubGlobal('crypto', {
  randomUUID: () => 'test-uuid-123',
})

// Import hooks after mocking
import { useMint, useBurn, usePreviewMint, usePreviewBurn } from '../usePlethCore'

function createWrapper() {
  const queryClient = new QueryClient({
    defaultOptions: { queries: { retry: false } },
  })
  return function Wrapper({ children }: { children: ReactNode }) {
    return <QueryClientProvider client={queryClient}>{children}</QueryClientProvider>
  }
}

describe('useMint', () => {
  beforeEach(() => {
    vi.clearAllMocks()

    // Default mock implementations
    mockUseAccount.mockReturnValue({ chainId: 11155111 })
    mockUseWriteContract.mockReturnValue({
      writeContract: mockWriteContract,
      data: undefined,
      isPending: false,
      error: null,
      reset: vi.fn(),
    })
    mockUseWaitForTransactionReceipt.mockReturnValue({
      isLoading: false,
      isSuccess: false,
      isError: false,
      error: null,
    })
  })

  it('returns initial state correctly', () => {
    const { result } = renderHook(() => useMint(), { wrapper: createWrapper() })

    expect(result.current.isPending).toBe(false)
    expect(result.current.isConfirming).toBe(false)
    expect(result.current.isSuccess).toBe(false)
    expect(result.current.error).toBeNull()
    expect(result.current.hash).toBeUndefined()
    expect(typeof result.current.mint).toBe('function')
  })

  it('does nothing when chainId is undefined', async () => {
    mockUseAccount.mockReturnValue({ chainId: undefined })

    const { result } = renderHook(() => useMint(), { wrapper: createWrapper() })

    await act(async () => {
      await result.current.mint(1000n)
    })

    expect(mockWriteContract).not.toHaveBeenCalled()
    expect(mockAddTransaction).not.toHaveBeenCalled()
  })

  it('calls writeContract with correct parameters', async () => {
    const { result } = renderHook(() => useMint(), { wrapper: createWrapper() })

    await act(async () => {
      await result.current.mint(1000n)
    })

    expect(mockAddTransaction).toHaveBeenCalledWith({
      id: 'test-uuid-123',
      type: 'mint',
      status: 'pending',
      hash: undefined,
      description: 'Minting DXY-BEAR + DXY-BULL',
    })

    expect(mockWriteContract).toHaveBeenCalledWith(
      expect.objectContaining({
        functionName: 'mint',
        args: [1000n],
      }),
      expect.objectContaining({
        onSuccess: expect.any(Function),
        onError: expect.any(Function),
      })
    )
  })

  it('updates transaction on success callback', async () => {
    let capturedOnSuccess: ((hash: string) => void) | undefined

    mockWriteContract.mockImplementation((_config, options) => {
      capturedOnSuccess = options.onSuccess
    })

    const { result } = renderHook(() => useMint(), { wrapper: createWrapper() })

    await act(async () => {
      await result.current.mint(1000n)
    })

    expect(capturedOnSuccess).toBeDefined()

    act(() => {
      capturedOnSuccess!('0xabcd1234')
    })

    expect(mockUpdateTransaction).toHaveBeenCalledWith('test-uuid-123', {
      hash: '0xabcd1234',
      status: 'confirming',
    })
  })

  it('updates transaction on error callback', async () => {
    let capturedOnError: ((error: Error) => void) | undefined

    mockWriteContract.mockImplementation((_config, options) => {
      capturedOnError = options.onError
    })

    const { result } = renderHook(() => useMint(), { wrapper: createWrapper() })

    await act(async () => {
      await result.current.mint(1000n)
    })

    expect(capturedOnError).toBeDefined()

    act(() => {
      capturedOnError!(new Error('User rejected'))
    })

    expect(mockUpdateTransaction).toHaveBeenCalledWith('test-uuid-123', {
      status: 'failed',
      errorMessage: expect.any(String),
    })
  })

  it('reflects isPending state from useWriteContract', () => {
    mockUseWriteContract.mockReturnValue({
      writeContract: mockWriteContract,
      data: undefined,
      isPending: true,
      error: null,
      reset: vi.fn(),
    })

    const { result } = renderHook(() => useMint(), { wrapper: createWrapper() })

    expect(result.current.isPending).toBe(true)
  })

  it('reflects isConfirming state from useWaitForTransactionReceipt', () => {
    mockUseWriteContract.mockReturnValue({
      writeContract: mockWriteContract,
      data: '0xhash',
      isPending: false,
      error: null,
      reset: vi.fn(),
    })
    mockUseWaitForTransactionReceipt.mockReturnValue({
      isLoading: true,
      isSuccess: false,
      isError: false,
      error: null,
    })

    const { result } = renderHook(() => useMint(), { wrapper: createWrapper() })

    expect(result.current.isConfirming).toBe(true)
    expect(result.current.hash).toBe('0xhash')
  })

  it('reflects isSuccess state from useWaitForTransactionReceipt', () => {
    mockUseWriteContract.mockReturnValue({
      writeContract: mockWriteContract,
      data: '0xhash',
      isPending: false,
      error: null,
      reset: vi.fn(),
    })
    mockUseWaitForTransactionReceipt.mockReturnValue({
      isLoading: false,
      isSuccess: true,
      isError: false,
      error: null,
    })

    const { result } = renderHook(() => useMint(), { wrapper: createWrapper() })

    expect(result.current.isSuccess).toBe(true)
  })
})

describe('useBurn', () => {
  beforeEach(() => {
    vi.clearAllMocks()

    mockUseAccount.mockReturnValue({ chainId: 11155111 })
    mockUseWriteContract.mockReturnValue({
      writeContract: mockWriteContract,
      data: undefined,
      isPending: false,
      error: null,
      reset: vi.fn(),
    })
    mockUseWaitForTransactionReceipt.mockReturnValue({
      isLoading: false,
      isSuccess: false,
      isError: false,
      error: null,
    })
  })

  it('returns initial state correctly', () => {
    const { result } = renderHook(() => useBurn(), { wrapper: createWrapper() })

    expect(result.current.isPending).toBe(false)
    expect(result.current.isConfirming).toBe(false)
    expect(result.current.isSuccess).toBe(false)
    expect(result.current.error).toBeNull()
    expect(result.current.hash).toBeUndefined()
    expect(typeof result.current.burn).toBe('function')
  })

  it('does nothing when chainId is undefined', async () => {
    mockUseAccount.mockReturnValue({ chainId: undefined })

    const { result } = renderHook(() => useBurn(), { wrapper: createWrapper() })

    await act(async () => {
      await result.current.burn(1000n)
    })

    expect(mockWriteContract).not.toHaveBeenCalled()
    expect(mockAddTransaction).not.toHaveBeenCalled()
  })

  it('calls writeContract with correct parameters', async () => {
    const { result } = renderHook(() => useBurn(), { wrapper: createWrapper() })

    await act(async () => {
      await result.current.burn(1000n)
    })

    expect(mockAddTransaction).toHaveBeenCalledWith({
      id: 'test-uuid-123',
      type: 'burn',
      status: 'pending',
      hash: undefined,
      description: 'Redeeming DXY-BEAR + DXY-BULL for USDC',
    })

    expect(mockWriteContract).toHaveBeenCalledWith(
      expect.objectContaining({
        functionName: 'burn',
        args: [1000n],
      }),
      expect.objectContaining({
        onSuccess: expect.any(Function),
        onError: expect.any(Function),
      })
    )
  })

  it('updates transaction on success callback', async () => {
    let capturedOnSuccess: ((hash: string) => void) | undefined

    mockWriteContract.mockImplementation((_config, options) => {
      capturedOnSuccess = options.onSuccess
    })

    const { result } = renderHook(() => useBurn(), { wrapper: createWrapper() })

    await act(async () => {
      await result.current.burn(1000n)
    })

    expect(capturedOnSuccess).toBeDefined()

    act(() => {
      capturedOnSuccess!('0xabcd1234')
    })

    expect(mockUpdateTransaction).toHaveBeenCalledWith('test-uuid-123', {
      hash: '0xabcd1234',
      status: 'confirming',
    })
  })

  it('updates transaction on error callback', async () => {
    let capturedOnError: ((error: Error) => void) | undefined

    mockWriteContract.mockImplementation((_config, options) => {
      capturedOnError = options.onError
    })

    const { result } = renderHook(() => useBurn(), { wrapper: createWrapper() })

    await act(async () => {
      await result.current.burn(1000n)
    })

    expect(capturedOnError).toBeDefined()

    act(() => {
      capturedOnError!(new Error('User rejected'))
    })

    expect(mockUpdateTransaction).toHaveBeenCalledWith('test-uuid-123', {
      status: 'failed',
      errorMessage: expect.any(String),
    })
  })
})

describe('usePreviewMint', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mockUseAccount.mockReturnValue({ chainId: 11155111 })
  })

  it('returns loading state initially', () => {
    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: true,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => usePreviewMint(1000n), { wrapper: createWrapper() })

    expect(result.current.isLoading).toBe(true)
    expect(result.current.usdcRequired).toBe(0n)
  })

  it('returns data when loaded', () => {
    mockUseReadContract.mockReturnValue({
      data: [200000000n, 180000000n, 20000000n],
      isLoading: false,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => usePreviewMint(1000n), { wrapper: createWrapper() })

    expect(result.current.isLoading).toBe(false)
    expect(result.current.usdcRequired).toBe(200000000n)
    expect(result.current.error).toBeNull()
  })

  it('returns 0 when no chainId', () => {
    mockUseAccount.mockReturnValue({ chainId: undefined })
    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: false,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => usePreviewMint(1000n), { wrapper: createWrapper() })

    expect(result.current.usdcRequired).toBe(0n)
  })
})

describe('usePreviewBurn', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mockUseAccount.mockReturnValue({ chainId: 11155111 })
  })

  it('returns loading state initially', () => {
    mockUseReadContract.mockReturnValue({
      data: undefined,
      isLoading: true,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => usePreviewBurn(1000n), { wrapper: createWrapper() })

    expect(result.current.isLoading).toBe(true)
    expect(result.current.usdcToReturn).toBe(0n)
  })

  it('returns data when loaded', () => {
    mockUseReadContract.mockReturnValue({
      data: [195000000n, 175000000n],
      isLoading: false,
      error: null,
      refetch: vi.fn(),
    })

    const { result } = renderHook(() => usePreviewBurn(1000n), { wrapper: createWrapper() })

    expect(result.current.isLoading).toBe(false)
    expect(result.current.usdcToReturn).toBe(195000000n)
    expect(result.current.error).toBeNull()
  })
})
