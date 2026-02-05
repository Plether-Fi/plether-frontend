module Main (main) where

import Plether.Api (app)
import Plether.Config (Config (..), loadConfig)
import Plether.Ethereum.Client (newClient)
import Web.Scotty (scotty)

main :: IO ()
main = do
  putStrLn "Loading configuration..."
  eConfig <- loadConfig

  case eConfig of
    Left err -> do
      putStrLn $ "Configuration error: " <> err
      putStrLn ""
      putStrLn "Required environment variables:"
      putStrLn "  RPC_URL      - Ethereum RPC endpoint (e.g., https://eth-sepolia.g.alchemy.com/v2/...)"
      putStrLn ""
      putStrLn "Optional environment variables:"
      putStrLn "  CHAIN_ID     - Chain ID (default: 11155111 for Sepolia)"
      putStrLn "  PORT         - Server port (default: 3001)"
      putStrLn "  CORS_ORIGINS - Space-separated CORS origins (default: http://localhost:5173)"
    Right cfg -> do
      putStrLn $ "Starting Plether API server on port " <> show (cfgPort cfg)
      putStrLn $ "Chain ID: " <> show (cfgChainId cfg)
      putStrLn ""
      putStrLn "Endpoints:"
      putStrLn "  GET /api/protocol/status"
      putStrLn "  GET /api/protocol/config"
      putStrLn "  GET /api/user/:address/dashboard"
      putStrLn "  GET /api/user/:address/balances"
      putStrLn "  GET /api/user/:address/positions"
      putStrLn "  GET /api/user/:address/allowances"
      putStrLn "  GET /api/quotes/mint?amount="
      putStrLn "  GET /api/quotes/burn?amount="
      putStrLn "  GET /api/quotes/zap?direction=&amount="
      putStrLn "  GET /api/quotes/trade?from=&amount="
      putStrLn "  GET /api/quotes/leverage?side=&principal=&leverage="
      putStrLn ""

      client <- newClient (cfgRpcUrl cfg)
      scotty (cfgPort cfg) (app client cfg)
