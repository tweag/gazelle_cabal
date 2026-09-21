module PackageD.Internal.NetworkImpl
  ( networkFetch
  ) where

import Network.Socket (HostName)

networkFetch :: HostName -> IO String
networkFetch host = return $ "Data from " ++ host
