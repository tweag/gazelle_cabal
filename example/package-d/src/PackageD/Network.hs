module PackageD.Network
  ( fetchData
  ) where

import Network.Socket (HostName)
import PackageD.Internal.NetworkImpl

fetchData :: HostName -> IO String
fetchData host = networkFetch host
