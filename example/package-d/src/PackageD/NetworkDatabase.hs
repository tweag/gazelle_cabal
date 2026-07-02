module PackageD.NetworkDatabase
  ( syncData
  ) where

import Database.SQLite3
import Network.Socket (HostName)
import PackageD.Network
import PackageD.Database

syncData :: HostName -> Database -> IO ()
syncData host conn = do
  _ <- fetchData host
  _ <- queryData conn
  putStrLn $ "Synced data from " ++ host
