module PackageD.NetworkDatabase
  ( syncData
  ) where

import Network.Socket (HostName)
import Database.SQLite.Simple
import PackageD.Network
import PackageD.Database

syncData :: HostName -> Connection -> IO ()
syncData host conn = do
  _ <- fetchData host
  _ <- queryData conn
  putStrLn $ "Synced data from " ++ host
