module PackageD.Database
  ( queryData
  ) where

import Database.SQLite.Simple
import PackageD.Internal.DatabaseImpl

queryData :: Connection -> IO [String]
queryData = dbQuery
