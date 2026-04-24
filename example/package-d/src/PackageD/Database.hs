module PackageD.Database
  ( queryData
  ) where

import Database.SQLite3
import PackageD.Internal.DatabaseImpl

queryData :: Database -> IO [String]
queryData = dbQuery
