module PackageD.Internal.DatabaseImpl
  ( dbQuery
  ) where

import Database.SQLite.Simple

dbQuery :: Connection -> IO [String]
dbQuery _ = return ["row1", "row2"]
