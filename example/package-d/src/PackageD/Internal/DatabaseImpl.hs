module PackageD.Internal.DatabaseImpl
  ( dbQuery
  ) where

import Database.SQLite3

dbQuery :: Database -> IO [String]
dbQuery _ = return ["row1", "row2"]
