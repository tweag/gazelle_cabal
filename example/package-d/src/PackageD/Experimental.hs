module PackageD.Experimental
  ( experimentalFeature
  ) where

import Data.Text (Text)
import qualified Data.Text as T

experimentalFeature :: Text -> Text
experimentalFeature = T.toUpper
