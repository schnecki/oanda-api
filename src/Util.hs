

module Util where

import           Data.Aeson
import           Data.Char
import           Data.Serialize (Serialize (..))
import           Data.Time

-- The following code is from https://github.com/jdreaver/oanda-rest-api/

-- | Aeson Options that remove the prefix from fields
unPrefix :: String -> Options
unPrefix prefix = defaultOptions
  { fieldLabelModifier = unCapitalize . dropPrefix prefix
  , omitNothingFields = True
  }

-- | Lower case leading character
unCapitalize :: String -> String
unCapitalize []     = []
unCapitalize (c:cs) = toLower c : cs

-- | Remove given prefix
dropPrefix :: String -> String -> String
dropPrefix prefix input = go prefix input
  where
    go pre [] = error $ contextual $ "prefix leftover: " <> pre
    go [] (c:cs) = c : cs
    go (p:preRest) (c:cRest)
      | p == c = go preRest cRest
      | otherwise = error $ contextual $ "not equal: " <>  (p:preRest)  <> " " <> (c:cRest)

    contextual msg = "dropPrefix: " <> msg <> ". " <> prefix <> " " <> input


-- | 'Serialize' instance for 'UTCTime'.
instance Serialize UTCTime where
  put (UTCTime day dayTime) = put day >> put dayTime
  get = UTCTime <$> get <*> get

-- | 'Serialize' instance for 'DiffTime'.
instance Serialize DiffTime where
  put = put . diffTimeToPicoseconds
  get = picosecondsToDiffTime <$> get

-- | 'Serialize' instance for 'Day'.
instance Serialize Day where
  put = put . toModifiedJulianDay
  get = ModifiedJulianDay <$> get
