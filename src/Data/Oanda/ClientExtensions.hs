{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Oanda.ClientExtensions
    ( ClientExtensions
    , prettyClientExtensions
    ) where

import           Control.DeepSeq
import           Data.Aeson
import qualified Data.Text        as T
import           GHC.Generics
import           Prelude          hiding ((<>))
import           Text.PrettyPrint

import           Data.Oanda.Types

data ClientExtensions = ClientExtensions
  { id      :: ClientId         -- ^ The Client ID of the Order/Trade
  , tag     :: ClientTag        -- ^ A tag associated with the Order/Trade
  , comment :: ClientComment    -- ^ A comment associated with the Order/Trade
  } deriving (Show, Eq, Ord, FromJSON, ToJSON, Generic, NFData)


prettyClientExtensions :: ClientExtensions -> Doc
prettyClientExtensions (ClientExtensions id tag comment) =
  text "ID " <> text (T.unpack id) <> comma <+> text "; TAG: " <> text (T.unpack tag) <> comma <+> "; COMMENT: " <> text (T.unpack comment)
