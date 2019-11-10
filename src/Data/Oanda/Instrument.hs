{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Data.Oanda.Instrument
    ( Instrument (..)

    ) where

import           Control.DeepSeq
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Text                       (Text)
import           Data.Time
import           Data.Time.RFC3339
import           GHC.Generics

import           Data.Oanda.DecimalNumber
import           Data.Oanda.InstrumentCommission
import           Data.Oanda.InstrumentType
import           Types

data Instrument = Instrument
  { name                        :: InstrumentName             -- ^ The name of the Instrument
  , instrumentType              :: InstrumentType             -- ^ The type of the Instrument
  , displayName                 :: Text                       -- ^ The display name of the Instrument
  , pipLocation                 :: Int                        -- ^ The location of the “pip” for this instrument. The
                                                              -- decimal position of the pip in this Instrument’s price
                                                              -- can be found at 10 ^ pipLocation (e.g. -4 pipLocation
                                                              -- results in a decimal pip position of 10 ^ -4 = 0.0001).
  , displayPrecision            :: Int                        -- ^ The number of decimal places that should be used to
                                                              -- display prices for this instrument. (e.g. a
                                                              -- displayPrecision of 5 would result in a price of “1”
                                                              -- being displayed as “1.00000”)
  , tradeUnitsPrecision         :: Int                        -- ^ The amount of decimal places that may be provided
                                                              -- when specifying the number of units traded for this
                                                              -- instrument.
  , minimumTradeSize            :: DecimalNumber              -- ^ The smallest number of units allowed to be traded for
                                                              -- this instrument.
  , maximumTrailingStopDistance :: DecimalNumber              -- ^ The maximum trailing stop distance allowed for a trailing
                                                              -- stop loss created for this instrument. Specified in price
                                                              -- units.
  , minimumTrailingStopDistance :: DecimalNumber              -- ^ The minimum trailing stop distance allowed for a
                                                              -- trailing stop loss created for this instrument.
                                                              -- Specified in price units.
  , maximumPositionSize         :: DecimalNumber              -- ^ The maximum position size allowed for this
                                                              -- instrument. Specified in units.
  , maximumOrderUnits           :: DecimalNumber              -- ^ The maximum units allowed for an Order placed for
                                                              -- this instrument. Specified in units.
  , marginRate                  :: DecimalNumber              -- ^ The margin rate for this instrument.
  , commission                  :: Maybe InstrumentCommission -- ^ The commission structure for this instrument.
  } deriving (Generic, Show, Eq, Ord, NFData)

-- Derive from JSON by modifying the NAV-nav field.
$(deriveFromJSON
    defaultOptions
      { fieldLabelModifier =
          let f "instrumentType" = "type"
              f x                = x
           in f
      }
    ''Instrument)

