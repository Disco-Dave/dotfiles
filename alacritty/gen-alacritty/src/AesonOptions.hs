module AesonOptions (options) where

import Data.Aeson (Options (..), camelTo2, defaultOptions)

options :: Options
options =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = camelTo2 '_'
    }
