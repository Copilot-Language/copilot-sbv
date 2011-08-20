--------------------------------------------------------------------------------
-- Copyright © 2011 National Institute of Aerospace / Galois, Inc.
--------------------------------------------------------------------------------

module Copilot.Compile.SBV.Params (Params (..), defaultParams, withPrefix) where

data Params = Params
  { prefix :: Maybe String
  }

defaultParams :: Params
defaultParams = Params
  { prefix = Nothing
  }

withPrefix :: Maybe String -> String -> String
withPrefix (Just cs) ds = cs ++ "_" ++ ds
withPrefix _         ds = ds
