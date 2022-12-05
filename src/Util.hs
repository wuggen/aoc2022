module Util
  ( FromArgs (..)
  , RunApp (..)
  ) where

import Data.Data (Proxy)

class FromArgs a where
    fromArgsIncremental :: [String] -> Either String (a, [String])

    fromArgs :: Proxy a -> [String] -> Either String a
    fromArgs _ = fmap fst . fromArgsIncremental

class FromArgs a => RunApp a where
    runApp :: a -> IO ()
