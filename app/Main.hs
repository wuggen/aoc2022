module Main where

import Util (fromArgs, runApp)
import System.Environment (getArgs)
import AOC (Challenge)
import Data.Proxy (Proxy (..))

main :: IO ()
main = do
    args <- getArgs
    either putStrLn runApp (fromArgs (Proxy :: Proxy Challenge) args)
