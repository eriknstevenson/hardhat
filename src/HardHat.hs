{-# LANGUAGE OverloadedStrings #-}

module HardHat
    ( someFunc
    ) where

import Data.Attoparsec.ByteString.Char8
import qualified Data.Text.Lazy as T
import Lucid

someFunc :: IO ()
someFunc =
  putStrLn . T.unpack . renderText $ p_ "how are you"
