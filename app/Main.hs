{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import HardHat

import Options.Generic
import System.FilePath.Glob

newtype Glob = Glob {pattern' :: Pattern} deriving (Show)

instance ParseField Glob where
  parseField h m = Glob . compile <$> parseField h m

instance ParseRecord Glob where
  parseRecord = fmap getOnly parseRecord

instance ParseFields Glob where
  parseFields = parseField

instance ParseRecord Opts

--example:
data Opts = Opts
  { inputGlob :: Maybe Glob <?> "Input file pattern"

   --If no outputDir is supplied, send output to stdout.
  , outputDir :: Maybe FilePath <?> "Directory to output generated files"
  } deriving (Show, Generic)

main :: IO ()
main = do
  opts <- getRecord "test"
  print (opts :: Opts)
