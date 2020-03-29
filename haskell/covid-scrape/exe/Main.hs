{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

------------------------------------------------------------------------------
import Data.Time
import Options.Applicative
------------------------------------------------------------------------------
import Covid19
------------------------------------------------------------------------------


main :: IO ()
main = do
    now <- getCurrentTime
    let env = Env now
    c <- execParser opts
    case c of
      JHU -> runScraper env
  where
    opts = info (commands <**> helper)
      (fullDesc <> header "COVID-19 data scraping tools")

data Command = JHU

commands :: Parser Command
commands = hsubparser
  (  command "jhu" (info (pure JHU)
       (progDesc "Johns Hopkins dataset"))
  )
