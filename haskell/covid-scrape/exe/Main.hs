{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

------------------------------------------------------------------------------
import Data.Time
import Options.Applicative
------------------------------------------------------------------------------
import Covid19
--import Covid19.USA.Alabama
import Covid19.USA.Michigan
import Covid19.USA.NewYork
import Covid19.USA.Utah
------------------------------------------------------------------------------


main :: IO ()
main = do
    now <- getCurrentTime
    let env = Env now
    c <- execParser opts
    case c of
      JHU -> runScraper env
      Utah -> scrapeUtah
      NewYork -> scrapeNewYork
      Michigan -> scrapeMichigan
      --Alabama -> scrapeAlabama
  where
    opts = info (commands <**> helper)
      (fullDesc <> header "COVID-19 data scraping tools")

data Command
  = JHU
--  | Alabama
  | Michigan
  | NewYork
  | Utah

commands :: Parser Command
commands = hsubparser
  (  command "jhu" (info (pure JHU)
       (progDesc "Johns Hopkins dataset"))
--  <> command "alabama" (info (pure Alabama)
--       (progDesc "Alabama cases"))
  <> command "michigan" (info (pure Michigan)
       (progDesc "Michigan cases"))
  <> command "newyork" (info (pure NewYork)
       (progDesc "New York cases"))
  <> command "utah" (info (pure Utah)
       (progDesc "Utah cases"))
  )
