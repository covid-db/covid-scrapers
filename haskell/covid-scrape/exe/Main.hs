{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

{-# LANGUAGE LambdaCase #-}

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
    runCommand env c
  where
    opts = info (commands <**> helper)
      (fullDesc <> header "COVID-19 data scraping tools")

data Command
  = All
  | JHU
--  | Alabama
  | Michigan
  | NewYork
  | Utah
  deriving (Eq,Ord,Show,Read,Enum,Bounded)

runCommand :: Env -> Command -> IO ()
runCommand env = \case
    All -> mapM_ (runCommand env) [succ All .. maxBound]
    JHU -> runScraper env
    --Alabama -> scrapeAlabama
    Michigan -> scrapeMichigan
    NewYork -> scrapeNewYork
    Utah -> scrapeUtah

commands :: Parser Command
commands = hsubparser
  (  command "all" (info (pure All)
       (progDesc "Run all scrapers"))
  <> command "jhu" (info (pure JHU)
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
