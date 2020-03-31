# covid-scrapers

**Help Needed:** We need help writing scrapers for individual state data.  You
don't have to use Haskell.  Any language will do.  To get started, [look at
the list of state data
sources](https://github.com/covid-db/covid-db/blob/master/state-sources.md).
Pick one that interests you, figure out where the data is, and start coding!

We would like to get the states with larger numbers of cases done first because
that data is more statistically significant. Look in the project backlog for a
sorted list of TODO states:

https://github.com/covid-db/covid-scrapers/projects/1

We started with U.S. data but we would like to expand to include international
data (at the same level of granularity if possible) as well. If you would like
to organize the scraping efforts for a whole country, please reach out.


## Completed Scrapers

* [Johns Hopkins Dataset](https://github.com/covid-db/covid-scrapers/blob/master/haskell/covid-scrape/lib/Covid19.hs#L80)

### USA

* [Michigan](haskell/covid-scrape/lib/Covid19/USA/Michigan.hs)
* [New York](haskell/covid-scrape/lib/Covid19/USA/NewYork.hs)
* [Utah](haskell/covid-scrape/lib/Covid19/USA/Utah.hs)
