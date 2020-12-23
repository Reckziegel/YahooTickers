
<!-- README.md is generated from README.Rmd. Please edit that file -->

# YahooTickers

[![Travis build
status](https://travis-ci.org/Reckziegel/YahooTickers.svg?branch=master)](https://travis-ci.org/Reckziegel/YahooTickers)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/Reckziegel/YahooTickers?branch=master&svg=true)](https://ci.appveyor.com/project/Reckziegel/YahooTickers)
[![Codecov test
coverage](https://codecov.io/gh/Reckziegel/YahooTickers/branch/master/graph/badge.svg)](https://codecov.io/gh/Reckziegel/YahooTickers?branch=master)
[![R-CMD-check](https://github.com/Reckziegel/YahooTickers/workflows/R-CMD-check/badge.svg)](https://github.com/Reckziegel/YahooTickers/actions)

The goal of YahooTickers is to easily get stock index constituents from
[Yahoo Finance](https://finance.yahoo.com/) without having to worry
about complicated `for` loops. This may be useful if you wish to focus
as quickly as possible in time-series *modeling*, but also convenient if
data is needed for it’s side effects (e.g. plotting).

The package depends heavly on
[quantmod](https://github.com/joshuaulrich/quantmod),
[rsample](https://github.com/topepo/rsample),
[sweep](https://github.com/business-science/sweep) and
[forecast](https://github.com/robjhyndman/forecast) projects.

## Instalation

You can install YahooTickers from github with:

``` r
# install.packages("devtools")
devtools::install_github("Reckziegel/YahooTickers")
```

## Examples

To download *all* the stock constituents of the Dow Jones Industrial
Average use `get_tickers()` and `get_stocks()` in one pipeline.

``` r
dow <- get_tickers(dow) %>% 
  get_stocks(., periodicity = "monthly")

dow 
#> # A tibble: 3,880 x 8
#>    date       tickers  open  high   low close    volume adjusted
#>    <date>     <fct>   <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl>
#>  1 2010-01-01 WMT      53.7  55.2  52.5  53.4 291591500     40.8
#>  2 2010-02-01 WMT      53.6  54.5  52.7  54.1 278505000     41.3
#>  3 2010-03-01 WMT      54.0  56.3  53.2  55.6 270764400     42.4
#>  4 2010-04-01 WMT      55.8  55.9  53.5  53.6 282951100     41.2
#>  5 2010-05-01 WMT      53.9  55.0  50    50.6 398119000     38.8
#>  6 2010-06-01 WMT      50.8  52.1  48.0  48.1 390548500     37.1
#>  7 2010-07-01 WMT      48.1  51.8  47.8  51.2 280340800     39.5
#>  8 2010-08-01 WMT      51.5  52.5  50    50.1 272559100     38.7
#>  9 2010-09-01 WMT      50.5  54.4  50.5  53.5 216284000     41.6
#> 10 2010-10-01 WMT      53.6  54.8  52.9  54.2 208445100     42.1
#> # ... with 3,870 more rows
```

With additional functions provided in the package it’s possible to fit a
model in a rolling or expanding time window. This is done by
`get_models()`.

``` r
library(forecast)

merval_models <- get_tickers(merval) %>%
  get_stocks(
    tickers     = ., 
    periodicity = "monthly",
    quiet       = TRUE
    ) %>% 
  get_returns(
    .tbl     = ., 
    .group   = tickers, 
    .type    = log, 
    .omit_na = TRUE, 
    ret_adj  = adjusted # change col names on the fly like dplyr!
    ) %>% 
  get_models(
    .tbl        = .,
    .group      = tickers, 
    .col        = ret_adj, 
    .initial    = 100, 
    .assess     = 1, 
    .cumulative = TRUE, 
    .fun        = ets
    ) 

merval_models
#> # A tibble: 496 x 17
#>    date       tickers data  term  estimate model.desc sigma logLik   AIC   BIC
#>    <date>     <fct>   <lis> <fct>    <dbl> <fct>      <dbl>  <dbl> <dbl> <dbl>
#>  1 2018-06-01 GGAL.BA <tib~ alpha 0.000100 ETS(A,N,N) 0.126  -22.5  50.9  58.7
#>  2 2018-06-01 GGAL.BA <tib~ l     0.0397   ETS(A,N,N) 0.126  -22.5  50.9  58.7
#>  3 2018-07-01 GGAL.BA <tib~ alpha 0.000100 ETS(A,N,N) 0.127  -23.3  52.6  60.4
#>  4 2018-07-01 GGAL.BA <tib~ l     0.0383   ETS(A,N,N) 0.127  -23.3  52.6  60.4
#>  5 2018-08-01 GGAL.BA <tib~ alpha 0.000100 ETS(A,N,N) 0.126  -23.5  53.1  60.9
#>  6 2018-08-01 GGAL.BA <tib~ l     0.0381   ETS(A,N,N) 0.126  -23.5  53.1  60.9
#>  7 2018-09-01 GGAL.BA <tib~ alpha 0.000100 ETS(A,N,N) 0.126  -24.6  55.2  63.1
#>  8 2018-09-01 GGAL.BA <tib~ l     0.0366   ETS(A,N,N) 0.126  -24.6  55.2  63.1
#>  9 2018-10-01 GGAL.BA <tib~ alpha 0.000100 ETS(A,N,N) 0.127  -25.7  57.5  65.4
#> 10 2018-10-01 GGAL.BA <tib~ l     0.0382   ETS(A,N,N) 0.127  -25.7  57.5  65.4
#> # ... with 486 more rows, and 7 more variables: ME <dbl>, RMSE <dbl>,
#> #   MAE <dbl>, MPE <dbl>, MAPE <dbl>, MASE <dbl>, ACF1 <dbl>
```

In this case, the the
[ETS](http://pkg.robjhyndman.com/forecast/reference/ets.html) function
was used to fit the first 100 observations on a expanding window,
`.cumulative = TRUE`. The output contains the fitted parameters,
selection criterias, as some traditional in-sample error metrics.

To extract the true out-of-sample forecasts use `get_forecasts()`. This
function automatically identifies the `.assess` argument in
`get_models()` and uses it as a target for the forecast horizon.

``` r
library(forecast)

merval_models  %>% 
  get_forecasts(.)
#> # A tibble: 248 x 8
#>    date       tickers ret_adj point_forecast  lo.80  lo.95 hi.80 hi.95
#>    <date>     <fct>     <dbl>          <dbl>  <dbl>  <dbl> <dbl> <dbl>
#>  1 2018-06-01 GGAL.BA -0.0998         0.0397 -0.122 -0.208 0.202 0.287
#>  2 2018-07-01 GGAL.BA  0.0244         0.0383 -0.124 -0.210 0.200 0.286
#>  3 2018-08-01 GGAL.BA -0.123          0.0381 -0.123 -0.209 0.200 0.285
#>  4 2018-09-01 GGAL.BA  0.205          0.0366 -0.125 -0.211 0.198 0.284
#>  5 2018-10-01 GGAL.BA -0.238          0.0382 -0.124 -0.210 0.201 0.287
#>  6 2018-11-01 GGAL.BA  0.202          0.0356 -0.130 -0.217 0.201 0.289
#>  7 2018-12-01 GGAL.BA  0.0136         0.0372 -0.129 -0.217 0.203 0.291
#>  8 2019-01-01 GGAL.BA  0.270          0.0369 -0.128 -0.216 0.202 0.290
#>  9 2019-02-01 GGAL.BA -0.114          0.0391 -0.128 -0.216 0.206 0.294
#> 10 2019-03-01 GGAL.BA -0.0873         0.0377 -0.130 -0.218 0.205 0.293
#> # ... with 238 more rows
```

Currently, `YahooTickers` supports the following stock indexes:

  - `dow`: Dow Jones Industrial Average (United States)
  - `sp500`: S\&P 500 (United States)
  - `nasdaq`: NASDAQ Composite Index (United States)
  - `nyse`: NYSE Composite Index (United States)
  - `amex`: AMEX Composite Index (United States)
  - `russell2000`: Russell 2000 Index (United States)
  - `ftse100`: FTSE 100 Index (United Kingdom)
  - `dax`: Deutsche Boerse AG German Stock (Germany)
  - `cac40`: CAC 40 Index (France)
  - `bel20`: BEL 20 Index (Belgium)
  - `topix`: TOPIX Core 30 Index (Japan)
  - `hangseng`: HangSeng Composite Index (Hong Kong)
  - `sensex`: S\&P BSE SENSEX Index (India)
  - `jakarta`: Jakarta Stock Exchange Stock Index (Indonesia)
  - `bursa`: Bursa Malaysia Bhd (Malaysia)
  - `nzx50`: S\&P/NZX 50 Gross Index (New Zealand)
  - `kospi`: Korea Stock Exchange KOSPI Index (south Korea)
  - `taiex`: Taiwan Stock Exchange Weighted Index (Taiwan)
  - `tsx`: S\&P/TSX Composite Index (Canada)
  - `ibovespa`: Ibovespa Brasil Sao Paulo Stock Exchange Index (Brazil)
  - `ipc`: S\&P/BMV IPC (Mexico)
  - `ipsa`: S\&P/CLX IPSA (Chile)
  - `merval`: Buenos Aires Stock Exchange Merval Index (Argentina)

## A Warning

The [Yahoo Finance](https://finance.yahoo.com/) API works reasonably
well but there are cases in which it does not cover historical data for
all stocks in an index universe. The `bel20` it’s a good example:
despite officially having 20 components, Yahoo Finance only maintains
data for 4 stocks. Thus, it is recommended to **always check the data
after downloading it\!**
