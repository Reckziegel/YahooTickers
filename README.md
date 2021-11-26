
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
#> # A tibble: 4,209 x 8
#>    date       tickers  open  high   low close    volume adjusted
#>    <date>     <fct>   <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl>
#>  1 2010-01-01 MMM      83.1  85.2  79.1  80.5  75208100     58.1
#>  2 2010-02-01 MMM      80.8  81.8  77.2  80.2  75020400     57.8
#>  3 2010-03-01 MMM      80.6  84.5  80.3  83.6  91066100     60.7
#>  4 2010-04-01 MMM      83.9  90.2  82.7  88.7  96407000     64.4
#>  5 2010-05-01 MMM      89.2  90.5  69.0  79.3 109573600     57.6
#>  6 2010-06-01 MMM      78.7  83    72.7  79.0 114407500     57.7
#>  7 2010-07-01 MMM      79.1  87.5  77.0  85.5  89556700     62.5
#>  8 2010-08-01 MMM      86.8  88.4  78.4  78.6  74721100     57.4
#>  9 2010-09-01 MMM      79.5  88    79.3  86.7  64059700     63.8
#> 10 2010-10-01 MMM      87.4  91.5  83.8  84.2  82038100     61.9
#> # ... with 4,199 more rows
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
#> # A tibble: 672 x 17
#>    date       tickers data    term  estimate model.desc sigma logLik   AIC   BIC
#>    <date>     <fct>   <list>  <fct>    <dbl> <fct>      <dbl>  <dbl> <dbl> <dbl>
#>  1 2018-06-01 GGAL.BA <tibbl~ alpha 0.000100 ETS(A,N,N) 0.126  -21.9  49.9  57.7
#>  2 2018-06-01 GGAL.BA <tibbl~ l     0.0397   ETS(A,N,N) 0.126  -21.9  49.9  57.7
#>  3 2018-07-01 GGAL.BA <tibbl~ alpha 0.000100 ETS(A,N,N) 0.126  -22.8  51.6  59.4
#>  4 2018-07-01 GGAL.BA <tibbl~ l     0.0383   ETS(A,N,N) 0.126  -22.8  51.6  59.4
#>  5 2018-08-01 GGAL.BA <tibbl~ alpha 0.000100 ETS(A,N,N) 0.125  -23.0  52.0  59.9
#>  6 2018-08-01 GGAL.BA <tibbl~ l     0.0382   ETS(A,N,N) 0.125  -23.0  52.0  59.9
#>  7 2018-09-01 GGAL.BA <tibbl~ alpha 0.000100 ETS(A,N,N) 0.126  -24.1  54.2  62.1
#>  8 2018-09-01 GGAL.BA <tibbl~ l     0.0366   ETS(A,N,N) 0.126  -24.1  54.2  62.1
#>  9 2018-10-01 GGAL.BA <tibbl~ alpha 0.000100 ETS(A,N,N) 0.126  -25.2  56.4  64.4
#> 10 2018-10-01 GGAL.BA <tibbl~ l     0.0382   ETS(A,N,N) 0.126  -25.2  56.4  64.4
#> # ... with 662 more rows, and 7 more variables: ME <dbl>, RMSE <dbl>,
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
#> # A tibble: 336 x 8
#>    date       tickers ret_adj point_forecast  lo.80  lo.95 hi.80 hi.95
#>    <date>     <fct>     <dbl>          <dbl>  <dbl>  <dbl> <dbl> <dbl>
#>  1 2018-06-01 GGAL.BA -0.0998         0.0397 -0.122 -0.207 0.201 0.286
#>  2 2018-07-01 GGAL.BA  0.0244         0.0383 -0.123 -0.209 0.200 0.285
#>  3 2018-08-01 GGAL.BA -0.123          0.0382 -0.122 -0.207 0.199 0.284
#>  4 2018-09-01 GGAL.BA  0.205          0.0366 -0.125 -0.210 0.198 0.283
#>  5 2018-10-01 GGAL.BA -0.238          0.0382 -0.124 -0.209 0.200 0.286
#>  6 2018-11-01 GGAL.BA  0.202          0.0356 -0.129 -0.216 0.200 0.287
#>  7 2018-12-01 GGAL.BA  0.0136         0.0372 -0.128 -0.215 0.202 0.290
#>  8 2019-01-01 GGAL.BA  0.270          0.0369 -0.127 -0.215 0.201 0.288
#>  9 2019-02-01 GGAL.BA -0.114          0.0391 -0.127 -0.215 0.205 0.293
#> 10 2019-03-01 GGAL.BA -0.0873         0.0377 -0.129 -0.217 0.204 0.292
#> # ... with 326 more rows
```

Currently, `YahooTickers` supports the following stock indexes:

-   `dow`: Dow Jones Industrial Average (United States)
-   `sp500`: S&P 500 (United States)
-   `nasdaq`: NASDAQ Composite Index (United States)
-   `nyse`: NYSE Composite Index (United States)
-   `amex`: AMEX Composite Index (United States)
-   `russell2000`: Russell 2000 Index (United States)
-   `ftse100`: FTSE 100 Index (United Kingdom)
-   `dax`: Deutsche Boerse AG German Stock (Germany)
-   `cac40`: CAC 40 Index (France)
-   `bel20`: BEL 20 Index (Belgium)
-   `topix`: TOPIX Core 30 Index (Japan)
-   `hangseng`: HangSeng Composite Index (Hong Kong)
-   `sensex`: S&P BSE SENSEX Index (India)
-   `jakarta`: Jakarta Stock Exchange Stock Index (Indonesia)
-   `bursa`: Bursa Malaysia Bhd (Malaysia)
-   `nzx50`: S&P/NZX 50 Gross Index (New Zealand)
-   `kospi`: Korea Stock Exchange KOSPI Index (south Korea)
-   `taiex`: Taiwan Stock Exchange Weighted Index (Taiwan)
-   `tsx`: S&P/TSX Composite Index (Canada)
-   `ibovespa`: Ibovespa Brasil Sao Paulo Stock Exchange Index (Brazil)
-   `ipc`: S&P/BMV IPC (Mexico)
-   `ipsa`: S&P/CLX IPSA (Chile)
-   `merval`: Buenos Aires Stock Exchange Merval Index (Argentina)

## A Warning

The [Yahoo Finance](https://finance.yahoo.com/) API works reasonably
well but there are cases in which it does not cover historical data for
all stocks in an index universe. The `bel20` it’s a good example:
despite officially having 20 components, Yahoo Finance only maintains
data for 4 stocks. Thus, it is recommended to **always check the data
after downloading it!**
