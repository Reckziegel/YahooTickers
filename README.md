
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
#> # A tibble: 4,149 x 8
#>    date       tickers  open  high   low close    volume adjusted
#>    <date>     <fct>   <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl>
#>  1 2010-01-01 MMM      83.1  85.2  79.1  80.5  75208100     58.5
#>  2 2010-02-01 MMM      80.8  81.8  77.2  80.2  75020400     58.3
#>  3 2010-03-01 MMM      80.6  84.5  80.3  83.6  91066100     61.2
#>  4 2010-04-01 MMM      83.9  90.2  82.7  88.7  96407000     64.9
#>  5 2010-05-01 MMM      89.2  90.5  69.0  79.3 109573600     58.1
#>  6 2010-06-01 MMM      78.7  83    72.7  79.0 114407500     58.2
#>  7 2010-07-01 MMM      79.1  87.5  77.0  85.5  89556700     63.0
#>  8 2010-08-01 MMM      86.8  88.4  78.4  78.6  74721100     57.9
#>  9 2010-09-01 MMM      79.5  88    79.3  86.7  64059700     64.3
#> 10 2010-10-01 MMM      87.4  91.5  83.8  84.2  82038100     62.4
#> # ... with 4,139 more rows
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
#> # A tibble: 656 x 17
#>    date       tickers data    term  estimate model.desc sigma logLik   AIC   BIC
#>    <date>     <fct>   <list>  <fct>    <dbl> <fct>      <dbl>  <dbl> <dbl> <dbl>
#>  1 2018-06-01 COME.BA <tibbl~ alpha 0.000100 ETS(A,N,N) 0.136  -29.7  65.4  73.2
#>  2 2018-06-01 COME.BA <tibbl~ l     0.0256   ETS(A,N,N) 0.136  -29.7  65.4  73.2
#>  3 2018-07-01 COME.BA <tibbl~ alpha 0.000100 ETS(A,N,N) 0.137  -31.3  68.6  76.4
#>  4 2018-07-01 COME.BA <tibbl~ l     0.0234   ETS(A,N,N) 0.137  -31.3  68.6  76.4
#>  5 2018-08-01 COME.BA <tibbl~ alpha 0.000100 ETS(A,N,N) 0.138  -32.8  71.7  79.6
#>  6 2018-08-01 COME.BA <tibbl~ l     0.0255   ETS(A,N,N) 0.138  -32.8  71.7  79.6
#>  7 2018-09-01 COME.BA <tibbl~ alpha 0.000100 ETS(A,N,N) 0.137  -33.2  72.3  80.3
#>  8 2018-09-01 COME.BA <tibbl~ l     0.0256   ETS(A,N,N) 0.137  -33.2  72.3  80.3
#>  9 2018-10-01 COME.BA <tibbl~ alpha 0.000100 ETS(A,N,N) 0.137  -33.5  73.1  81.0
#> 10 2018-10-01 COME.BA <tibbl~ l     0.0260   ETS(A,N,N) 0.137  -33.5  73.1  81.0
#> # ... with 646 more rows, and 7 more variables: ME <dbl>, RMSE <dbl>,
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
#> # A tibble: 328 x 8
#>    date       tickers  ret_adj point_forecast  lo.80  lo.95 hi.80 hi.95
#>    <date>     <fct>      <dbl>          <dbl>  <dbl>  <dbl> <dbl> <dbl>
#>  1 2018-06-01 COME.BA -0.195           0.0256 -0.149 -0.241 0.200 0.292
#>  2 2018-07-01 COME.BA  0.238           0.0234 -0.152 -0.245 0.199 0.292
#>  3 2018-08-01 COME.BA  0.0387          0.0255 -0.151 -0.245 0.202 0.296
#>  4 2018-09-01 COME.BA  0.0620          0.0256 -0.150 -0.243 0.202 0.295
#>  5 2018-10-01 COME.BA -0.232           0.0260 -0.149 -0.242 0.201 0.294
#>  6 2018-11-01 COME.BA -0.0285          0.0235 -0.154 -0.248 0.201 0.295
#>  7 2018-12-01 COME.BA -0.0353          0.0230 -0.154 -0.247 0.200 0.293
#>  8 2019-01-01 COME.BA  0.0382          0.0225 -0.153 -0.246 0.198 0.291
#>  9 2019-02-01 COME.BA -0.0533          0.0226 -0.152 -0.245 0.198 0.290
#> 10 2019-03-01 COME.BA  0.00908         0.0219 -0.153 -0.245 0.196 0.289
#> # ... with 318 more rows
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
