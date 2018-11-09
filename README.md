
<!-- README.md is generated from README.Rmd. Please edit that file -->
YahooTickers
============

[![Travis-CI Build Status](https://travis-ci.org/Reckziegel/YahooTickers.svg?branch=master)](https://travis-ci.org/Reckziegel/YahooTickers)

The goal of YahooTickers is to help users to easily get stock index constituents from [Yahoo Finance](https://finance.yahoo.com/) without having to worry about complicated `for` loops. This may be useful if you wish to focus as quickly as possible in time-series *modeling*, but also convenient if data is needed for it's side effects (e.g. plotting).

The package depends heavly on [quantmod](https://github.com/joshuaulrich/quantmod), [rsample](https://github.com/topepo/rsample), [sweep](https://github.com/business-science/sweep) and [forecast](https://github.com/robjhyndman/forecast) projects.

Instalation
-----------

You can install YahooTickers from github with:

``` r
# install.packages("devtools")
devtools::install_github("Reckziegel/YahooTickers")
```

Examples
--------

Suppose we want to download *all* the stock constituents of the Dow Jones Industrial Average. This can easily be done with `get_tickers()` and `get_stock()` in one pipeline.

``` r
library(YahooTickers)
library(dplyr)

get_tickers(dow) %>% 
  get_stocks(., periodicity = "monthly")
#> # A time tibble: 3,210 x 8
#> # Index: index
#>    index      tickers  open  high   low close    volume adjusted
#>    <date>     <chr>   <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl>
#>  1 2010-01-01 JNJ      64.7  65.9  62.2  62.9 245125300     48.1
#>  2 2010-02-01 JNJ      63.3  64.7  61.9  63   213341500     48.2
#>  3 2010-03-01 JNJ      63.3  65.5  63.0  65.2 249947500     50.3
#>  4 2010-04-01 JNJ      65.4  66.2  64.2  64.3 235860700     49.6
#>  5 2010-05-01 JNJ      65.2  65.6  58.2  58.3 357775800     45.0
#>  6 2010-06-01 JNJ      58.2  60.2  57.5  59.1 357343400     46.0
#>  7 2010-07-01 JNJ      59.1  61.7  56.9  58.1 310708000     45.2
#>  8 2010-08-01 JNJ      58.5  60.2  57.0  57.0 239384600     44.4
#>  9 2010-09-01 JNJ      57.7  62.7  57.4  62.0 209239500     48.7
#> 10 2010-10-01 JNJ      62.1  64.2  61.3  63.7 208721800     50.1
#> # ... with 3,200 more rows
```

Usign additional functions provided in the package it's possible to fit a model in a rolling or expanding time window. This is done by `get_models()`.

``` r
library(YahooTickers)
library(dplyr)
library(forecast)

get_tickers(merval) %>%
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
    adjusted
    ) %>% 
  get_models(
    .tbl        = .,
    .group      = tickers, 
    .col        = return_adjusted, 
    .initial    = 100, 
    .assess     = 1, 
    .cumulative = TRUE, 
    .fun        = ets
    ) 
#> # A time tibble: 132 x 16
#> # Index: index
#>    index      tickers term  estimate model.desc sigma logLik   AIC   BIC
#>    <date>     <fct>   <fct>    <dbl> <fct>      <dbl>  <dbl> <dbl> <dbl>
#>  1 2018-06-01 FRAN.BA alpha 0.000100 ETS(A,N,N) 0.136  -29.9  65.8  73.6
#>  2 2018-06-01 FRAN.BA l     0.0282   ETS(A,N,N) 0.136  -29.9  65.8  73.6
#>  3 2018-07-01 FRAN.BA alpha 0.000100 ETS(A,N,N) 0.136  -30.2  66.4  74.3
#>  4 2018-07-01 FRAN.BA l     0.0280   ETS(A,N,N) 0.136  -30.2  66.4  74.3
#>  5 2018-08-01 FRAN.BA alpha 0.000100 ETS(A,N,N) 0.135  -30.6  67.1  75.0
#>  6 2018-08-01 FRAN.BA l     0.0283   ETS(A,N,N) 0.135  -30.6  67.1  75.0
#>  7 2018-09-01 FRAN.BA alpha 0.000100 ETS(A,N,N) 0.135  -31.7  69.3  77.3
#>  8 2018-09-01 FRAN.BA l     0.0267   ETS(A,N,N) 0.135  -31.7  69.3  77.3
#>  9 2018-10-01 FRAN.BA alpha 0.000100 ETS(A,N,N) 0.139  -35.2  76.5  84.4
#> 10 2018-10-01 FRAN.BA l     0.0301   ETS(A,N,N) 0.139  -35.2  76.5  84.4
#> # ... with 122 more rows, and 7 more variables: ME <dbl>, RMSE <dbl>,
#> #   MAE <dbl>, MPE <dbl>, MAPE <dbl>, MASE <dbl>, ACF1 <dbl>
```

In this case, the the [ETS](http://pkg.robjhyndman.com/forecast/reference/ets.html) function was used to fit the first 100 observations on a expanding window, `cumulative = TRUE`. The output contains the fitted parameters, selection criterias, as some in-sample error metrics.

To extract the true out-of-sample forecasts use `get_forecasts()`. This function automatically identifies the `.assess` argument and uses it as a target for the forecast horizon.

``` r
library(YahooTickers)
library(dplyr)
library(forecast)

get_tickers(merval) %>%
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
    adjusted
    ) %>% 
  get_models(
    .tbl        = .,
    .group      = tickers, 
    .col        = return_adjusted, 
    .initial    = 100, 
    .assess     = 1, 
    .cumulative = TRUE, 
    .fun        = ets
    )  %>% 
  get_forecasts(.)
#> # A time tibble: 66 x 8
#> # Index: index
#>    index      tickers return_adjusted mean_forecast  lo.80  lo.95 hi.80
#>    <date>     <fct>             <dbl>         <dbl>  <dbl>  <dbl> <dbl>
#>  1 2018-06-01 FRAN.BA         0.00717        0.0282 -0.146 -0.239 0.203
#>  2 2018-07-01 FRAN.BA         0.0617         0.0280 -0.146 -0.238 0.202
#>  3 2018-08-01 FRAN.BA        -0.144          0.0283 -0.145 -0.236 0.201
#>  4 2018-09-01 FRAN.BA         0.375          0.0267 -0.147 -0.239 0.200
#>  5 2018-10-01 FRAN.BA        -0.192          0.0301 -0.148 -0.242 0.208
#>  6 2018-11-01 FRAN.BA         0.0660         0.0278 -0.152 -0.247 0.207
#>  7 2018-06-01 TS.BA           0.147          0.0170 -0.122 -0.196 0.156
#>  8 2018-07-01 TS.BA          -0.0332         0.0183 -0.121 -0.195 0.158
#>  9 2018-08-01 TS.BA           0.213          0.0178 -0.121 -0.195 0.157
#> 10 2018-09-01 TS.BA           0.108          0.0197 -0.121 -0.195 0.160
#> # ... with 56 more rows, and 1 more variable: hi.95 <dbl>
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

A Warning
---------

The [Yahoo Finance](https://finance.yahoo.com/) API works reasonably well but there are cases in which it does not cover historical data for all stocks in an index universe. The `bel20` it's a good example: despite officially having 20 components, Yahoo Finance only maintains data for 4 stocks. Thus, it is recommended to **always check the data after downloading it!**
