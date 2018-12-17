
<!-- README.md is generated from README.Rmd. Please edit that file -->
YahooTickers
============

[![Travis build status](https://travis-ci.org/Reckziegel/YahooTickers.svg?branch=master)](https://travis-ci.org/Reckziegel/YahooTickers) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/Reckziegel/YahooTickers?branch=master&svg=true)](https://ci.appveyor.com/project/Reckziegel/YahooTickers)

The goal of YahooTickers is to easily get stock index constituents from [Yahoo Finance](https://finance.yahoo.com/) without having to worry about complicated `for` loops. This may be useful if you wish to focus as quickly as possible in time-series *modeling*, but also convenient if data is needed for it's side effects (e.g. plotting).

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

To download *all* the stock constituents of the Dow Jones Industrial Average use `get_tickers()` and `get_stocks()` in one pipeline.

``` r
get_tickers(dow) %>% 
  get_stocks(., periodicity = "monthly")
#> # A time tibble: 3,270 x 8
#> # Index: index
#>    index      tickers  open  high   low close    volume adjusted
#>    <date>     <fct>   <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl>
#>  1 2010-01-01 VZ       31.3  31.4  27.3  27.6 505361100     17.8
#>  2 2010-02-01 VZ       28.0  28.0  26.6  27.1 348600600     17.8
#>  3 2010-03-01 VZ       27.2  29.5  27.1  29.1 409463500     19.1
#>  4 2010-04-01 VZ       29.3  29.7  26.8  27.1 503870600     17.8
#>  5 2010-05-01 VZ       27.3  27.6  24.8  25.8 515564500     17.2
#>  6 2010-06-01 VZ       25.6  27.6  25.4  26.3 447252100     17.5
#>  7 2010-07-01 VZ       26.3  29.2  25.9  29.1 497527600     19.4
#>  8 2010-08-01 VZ       29.5  30.4  29.1  29.5 381693500     20.0
#>  9 2010-09-01 VZ       29.7  33.1  29.6  32.6 357902700     22.1
#> 10 2010-10-01 VZ       32.8  33.7  31.8  32.5 423067300     22.0
#> # ... with 3,260 more rows
```

With additional functions provided in the package it's possible to fit a model in a rolling or expanding time window. This is done by `get_models()`.

``` r
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
#> # A time tibble: 176 x 16
#> # Index: index
#>    index      tickers term  estimate model.desc sigma logLik   AIC   BIC
#>    <date>     <fct>   <fct>    <dbl> <fct>      <dbl>  <dbl> <dbl> <dbl>
#>  1 2018-06-01 TECO2.~ alpha 0.000100 ETS(A,N,N) 0.108  -7.01  20.0  27.8
#>  2 2018-06-01 TECO2.~ l     0.0247   ETS(A,N,N) 0.108  -7.01  20.0  27.8
#>  3 2018-07-01 TECO2.~ alpha 0.000100 ETS(A,N,N) 0.108  -7.45  20.9  28.7
#>  4 2018-07-01 TECO2.~ l     0.0238   ETS(A,N,N) 0.108  -7.45  20.9  28.7
#>  5 2018-08-01 TECO2.~ alpha 0.000100 ETS(A,N,N) 0.108  -7.64  21.3  29.2
#>  6 2018-08-01 TECO2.~ l     0.0243   ETS(A,N,N) 0.108  -7.64  21.3  29.2
#>  7 2018-09-01 TECO2.~ alpha 0.000100 ETS(A,N,N) 0.108  -8.12  22.2  30.1
#>  8 2018-09-01 TECO2.~ l     0.0253   ETS(A,N,N) 0.108  -8.12  22.2  30.1
#>  9 2018-10-01 TECO2.~ alpha 0.000100 ETS(A,N,N) 0.108  -8.79  23.6  31.5
#> 10 2018-10-01 TECO2.~ l     0.0264   ETS(A,N,N) 0.108  -8.79  23.6  31.5
#> # ... with 166 more rows, and 7 more variables: ME <dbl>, RMSE <dbl>,
#> #   MAE <dbl>, MPE <dbl>, MAPE <dbl>, MASE <dbl>, ACF1 <dbl>
```

In this case, the the [ETS](http://pkg.robjhyndman.com/forecast/reference/ets.html) function was used to fit the first 100 observations on a expanding window, `.cumulative = TRUE`. The output contains the fitted parameters, selection criterias, as some traditional in-sample error metrics.

To extract the true out-of-sample forecasts use `get_forecasts()`. This function automatically identifies the `.assess` argument in `get_models()` and uses it as a target for the forecast horizon.

``` r
library(forecast)

get_tickers(merval) %>%
  get_stocks(
    tickers     = ., 
    periodicity = "monthly",
    quiet       = TRUE
    ) %>% 
  get_returns(
    .tbl            = ., 
    .group          = tickers, 
    .type           = log, 
    .omit_na        = TRUE, 
    return_adjusted = adjusted
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
#> # A time tibble: 88 x 8
#> # Index: index
#>    index      tickers return_adjusted mean_forecast  lo.80  lo.95 hi.80
#>    <date>     <fct>             <dbl>         <dbl>  <dbl>  <dbl> <dbl>
#>  1 2018-06-01 TECO2.~         -0.0669       0.0247  -0.114 -0.188 0.164
#>  2 2018-07-01 TECO2.~          0.0765       0.0238  -0.115 -0.188 0.162
#>  3 2018-08-01 TECO2.~          0.121        0.0243  -0.114 -0.187 0.162
#>  4 2018-09-01 TECO2.~          0.142        0.0253  -0.113 -0.186 0.163
#>  5 2018-10-01 TECO2.~         -0.102        0.0264  -0.112 -0.185 0.164
#>  6 2018-11-01 TECO2.~          0            0.0252  -0.113 -0.186 0.164
#>  7 2018-12-01 TECO2.~         -0.0461       0.0249  -0.113 -0.186 0.163
#>  8 2018-12-14 TECO2.~          0            0.0243  -0.113 -0.186 0.162
#>  9 2018-06-01 APBR.BA         -0.0385       0.00661 -0.196 -0.303 0.209
#> 10 2018-07-01 APBR.BA          0.103        0.00615 -0.195 -0.302 0.207
#> # ... with 78 more rows, and 1 more variable: hi.95 <dbl>
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
