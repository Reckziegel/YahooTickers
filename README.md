
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
dow <- get_tickers(dow) %>% 
  get_stocks(., periodicity = "monthly")

dow 
#> # A time tibble: 3,300 x 8
#> # Index: index
#>    index      tickers  open  high   low close    volume adjusted
#>    <date>     <fct>   <dbl> <dbl> <dbl> <dbl>     <dbl>    <dbl>
#>  1 2010-01-01 HD       29.2  29.2  27.2  28.0 305927400     22.7
#>  2 2010-02-01 HD       28.0  31.5  27.5  31.2 358199600     25.3
#>  3 2010-03-01 HD       31.2  33.0  31.1  32.3 311755400     26.2
#>  4 2010-04-01 HD       32.4  37.0  32.2  35.2 371036400     28.8
#>  5 2010-05-01 HD       35.4  36.3  32.1  33.9 522063700     27.7
#>  6 2010-06-01 HD       33.4  34.2  28.0  28.1 445088000     22.9
#>  7 2010-07-01 HD       28.4  29.4  26.6  28.5 375351500     23.4
#>  8 2010-08-01 HD       28.8  29.2  27.1  27.8 279092800     22.9
#>  9 2010-09-01 HD       28.1  32.2  28.1  31.7 235029100     26.3
#> 10 2010-10-01 HD       31.8  32.1  30.2  30.9 248397600     25.6
#> # ... with 3,290 more rows
```

With additional functions provided in the package it's possible to fit a model in a rolling or expanding time window. This is done by `get_models()`.

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
#> # A time tibble: 198 x 16
#> # Index: index
#>    index      tickers term  estimate model.desc sigma logLik   AIC   BIC
#>    <date>     <fct>   <fct>    <dbl> <fct>      <dbl>  <dbl> <dbl> <dbl>
#>  1 2018-06-01 TS.BA   alpha 0.000100 ETS(A,N,N) 0.109  -7.34  20.7  28.5
#>  2 2018-06-01 TS.BA   l     0.0170   ETS(A,N,N) 0.109  -7.34  20.7  28.5
#>  3 2018-07-01 TS.BA   alpha 0.000100 ETS(A,N,N) 0.109  -8.13  22.3  30.1
#>  4 2018-07-01 TS.BA   l     0.0183   ETS(A,N,N) 0.109  -8.13  22.3  30.1
#>  5 2018-08-01 TS.BA   alpha 0.000100 ETS(A,N,N) 0.109  -8.33  22.7  30.5
#>  6 2018-08-01 TS.BA   l     0.0178   ETS(A,N,N) 0.109  -8.33  22.7  30.5
#>  7 2018-09-01 TS.BA   alpha 0.000100 ETS(A,N,N) 0.110 -10.0   26.1  34.0
#>  8 2018-09-01 TS.BA   l     0.0197   ETS(A,N,N) 0.110 -10.0   26.1  34.0
#>  9 2018-10-01 TS.BA   alpha 0.000100 ETS(A,N,N) 0.109 -10.5   26.9  34.8
#> 10 2018-10-01 TS.BA   l     0.0205   ETS(A,N,N) 0.109 -10.5   26.9  34.8
#> # ... with 188 more rows, and 7 more variables: ME <dbl>, RMSE <dbl>,
#> #   MAE <dbl>, MPE <dbl>, MAPE <dbl>, MASE <dbl>, ACF1 <dbl>
```

In this case, the the [ETS](http://pkg.robjhyndman.com/forecast/reference/ets.html) function was used to fit the first 100 observations on a expanding window, `.cumulative = TRUE`. The output contains the fitted parameters, selection criterias, as some traditional in-sample error metrics.

To extract the true out-of-sample forecasts use `get_forecasts()`. This function automatically identifies the `.assess` argument in `get_models()` and uses it as a target for the forecast horizon.

``` r
library(forecast)

merval_models  %>% 
  get_forecasts(.)
#> # A time tibble: 99 x 8
#> # Index: index
#>    index      tickers ret_adj mean_forecast  lo.80  lo.95 hi.80 hi.95
#>    <date>     <fct>     <dbl>         <dbl>  <dbl>  <dbl> <dbl> <dbl>
#>  1 2018-06-01 TS.BA    0.147         0.0170 -0.122 -0.196 0.156 0.230
#>  2 2018-07-01 TS.BA   -0.0332        0.0183 -0.121 -0.195 0.158 0.232
#>  3 2018-08-01 TS.BA    0.213         0.0178 -0.121 -0.195 0.157 0.230
#>  4 2018-09-01 TS.BA    0.108         0.0197 -0.121 -0.195 0.160 0.235
#>  5 2018-10-01 TS.BA   -0.277         0.0205 -0.120 -0.194 0.161 0.235
#>  6 2018-11-01 TS.BA   -0.128         0.0177 -0.127 -0.203 0.162 0.239
#>  7 2018-12-01 TS.BA   -0.123         0.0173 -0.128 -0.204 0.162 0.239
#>  8 2019-01-01 TS.BA    0.126         0.0150 -0.130 -0.207 0.160 0.237
#>  9 2019-02-01 TS.BA    0.125         0.0160 -0.129 -0.206 0.161 0.238
#> 10 2018-06-01 COME.BA -0.195         0.0256 -0.150 -0.243 0.201 0.294
#> # ... with 89 more rows
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
