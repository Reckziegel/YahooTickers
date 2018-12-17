library(YahooTickers)
library(forecast)
library(dplyr)
library(tidyr)

context("Testing get_forecasts()")

stocks <- tibble(
  time = as.Date('2009-01-01') + 0:49,
  X    = rnorm(50, 0, 1),
  Y    = rnorm(50, 0, 2),
  Z    = rnorm(50, 0, 4)
) %>%
  gather(stock, price, -time) %>%
  get_models(.group      = stock,
             .col        = price,
             .initial    = 40,
             .assess     = 1,
             .cumulative = TRUE,
             .fun        = auto.arima,
             seasonal    = FALSE,
             stationary  = TRUE,
             max.p       = 1,
             max.q       = 1
             ) %>%
  get_forecasts()

test_that("The output contains the correct dimmensions", {

  # class
  expect_is(stocks, "tbl")

  # rows
  expect_equal(nrow(stocks), 30)

  # cols
  expect_equal(ncol(stocks), 8)

  # names
  expect_equal(names(stocks),
               c("time", "stock", "price", "mean_forecast", "lo.80", "lo.95", "hi.80", "hi.95")
  )


})
