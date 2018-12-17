library(YahooTickers)
library(forecast)
library(dplyr)
library(tidyr)

context("Testing get_forecasts()")

set.seed(1)

stock_fc <- tibble(
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
             .fun        = Arima,
             order       = c(0, 0, 0),
             seasonal    = c(0, 0, 0)
             ) %>%
  get_forecasts()

test_that("The output contains the correct dimmensions", {

  # class
  expect_is(stock_fc, "tbl")

  # rows
  expect_equal(nrow(stock_fc), 30)

  # cols
  expect_equal(ncol(stock_fc), 8)

  # names
  expect_equal(names(stock_fc),
               c("time", "stock", "price", "mean_forecast", "lo.80", "lo.95", "hi.80", "hi.95")
  )


})
