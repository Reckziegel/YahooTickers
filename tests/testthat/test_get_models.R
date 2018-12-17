library(YahooTickers)
library(forecast)
library(dplyr)
library(tidyr)

context("Testing get_models()")

set.seed(1)

stocks_mod <- tibble(
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
)

test_that("The output contains the correct dimmensions", {

  # class
  expect_is(stocks_mod, "tbl")

  # rows
  expect_equal(nrow(stocks_mod), 30)

  # cols
  expect_equal(ncol(stocks_mod), 16)

  # names
  expect_equal(names(stocks_mod),
               c("time", "stock", "term", "estimate", "model.desc", "sigma",
                 "logLik", "AIC", "BIC", "ME", "RMSE", "MAE", "MPE", "MAPE", "MASE", "ACF1")
  )



})
