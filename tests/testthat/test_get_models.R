# library(YahooTickers)
# library(forecast)
# library(dplyr)
# library(tidyr)
#
# context("Testing get_models()")
#
# set.seed(1)
#
# stocks_mod <- tibble(
#   time = as.Date('2009-01-01') + 0:49,
#   X    = rnorm(50, 0, 1),
#   Y    = rnorm(50, 0, 2),
#   Z    = rnorm(50, 0, 4)
# ) %>%
#   pivot_longer(cols = c(X, Y, Z), names_to = 'stocks', values_to = 'returns') %>%
#   get_models(.tbl        = .,
#              .group      = stocks,
#              .col        = returns,
#              .initial    = 40,
#              .assess     = 1,
#              .cumulative = TRUE,
#              .fun        = Arima,
#              order       = c(0, 0, 0),
#              seasonal    = c(0, 0, 0)
# )
#
# test_that("The output contains the correct dimmensions", {
#
#   # class
#   expect_is(stocks_mod, "tbl")
#   expect_is(stocks_mod, "YahooTickers")
#
#   # rows
#   expect_equal(nrow(stocks_mod), 30)
#
#   # cols
#   expect_equal(ncol(stocks_mod), 17)
#
#   # names
#   expect_equal(names(stocks_mod),
#                c("time", "stocks", "data", "term", "estimate", "model.desc", "sigma",
#                  "logLik", "AIC", "BIC", "ME", "RMSE", "MAE", "MPE", "MAPE", "MASE", "ACF1")
#   )
#
#
#
# })
