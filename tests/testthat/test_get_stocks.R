library(YahooTickers)
library(dplyr)

context("testing get_stocks()")


# data for the tests
belgium_stocks <- get_tickers(bel20) %>%
  slice(1:5) %>%
  get_stocks(.)

dow_stocks <- get_tickers(dow) %>%
  slice(1:2) %>%
  get_stocks(., periodicity = "monthly", simplify = FALSE)



test_that("Contains the correct column names", {

  expect_equal(
    colnames(belgium_stocks),
    c("index", "tickers", "open", "high", "low", "close", "volume", "adjusted")
    )

})



test_that("'Simplify' argument works as it should", {

  expect_output(str(dow_stocks), "List of 2")

})





