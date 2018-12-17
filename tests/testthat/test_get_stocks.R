library(YahooTickers)

context("testing get_stocks()")

merval_tickers <- get_tickers(merval)
merval_stocks  <- get_stocks(merval_tickers, from = "2018-01-01", to = "2018-01-03", periodicity = "monthly")

test_that("It returns a tibble with the correct dimensions", {

  # class
  expect_is(merval_stocks, "tbl")

  # rows
  expect_equal(nrow(merval_stocks), 11)

  # cols
  expect_equal(ncol(merval_stocks), 8)

  # names
  expect_equal(names(merval_stocks),
               c("index", "tickers", "open", "high", "low", "close", "volume", "adjusted")
               )


})

