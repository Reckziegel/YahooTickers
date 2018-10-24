library(YahooTickers)

context("testing get_nasdaq()")

nasdaq_tickers <- YahooTickers:::get_nasdaq()

test_that("returns a tibble with one column and rows of lenght >= 1000", {

  # message
  expect_message(YahooTickers:::get_nasdaq())

  # class
  expect_is(nasdaq_tickers, "tbl")
  expect_is(purrr::map_chr(nasdaq_tickers, class), "character")

  # names
  expect_equal(names(nasdaq_tickers), "tickers")

  # size
  expect_equal(nasdaq_tickers %>% ncol(), 1L)
  expect_gte(nasdaq_tickers %>% nrow(), 1000)

})
