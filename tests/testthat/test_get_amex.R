library(YahooTickers)

context("testing get_amex()")

amex_tickers <- YahooTickers:::get_amex()

test_that("returns a tibble with one column and rows of lenght >= 100", {

  # message
  expect_message(YahooTickers:::get_amex())

  # class
  expect_is(amex_tickers, "tbl")
  expect_is(purrr::map_chr(amex_tickers, class), "character")

  # names
  expect_equal(names(amex_tickers), "tickers")

  # size
  expect_equal(amex_tickers %>% ncol(), 1L)
  expect_gte(amex_tickers %>% nrow(), 100)

})
