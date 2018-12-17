library(YahooTickers)

context("testing get_sp500()")

sp500_tickers <- YahooTickers:::get_sp500()

test_that("returns a tibble with one column and rows of lenght >= 500", {

  # message
  #expect_message(YahooTickers:::get_sp500())

  # class
  expect_is(sp500_tickers, "tbl")
  expect_is(purrr::map_chr(sp500_tickers, class), "character")

  # names
  expect_equal(names(sp500_tickers), "tickers")

  # size
  expect_equal(sp500_tickers %>% ncol(), 1L)
  expect_gte(sp500_tickers %>% nrow(), 500)

  # tickers length
  expect_lte(purrr::map(sp500_tickers, stringr::str_length)[[1]] %>% max(), 5)

})
