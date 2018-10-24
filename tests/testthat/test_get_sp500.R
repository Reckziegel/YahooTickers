library(YahooTickers)

context("testing get_sp500()")

sp_tickers <- YahooTickers:::get_sp500()

test_that("returns a tibble with one column and rows of lenght >= 500", {

  # message
  expect_message(YahooTickers:::get_sp500())

  # class
  expect_is(sp_tickers, "tbl")
  expect_is(purrr::map_chr(sp_tickers, class), "character")

  # names
  expect_equal(names(sp_tickers), "tickers")

  # size
  expect_equal(sp_tickers %>% ncol(), 1L)
  expect_gte(sp_tickers %>% nrow(), 500)

})
