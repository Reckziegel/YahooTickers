library(YahooTickers)

context("testing get_ibov())")

ibov_tickers <- YahooTickers:::get_ibov()

test_that("returns a tibble with one column and rows of lenght >= 50", {

  # message
  expect_message(YahooTickers:::get_ibov())

  # class
  expect_is(ibov_tickers, "tbl")
  expect_is(purrr::map_chr(ibov_tickers, class), "character")

  # names
  expect_equal(names(ibov_tickers), "tickers")

  # size
  expect_equal(ibov_tickers %>% ncol(), 1L)
  expect_gte(ibov_tickers %>% nrow(), 50)

})
