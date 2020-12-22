library(YahooTickers)

context("testing get_sensex()")

sensex_tickers <- YahooTickers:::get_sensex()

test_that("returns a tibble with one column and rows of lenght >= 30", {

  # message
  #expect_message(YahooTickers:::get_sensex())

  # class
  expect_is(sensex_tickers, "tbl")
  expect_is(purrr::map_chr(sensex_tickers, class), "character")

  # names
  expect_equal(names(sensex_tickers), "tickers")

  # size
  expect_equal(sensex_tickers %>% ncol(), 1L)
  expect_gte(sensex_tickers %>% nrow(), 29L)

  # tickers length
  expect_lte(purrr::map(sensex_tickers, stringr::str_length)[[1]] %>% max(), 10)

})
