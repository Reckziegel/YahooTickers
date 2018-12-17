library(YahooTickers)

context("testing get_taiex()")

taiex_tickers <- YahooTickers:::get_taiex()

test_that("returns a tibble with one column and rows of lenght >= 800", {

  # message
  #expect_message(YahooTickers:::get_taiex())

  # class
  expect_is(taiex_tickers, "tbl")
  expect_is(purrr::map_chr(taiex_tickers, class), "character")

  # names
  expect_equal(names(taiex_tickers), "tickers")

  # size
  expect_equal(taiex_tickers %>% ncol(), 1L)
  expect_gte(taiex_tickers %>% nrow(), 800)

  # tickers length
  expect_lte(purrr::map(taiex_tickers, stringr::str_length)[[1]] %>% max(), 8)

})
