library(YahooTickers)

context("testing get_cac40()")

cac40_tickers <- YahooTickers:::get_cac40()

test_that("returns a tibble with one column and rows of lenght == 40", {

  # message
  #expect_message(YahooTickers:::get_cac40())

  # class
  expect_is(cac40_tickers, "tbl")
  expect_is(purrr::map_chr(cac40_tickers, class), "character")

  # names
  expect_equal(names(cac40_tickers), "tickers")

  # size
  expect_equal(cac40_tickers %>% ncol(), 1L)
  expect_equal(cac40_tickers %>% nrow(), 40L)

  # tickers length
  expect_lte(purrr::map(cac40_tickers, stringr::str_length)[[1]] %>% max(), 8)

})

