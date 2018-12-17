library(YahooTickers)

context("testing get_jakarta()")

jakarta_tickers <- YahooTickers:::get_jakarta()

test_that("returns a tibble with one column and rows of lenght >= 400", {

  # message
  #expect_message(YahooTickers:::get_jakarta())

  # class
  expect_is(jakarta_tickers, "tbl")
  expect_is(purrr::map_chr(jakarta_tickers, class), "character")

  # names
  expect_equal(names(jakarta_tickers), "tickers")

  # size
  expect_equal(jakarta_tickers %>% ncol(), 1L)
  expect_gte(jakarta_tickers %>% nrow(), 400)

  # tickers length
  expect_lte(purrr::map(jakarta_tickers, stringr::str_length)[[1]] %>% max(), 9)


})
