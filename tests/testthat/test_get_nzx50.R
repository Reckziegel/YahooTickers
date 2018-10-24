library(YahooTickers)

context("testing get_nzx50()")

nzx50_tickers <- YahooTickers:::get_nzx50()

test_that("returns a tibble with one column and rows of lenght >= 49", {

  # message
  expect_message(YahooTickers:::get_nzx50())

  # class
  expect_is(nzx50_tickers, "tbl")
  expect_is(purrr::map_chr(nzx50_tickers, class), "character")

  # names
  expect_equal(names(nzx50_tickers), "tickers")

  # size
  expect_equal(nzx50_tickers %>% ncol(), 1L)
  expect_gte(nzx50_tickers %>% nrow(), 49)

})
