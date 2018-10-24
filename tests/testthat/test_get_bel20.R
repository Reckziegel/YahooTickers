library(YahooTickers)

context("testing get_bel20()")

bel20_tickers <- YahooTickers:::get_bel20()

test_that("returns a tibble with one column and rows of lenght == 20", {

  # class
  expect_is(bel20_tickers, "tbl")
  expect_is(purrr::map_chr(bel20_tickers, class), "character")

  # names
  expect_equal(names(bel20_tickers), "tickers")

  # size
  expect_equal(bel20_tickers %>% ncol(), 1L)
  expect_gte(bel20_tickers %>% nrow(), 20)

})
