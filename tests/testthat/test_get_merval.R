library(YahooTickers)

context("testing get_merval()")

merval_tickers <- YahooTickers:::get_merval()

test_that("returns a tibble with one column and rows of lenght >= 10", {

  # class
  expect_is(merval_tickers, "tbl")
  expect_is(purrr::map_chr(merval_tickers, class), "character")

  # names
  expect_equal(names(merval_tickers), "tickers")

  # size
  expect_equal(merval_tickers %>% ncol(), 1L)
  expect_gte(merval_tickers %>% nrow(), 10)

})
