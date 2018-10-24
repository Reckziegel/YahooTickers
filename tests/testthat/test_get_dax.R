library(YahooTickers)

context("testing get_dax()")

dax_tickers <- YahooTickers:::get_dax()

test_that("returns a tibble with one column and rows of lenght >= 30", {

  # class
  expect_is(dax_tickers, "tbl")
  expect_is(purrr::map_chr(dax_tickers, class), "character")

  # names
  expect_equal(names(dax_tickers), "tickers")

  # size
  expect_equal(dax_tickers %>% ncol(), 1L)
  expect_gte(dax_tickers %>% nrow(), 30)

})
