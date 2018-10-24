library(YahooTickers)

context("testing get_dow()")

dow_tickers <- YahooTickers:::get_dow()

test_that("returns a tibble with one column and rows of lenght == 30", {

  # class
  expect_is(dow_tickers, "tbl")
  expect_is(purrr::map_chr(dow_tickers, class), "character")

  # names
  expect_equal(names(dow_tickers), "tickers")

  # size
  expect_equal(dow_tickers %>% ncol(), 1L)
  expect_gte(dow_tickers %>% nrow(), 30)

})
