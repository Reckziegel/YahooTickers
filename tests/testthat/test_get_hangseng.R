library(YahooTickers)

context("testing get_hangseng()")

hangseng_tickers <- YahooTickers:::get_hangseng()

test_that("returns a tibble with one column and rows of lenght >= 50", {

  # message
  expect_message(YahooTickers:::get_hangseng())

  # class
  expect_is(hangseng_tickers, "tbl")
  expect_is(purrr::map_chr(hangseng_tickers, class), "character")

  # names
  expect_equal(names(hangseng_tickers), "tickers")

  # size
  expect_equal(hangseng_tickers %>% ncol(), 1L)
  expect_gte(hangseng_tickers %>% nrow(), 50)

})
