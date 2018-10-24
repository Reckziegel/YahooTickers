library(YahooTickers)

context("testing get_topix()")

topix_tickers <- YahooTickers:::get_topix()

test_that("returns a tibble with one column and rows of lenght >= 30", {

  # message
  expect_message(YahooTickers:::get_topix())

  # class
  expect_is(topix_tickers, "tbl")
  expect_is(purrr::map_chr(topix_tickers, class), "character")

  # names
  expect_equal(names(topix_tickers), "tickers")

  # size
  expect_equal(topix_tickers %>% ncol(), 1L)
  expect_gte(topix_tickers %>% nrow(), 30)

})
