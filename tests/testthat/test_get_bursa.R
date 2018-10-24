library(YahooTickers)

context("testing get_bursa()")

bursa_tickers <- YahooTickers:::get_bursa()

test_that("returns a tibble with one column and rows of lenght >= 30", {

  # message
  expect_message(YahooTickers:::get_bursa())

  # class
  expect_is(bursa_tickers, "tbl")
  expect_is(purrr::map_chr(bursa_tickers, class), "character")

  # names
  expect_equal(names(bursa_tickers), "tickers")

  # size
  expect_equal(bursa_tickers %>% ncol(), 1L)
  expect_gte(bursa_tickers %>% nrow(), 30)

})
