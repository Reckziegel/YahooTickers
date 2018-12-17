library(YahooTickers)

context("testing get_amex()")

#amex_tickers <- YahooTickers:::get_amex()

test_that("returns a tibble with one column and rows of lenght >= 100", {

  skip("It takes 30s to run")
  skip_on_travis("It takes 30s to run")
  skip_on_appveyor("It takes 30s to run")

  # message
  #expect_message(YahooTickers:::get_amex())

  # class
  expect_is(amex_tickers, "tbl")
  expect_is(purrr::map_chr(amex_tickers, class), "character")

  # names
  expect_equal(names(amex_tickers), "tickers")

  # size
  expect_equal(amex_tickers %>% ncol(), 1L)
  expect_gte(amex_tickers %>% nrow(), 200)

  # tickers length
  expect_lte(purrr::map(amex_tickers, stringr::str_length)[[1]] %>% max(), 5)

})
