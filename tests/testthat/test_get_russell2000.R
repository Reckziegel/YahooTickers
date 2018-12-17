library(YahooTickers)

context("testing get_russell2000()")

#russell2000_tickers <- YahooTickers:::get_russell2000()

test_that("returns a tibble with one column and rows of lenght >= 2000", {

  skip("It takes 5min to run")
  skip_on_travis("It takes 30s to run")
  skip_on_appveyor("It takes 30s to run")

  # message
  #expect_message(YahooTickers:::get_russell2000())

  # class
  expect_is(russell2000_tickers, "tbl")
  expect_is(purrr::map_chr(russell2000_tickers, class), "character")

  # names
  expect_equal(names(russell2000_tickers), "tickers")

  # size
  expect_equal(russell2000_tickers %>% ncol(), 1L)
  expect_gte(russell2000_tickers %>% nrow(), 1266)

  # tickers length
  expect_lte(purrr::map(russell2000_tickers, stringr::str_length)[[1]] %>% max(), 5)

})
