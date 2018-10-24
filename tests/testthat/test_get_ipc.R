library(YahooTickers)

context("testing get_ipc()")

ipc_tickers <- YahooTickers:::get_ipc()

test_that("returns a tibble with one column and rows of lenght >= 30", {

  # message
  expect_message(YahooTickers:::get_ipc())

  # class
  expect_is(ipc_tickers, "tbl")
  expect_is(purrr::map_chr(ipc_tickers, class), "character")

  # names
  expect_equal(names(ipc_tickers), "tickers")

  # size
  expect_equal(ipc_tickers %>% ncol(), 1L)
  expect_gte(ipc_tickers %>% nrow(), 30)

})
