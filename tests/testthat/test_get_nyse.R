# library(YahooTickers)
#
# context("testing get_nyse()")
#
# #nyse_tickers <- YahooTickers:::get_nyse()
#
# test_that("returns a tibble with one column and rows of lenght >= 1000", {
#
#   skip("It takes 5min to run")
#   skip_on_travis("It takes 30s to run")
#   skip_on_appveyor("It takes 30s to run")
#
#   # message
#   #expect_message(YahooTickers:::get_nasdaq())
#
#   # class
#   expect_is(nyse_tickers, "tbl")
#   expect_is(purrr::map_chr(nyse_tickers, class), "character")
#
#   # names
#   expect_equal(names(nyse_tickers), "tickers")
#
#   # size
#   expect_equal(nyse_tickers %>% ncol(), 1L)
#   expect_gte(nyse_tickers %>% nrow(), 1000)
#
#   # tickers length
#   expect_lte(purrr::map(nyse_tickers, stringr::str_length)[[1]] %>% max(), 6)
#
# })
