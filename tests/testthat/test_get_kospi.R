# library(YahooTickers)
#
# context("testing get_kospi()")
#
# kospi_tickers <- YahooTickers:::get_kospi()
#
# test_that("returns a tibble with one column and rows of lenght >= 800", {
#
#   # message
#   #expect_message(YahooTickers:::get_kospi())
#
#   # class
#   expect_is(kospi_tickers, "tbl")
#   expect_is(purrr::map_chr(kospi_tickers, class), "character")
#
#   # names
#   expect_equal(names(kospi_tickers), "tickers")
#
#   # size
#   expect_equal(kospi_tickers %>% ncol(), 1L)
#   expect_gte(kospi_tickers %>% nrow(), 800)
#
#   # tickers length
#   expect_lte(purrr::map(kospi_tickers, stringr::str_length)[[1]] %>% max(), 10)
#
# })
