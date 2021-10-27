# library(YahooTickers)
#
# context("testing get_ipsa()")
#
# ipsa_tickers <- YahooTickers:::get_ipsa()
#
# test_that("returns a tibble with one column and rows of lenght >= 29", {
#
#   # class
#   expect_is(ipsa_tickers, "tbl")
#   expect_is(purrr::map_chr(ipsa_tickers, class), "character")
#
#   # names
#   expect_equal(names(ipsa_tickers), "tickers")
#
#   # size
#   expect_equal(ipsa_tickers %>% ncol(), 1L)
#   expect_gte(ipsa_tickers %>% nrow(), 29)
#
#   # tickers length
#   expect_lte(purrr::map(ipsa_tickers, stringr::str_length)[[1]] %>% max(), 14)
#
#
# })
