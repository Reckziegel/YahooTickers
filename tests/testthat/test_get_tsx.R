# library(YahooTickers)
#
# context("testing get_tsx()")
#
# tsx_tickers <- YahooTickers:::get_tsx()
#
# test_that("returns a tibble with one column and rows of lenght >= 200", {
#
#   # message
#   #expect_message(YahooTickers:::get_tsx())
#
#   # class
#   expect_is(tsx_tickers, "tbl")
#   expect_is(purrr::map_chr(tsx_tickers, class), "character")
#
#   # names
#   expect_equal(names(tsx_tickers), "tickers")
#
#   # size
#   expect_equal(tsx_tickers %>% ncol(), 1L)
#   expect_gte(tsx_tickers %>% nrow(), 234L)
#
#   # tickers length
#   expect_lte(purrr::map(tsx_tickers, stringr::str_length)[[1]] %>% na.omit() %>% max(), 10)
#
# })
#
