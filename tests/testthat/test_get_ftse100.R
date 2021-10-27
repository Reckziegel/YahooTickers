# library(YahooTickers)
#
# context("testing get_ftse100()")
#
# ftse100_tickers <- YahooTickers:::get_ftse100()
#
# test_that("returns a tibble with one column and rows of lenght >= 100", {
#
#   # message
#   #expect_message(YahooTickers:::get_ftse100())
#
#   # class
#   expect_is(ftse100_tickers, "tbl")
#   expect_is(purrr::map_chr(ftse100_tickers, class), "character")
#
#   # names
#   expect_equal(names(ftse100_tickers), "tickers")
#
#   # size
#   expect_equal(ftse100_tickers %>% ncol(), 1L)
#   expect_gte(ftse100_tickers %>% nrow(), 100L)
#
#   # tickers length
#   expect_lte(purrr::map(ftse100_tickers, stringr::str_length)[[1]] %>% max(), 7)
#
# })
