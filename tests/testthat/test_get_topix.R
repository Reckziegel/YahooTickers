# library(YahooTickers)
#
# context("testing get_topix()")
#
# # topix_tickers <- YahooTickers:::get_topix()
#
# test_that("returns a tibble with one column and rows of lenght >= 30", {
#
#   skip("It takes 20s to run")
#   skip_on_travis("It takes 30s to run")
#   skip_on_appveyor("It takes 30s to run")
#
#   # message
#   #expect_message(YahooTickers:::get_topix())
#
#   # class
#   expect_is(topix_tickers, "tbl")
#   expect_is(purrr::map_chr(topix_tickers, class), "character")
#
#   # names
#   expect_equal(names(topix_tickers), "tickers")
#
#   # size
#   expect_equal(topix_tickers %>% ncol(), 1L)
#   expect_gte(topix_tickers %>% nrow(), 30)
#
#   # tickers length
#   expect_lte(purrr::map(topix_tickers, stringr::str_length)[[1]] %>% max(), 7)
#
# })
