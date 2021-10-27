# library(YahooTickers)
#
# context("testing get_stocks()")
#
# dow_tickers <- get_tickers(dow)
# dow_stocks  <- suppressWarnings(
#   get_stocks(dplyr::slice(dow_tickers, 1:2), from = "2018-01-01", to = "2018-01-03", periodicity = "daily", quiet = TRUE)
# )
#
# test_that("It returns a tibble with the correct dimensions", {
#
#   # class
#   expect_is(dow_stocks, "tbl")
#
#   # rows
#   expect_equal(nrow(dow_stocks), 2L)
#
#   # cols
#   expect_equal(ncol(dow_stocks), 8L)
#
#   # names
#   expect_equal(names(dow_stocks),
#                c("date", "tickers", "open", "high", "low", "close", "volume", "adjusted")
#                )
#
#
# })
#
