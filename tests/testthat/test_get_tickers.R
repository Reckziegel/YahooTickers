library(YahooTickers)

context("Testing get_tickers()")

test_that("Strings return an error", {
  expect_error(get_tickers("dow"))
})

test_that("Case insensitive", {
  expect_identical(get_tickers(bel20), get_tickers(BEL20))
})

test_that("Contatins one variable called 'tickers'", {

  expect_output(get_tickers(dow) %>% str(), "1 variable")
  expect_output(get_tickers(dow) %>% str(), "$ tickers", fixed = TRUE)

})

test_that("Returns an error if an strange index is called", {
  expect_error(get_tickers(aaa))

})
