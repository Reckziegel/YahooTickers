library(YahooTickers)
library(dplyr)
library(tidyr)

context("Testing get_returns()")

stocks <- tibble(
  time = as.Date('2009-01-01') + 0:4,
  X    = rnorm(5, 0, 1),
  Y    = rnorm(5, 0, 2),
  Z    = rnorm(5, 0, 4)
  ) %>%
  gather(stock, price, -time)

stocks %>% get_returns(.group = stock, .type = arithmetic, .omit_na = TRUE, ret = price)

test_that("A tibble is returned with the correct dimmensions", {

  # .omit_na = FALSE do not delete any row
  expect_equal(
    get_returns(.tbl = stocks, .group = stock, .type = arithmetic, .omit_na = FALSE, price) %>%
      nrow(),
    15
    )

  # .omit_na = TRUE does
  expect_equal(
    get_returns(.tbl = stocks, .group = stock, .type = arithmetic, .omit_na = TRUE, price) %>%
      nrow(),
    12
  )

  # col names can be given on the fly
  expect_equal(
    get_returns(.tbl = stocks, .group = stock, .type = arithmetic, .omit_na = TRUE, price_ret = price) %>%
      names() %>%
      last(),
    "price_ret"
    )


})
