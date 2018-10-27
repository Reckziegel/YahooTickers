#' Calculate Returns From Stock Prices
#'
#' Stock prices are known for not been stationary. For econometric modeling use \code{get_returns()}.
#'
#' @param .tbl A tidy \code{tibble}.
#' @param ... The columns in which the calculation shoud be conducted.
#'
#' @return A tidy \code{tibble}.
#'
#' @export
#'
#' @examples
#' library(YahooTickers)
#' library(dplyr)
#' get_tickers("dow") %>%
#'   get_stocks(.) %>%
#'   get_returns(., adjusted)
get_returns <- function(.tbl, ...) {

  # return function
    .return <- function(.col) (.col / dplyr::lag(.col)) - 1

  # tidy eval
  mutate_vars <- dplyr::enquos(..., .named = TRUE)
  mutate_vars <- purrr::map(
    .x = mutate_vars,
    .f = function(var) dplyr::expr(.return(!! var))
    )
  names(mutate_vars) <- stringr::str_c("return_", names(mutate_vars))

  # data wrangling
  .tbl %>%
    dplyr::group_by(tickers) %>%
    dplyr::mutate(index, !!! mutate_vars) %>%
    dplyr::ungroup() %>%
    dplyr::select(index, tickers, dplyr::starts_with("return"))

}






