#' Calculate Returns From Stock Prices
#'
#' Stock prices are known for been non-stationary. For econometric modeling use \code{get_returns()}.
#'
#' @param .tbl A tidy \code{tibble}.
#' @param .group The column in which the data should be grouped. This will often be a column containing stock tickers or stocks names.
#' @param .type The method used to calculate returns. One of: \code{log} or \code{arithmetic}.
#' @param ... The column (columns) in which the calculation shoud be conducted.
#'
#' @return A tidy \code{tibble}.
#'
#' @export
#'
#' @examples
#' library(YahooTickers)
#' library(dplyr)
#' get_tickers(dow) %>%
#'   get_stocks(.) %>%
#'   get_returns(., tickers, arithmetic, adjusted)
get_returns <- function(.tbl, .group, .type, ...) {


  # return function
  if (lazyeval::expr_text(.type) == "log") {

    .return <- function(.col) log(.col / dplyr::lag(.col))

  } else if (lazyeval::expr_text(.type) == "arithmetic") {

    .return <- function(.col) (.col / dplyr::lag(.col)) - 1

  } else {

    rlang::abort(".type not supported. Try 'log' or 'arithmetic', insted.")

  }


  # tidy eval
  .index_var   <- tibbletime::get_index_quo(.tbl)
  .group_var   <- dplyr::enquo(.group)
  .mutate_vars <- dplyr::enquos(..., .named = TRUE)
  .mutate_vars <- purrr::map(
    .x = .mutate_vars,
    .f = function(var) dplyr::expr(.return(!!var))
    )
  names(.mutate_vars) <- stringr::str_c("return_", names(.mutate_vars))


  # data wrangling
  .tbl %>%
    dplyr::group_by(!!.group_var) %>%
    dplyr::mutate(!!.index_var, !!!.mutate_vars) %>%
    dplyr::ungroup() %>%
    dplyr::select(!!.index_var, !!.group_var, dplyr::starts_with("return"))

}






