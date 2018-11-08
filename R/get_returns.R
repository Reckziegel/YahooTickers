#' Calculate Returns From Stock Prices
#'
#' Stock prices are known for been non-stationary. Use \code{get_returns()} as a preprocessing step for econometric modeling.
#'
#' @param .tbl A tidy \code{tibble}.
#' @param .group The column in which the data should be grouped. This will often be a column with stock tickers or stocks names.
#' @param .type The method used to calculate returns. One of:
#'
#'   \itemize{
#'     \item \code{log}
#'     \item \code{arithmetic}
#'  }
#'
#' @param .omit_na Should NA values be omitted? Default is TRUE.
#' @param ... The column (or columns) in which the calculation shoud be conducted.
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
#'   get_returns(., .group = tickers, .type = arithmetic,
#'               .omit_na = TRUE, adjusted)
get_returns <- function(.tbl, .group, .type, .omit_na = TRUE, ...) {

  UseMethod("get_returns", .tbl)

}



# Method Default ----------------------------------------------------------

#' @rdname get_returns
#' @export
get_returns.default <- function(.tbl, .group, .type, .omit_na = TRUE, ...) {

  rlang::abort(".tbl must be a tibble or a tbl_time object.")

}



# Medthod tbl -------------------------------------------------------------

#' @rdname get_returns
#' @export
get_returns.tbl_df <- function(.tbl, .group, .type, .omit_na = TRUE, ...) {

  # coerce to tbl_time
  if (!("tbl_time" %in% class(.tbl))) {

    # find index name
    .index_name <- .tbl %>%
      dplyr::select_if(., .predicate = lubridate::is.Date) %>%
      names() %>%
      dplyr::sym(.)

    # coerce
    .tbl <- .tbl %>%
      tibbletime::as_tbl_time(., index = !!.index_name)

  }


  # return function
  if (lazyeval::expr_text(.type) == "log") {

    .return <- function(.col) log(.col / dplyr::lag(.col))

  } else if (lazyeval::expr_text(.type) == "arithmetic") {

    .return <- function(.col) (.col / dplyr::lag(.col)) - 1

  } else {

    rlang::abort(".type not supported. Try 'log' or 'arithmetic', instead.")

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
  .tbl <- .tbl %>%
    dplyr::group_by(!!.group_var) %>%
    dplyr::mutate(!!.index_var, !!!.mutate_vars) %>%
    dplyr::ungroup() %>%
    dplyr::select(!!.index_var, !!.group_var, dplyr::starts_with("return"))

  # should NA's be deleted?
  if (.omit_na) .tbl <- stats::na.omit(.tbl)

  # output
  return(.tbl)

}



# Method tbl_time ---------------------------------------------------------

#' @rdname get_returns
#' @export
get_returns.tbl_time <- function(.tbl, .group, .type, .omit_na = TRUE, ...) {


  # return function
  if (lazyeval::expr_text(.type) == "log") {

    .return <- function(.col) log(.col / dplyr::lag(.col))

  } else if (lazyeval::expr_text(.type) == "arithmetic") {

    .return <- function(.col) (.col / dplyr::lag(.col)) - 1

  } else {

    rlang::abort(".type not supported. Try 'log' or 'arithmetic', instead.")

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
  .tbl <- .tbl %>%
    dplyr::group_by(!!.group_var) %>%
    dplyr::mutate(!!.index_var, !!!.mutate_vars) %>%
    dplyr::ungroup() %>%
    dplyr::select(!!.index_var, !!.group_var, dplyr::starts_with("return"))


  # should NA's be deleted?
  if (.omit_na) .tbl <- na.omit(.tbl)

  # output
  return(.tbl)

}






