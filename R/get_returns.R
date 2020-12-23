#' Calculate Returns From Stock Prices
#'
#' Stock prices are known for been non-stationary. Use \code{get_returns()} as a
#' pre-processing step for econometric modeling.
#'
#' @param .tbl A tidy \code{tibble}.
#' @param .group The column in which the data should be grouped. This will often
#' be a column with stock tickers or stocks names.
#' @param .type The method used to calculate returns. One of:
#'
#'   \itemize{
#'     \item \code{log}
#'     \item \code{arithmetic}
#'  }
#'
#' @param .omit_na Should NA values be omitted? Default is \code{FALSE} (NA's are replaced by \code{0}).
#' @param ... The column (or columns) in which the calculation should be conducted.
#'
#' @return A tidy \code{tibble}.
#'
#' @export
#'
#' @examples
#' library(YahooTickers)
#' library(dplyr)
#' get_tickers(dow) %>%
#'   slice(1:3) %>%
#'   get_stocks(.) %>%
#'   get_returns(., .group = tickers, .type = arithmetic,
#'               .omit_na = TRUE, adjusted)
get_returns <- function(.tbl, .group, .type, .omit_na = FALSE, ...) {

  UseMethod("get_returns", .tbl)

}


# Method Default ----------------------------------------------------------

#' @rdname get_returns
#' @export
get_returns.default <- function(.tbl, .group, .type, .omit_na = FALSE, ...) {

  rlang::abort(".tbl must be a tibble object.")

}


# Medthod tbl -------------------------------------------------------------

#' @rdname get_returns
#' @export
get_returns.tbl_df <- function(.tbl, .group, .type, .omit_na = FALSE, ...) {

  # return function
  if (lazyeval::expr_text(.type) == "log") {
    .return <- function(.col) log(.col / dplyr::lag(.col))
  } else if (lazyeval::expr_text(.type) == "arithmetic") {
    .return <- function(.col) (.col / dplyr::lag(.col)) - 1
  } else {
    rlang::abort(".type not supported. Try 'log' or 'arithmetic', instead.")
  }


  # tidy eval
  #.index_var   <- get_index_col(.tbl)
  .index_name  <- get_index_char(.tbl)
  .group_var   <- dplyr::enquo(.group)
  .mutate_vars <- dplyr::enquos(..., .named = TRUE)
  .mutate_vars <- purrr::map(
    .x = .mutate_vars,
    .f = function(var) dplyr::expr(.return(!!var))
  )
  #.mutate_vars_names <- names(.mutate_vars)


  # data wrangling
  .tbl <- .tbl %>%

    dplyr::group_by(!!.group_var) %>%
    dplyr::mutate(!!! .mutate_vars) %>%
    dplyr::ungroup() %>%

    dplyr::select(!!.index_name, !!.group_var, names(.mutate_vars))

  # should NA's be deleted?
  if (.omit_na) {

    .tbl <- stats::na.omit(.tbl)

  } else {

    .tbl <- .tbl %>%
      dplyr::group_by(!! .group_var) %>%
      purrr::modify_if(
        .p = purrr::map_lgl(.x = ., .f = ~ NA %in% .x),
        .f = ~ .x %>%
          tidyr::replace_na(list(0)) %>%
          unlist()
      ) %>%
      dplyr::ungroup()

  }

  # output
  tibble::new_tibble(x = .tbl, nrow = nrow(.tbl), class = 'YahooTickers')

}
