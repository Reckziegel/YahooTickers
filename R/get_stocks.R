#' A wrapper over \code{getSymbols} that maps over index components.
#'
#' \code{get_stocks} is a time saver that allows you to quickly download stock indexes from the the \href{https://finance.yahoo.com/}{Yahoo Finance} without having to run \code{for} loops or worrying about coertion rules between different object classes. The output is always a tidy \code{tibble}.
#'
#' @param tickers A character vector of with the yahoo symbols.
#' @param from A date in the format YYYY-MM-DD.
#' @param to A date in the format YYYY-MM-DD.
#' @param periodicity One of: \code{daily}, \code{weely} or \code{monthly}.
#' @param simplify If TRUE, a \code{tibble} is returned; if FALSE a list with tow elements is returned: \code{result} and \code{error}. The defaul is TRUE.
#' @param otherwise Argument passed to \code{purrr::safely}.
#' @param quiet Argument passed to \code{purrr::safely}. A notification is showed whenever an error occurs.
#'
#' @return A tidy \code{tibble} if \code{simplify = TRUE} and a \code{list} of two components if \code{simplify = FALSE}.
#'
#' @importFrom magrittr "%>%"
#'
#' @export
#'
#' @examples
#' # Get stocks from the Merval Index (Argentina)
#' library(YahooTickers)
#' ticks <- get_tickers("merval")
#' get_stocks(ticks, periodicity = "monthly")
get_stocks <- function(tickers,
                       from        = "2010-01-01",
                       to          = Sys.Date(),
                       periodicity = "daily",
                       simplify    = TRUE,
                       otherwise   = NULL,
                       quiet       = FALSE) {

  # check inputs
  if (!purrr::is_vector(tickers)) {
    rlang::abort("The tickers argument must be an atomic vector of dimension N x 1.")
  } else if (!purrr::is_logical(quiet)) {
    rlang::abort("The quiet argument must be TRUE or FALSE.")
  } else if (!purrr::is_logical(simplify)) {
    rlang::abort("The quiet argument must be TRUE or FALSE.")
  }

  periodicity_arg <- rlang::arg_match(
    arg    = periodicity,
    values = c("daily", "weely", "monthly")
  )

  # error handler
  safe_getSymbols = purrr::safely(
    .f        = quantmod::getSymbols,
    otherwise = otherwise,
    quiet     = quiet
  )

  # start downlaod
  stocks <- tickers %>%
    dplyr::mutate(

      # get the tickers
      download = purrr::map(
        .x          = tickers,
        .f          = ~ safe_getSymbols(
          Symbols     = .,
          from        = from,
          to          = to,
          periodicity = periodicity_arg,
          warnings    = TRUE,
          src         = "yahoo",
          auto.assign = getOption('getSymbols.auto.assign', FALSE)
        )
      ),

      # unnest result
      result = purrr::map(
        .x = download,
        .f = "result"
      ) %>%
        purrr::map(.f = timetk::tk_tbl),

      # unnest error
      error = purrr::map_chr(
        .x = download,
        .f = ~ dplyr::if_else(
          condition = purrr::is_null(.$error),
          true      = NA_character_,
          false     = .$error$message
        )
      )
    )

  stocks_simple <- stocks %>%
    dplyr::filter(is.na(error)) %>%
    dplyr::mutate(result = purrr::map(
      .x = result,
      .f = ~ dplyr::rename(
        .data    = .,
        open     = 2,
        high     = 3,
        low      = 4,
        close    = 5,
        volume   = 6,
        adjusted = 7
        )
      )
    ) %>%
    tidyr::unnest(result) %>%
    dplyr::select(index, tickers, open, high, low, close, volume, adjusted) %>%
    tibbletime::as_tbl_time(., index = index)

  if (simplify) {

    stocks_simple

  } else {

    error_simple <- stocks %>%
      dplyr::filter(!is.na(error)) %>%
      dplyr::select(error)

    if (nrow(error_simple) != 0) {

      error_simple <- error_simple

    } else {

      error_simple <- "No error found."

    }

    list(result = stocks_simple, error = error_simple)

  }

}

