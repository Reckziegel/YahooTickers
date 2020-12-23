#' A wrapper over \code{getSymbols} that maps over index components.
#'
#' \code{get_stocks()} allows you to quickly download stock indexes from the the \href{https://finance.yahoo.com/}{Yahoo Finance} without having to run \code{for} loops.
#'
#' @param tickers A \code{character} vector with the stock tickers.
#' @param from A date in the format \code{YYYY-MM-DD}.
#' @param to A date in the format \code{YYYY-MM-DD}.
#' @param periodicity One of: \code{daily}, \code{weely} or \code{monthly}.
#' @param simplify If TRUE, a \code{tibble} is returned. If FALSE a list with two elements is returned:
#'
#'   \itemize{
#'     \item \code{result}: with the downloads that succeeded;
#'     \item \code{error}: with the stocks not covered by the Yahoo Finance.
#'   }
#'
#' The defaul is TRUE.
#'
#' @param otherwise Argument passed to \code{\link[purrr]{safely}}.
#' @param quiet Argument passed to \code{\link[purrr]{safely}}. A notification is showed whenever an error occurs. Default is \code{TRUE}.
#'
#' @return A tidy \code{tibble} if \code{simplify = TRUE} and a \code{list} of two components if \code{simplify = FALSE}.
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#'
#' @seealso \href{https://purrr.tidyverse.org/reference/safely.html}{safely()}
#'
#' @export
#'
#' @examples
#' # Get stocks from the Merval Stock Index (Argentina)
#' library(YahooTickers)
#' ticks <- get_tickers(merval)
#' get_stocks(ticks, periodicity = "monthly")
get_stocks <- function(tickers,
                       from        = "2010-01-01",
                       to          = Sys.Date(),
                       periodicity = "daily",
                       simplify    = TRUE,
                       otherwise   = NULL,
                       quiet       = TRUE) {

  # check inputs
  if (!purrr::is_vector(tickers)) {
    rlang::abort("The tickers argument must be an atomic vector of dimension N x 1.")
  } else if (!purrr::is_logical(quiet)) {
    rlang::abort("The quiet argument must be TRUE or FALSE.")
  } else if (!purrr::is_logical(simplify)) {
    rlang::abort("The quiet argument must be TRUE or FALSE.")
  }

  periodicity <- periodicity %>%
    stringr::str_trim(., side = "both") %>%
    stringr::str_remove(., " ") %>%
    stringr::str_to_lower(.)

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

  options("getSymbols.warning4.0" = FALSE)

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
          auto.assign = FALSE
        )
      ),

      # unnest result
      result = purrr::map(
        .x = .data$download,
        .f = "result"
      ) %>%
        purrr::map(.f = timetk::tk_tbl),

      # unnest error
      error = purrr::map_chr(
        .x = .data$download,
        .f = ~ dplyr::if_else(
          condition = purrr::is_null(.$error),
          true      = NA_character_,
          false     = .$error$message
        )
      )
    )

  stocks_simple <- stocks %>%
    dplyr::filter(is.na(.data$error)) %>%
    dplyr::mutate(result = purrr::map(
      .x = .data$result,
      .f = ~ dplyr::rename(
        .data    = .,
        date     = 1,
        open     = 2,
        high     = 3,
        low      = 4,
        close    = 5,
        volume   = 6,
        adjusted = 7
        )
      )
    ) %>%
    tidyr::unnest(.data$result) %>%
    dplyr::select(
      .data$date,
      .data$tickers,
      .data$open,
      .data$high,
      .data$low,
      .data$close,
      .data$volume,
      .data$adjusted
      ) %>%
    dplyr::mutate_if(purrr::is_character, forcats::as_factor) #%>%

  # FIXME must ensure that simplify is a logical argument
  if (simplify) {

    tibble::new_tibble(x = stocks_simple, nrow = nrow(stocks_simple), class = 'YahooTickers')

  } else {

    error_simple <- stocks %>%
      dplyr::filter(!is.na(.data$error)) %>%
      dplyr::select(.data$error)

    if (nrow(error_simple) == 0) {
      error_simple <- "No error found."
    }

    list(
      result = tibble::new_tibble(x = stocks_simple, nrow = nrow(stocks_simple), class = 'YahooTickers'),
      error  = tibble::new_tibble(x = error_simple, nrow = nrow(error_simple), class = 'YahooTickers')
    )

  }

}

