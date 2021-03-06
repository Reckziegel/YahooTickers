#' True Out-of-Sample Time Series Forecasting
#'
#' \code{get_forecasts()} extracts the true out-of-sample forecasts from an object previously fitted with \code{get_models()}.
#'
#'
#' @param .tbl A tidy \code{tibble} of the \code{tbl_time} class.
#' @param ... Additional parameters to be passed to \code{forecast}.
#'
#' @return A tidy \code{tibble}.
#'
#' @importFrom rlang ":="
#' @importFrom magrittr "%>%"
#'
#' @seealso \href{https://tidymodels.github.io/rsample/reference/rolling_origin.html}{rolling_origin()}
#'          \href{https://pkg.robjhyndman.com/forecast/}{forecast}
#'
#' @export
#'
#' @examples
#' library(YahooTickers)
#' library(dplyr)
#' library(forecast)
#'
#' # Download and forecast time series using the "auto.arima"
#' # function from the forecast package
#' get_tickers(dow) %>%
#'   slice(1:2) %>%
#'   get_stocks(., periodicity = "monthly") %>%
#'   get_returns(., tickers, log, TRUE, adjusted) %>%
#'   get_models(., tickers, adjusted, 100, 1, FALSE, auto.arima,
#'              seasonal = FALSE, stationary = TRUE) %>%
#'   get_forecasts(.) # h = 1 because .assess = 1
get_forecasts <- function(.tbl, ...) {

  UseMethod("get_forecasts", .tbl)

}



# Method default ----------------------------------------------------------

#' @rdname get_forecasts
#' @export
get_forecasts.default <- function(.tbl, ...) {

  rlang::abort(".tbl must be a tibble object.")

}



# Method tbl --------------------------------------------------------------

#' @rdname get_forecasts
#' @export
get_forecasts.tbl_df <- function(.tbl, ...) {

  if (inherits(.tbl, 'YahooTickers')) {

    if (!("get_models" %in% names(attributes(.tbl)))) {
      rlang::abort("The .tbl argument must be an object returned from get_models().")
    }

    # tidy eval
    .dots_expr <- dplyr::enquos(...)

    # find index name
    .index_name <- .tbl %>%
      dplyr::select_if(., .predicate = lubridate::is.Date) %>%
      names() %>%
      dplyr::sym()


    # verify if the forecasting horizon is identical to .assess argument in get_models()
    if (!(purrr::is_empty(.dots_expr$h))) {

      # if not coerce
      if (rlang::eval_tidy(.dots_expr$h) != attributes(.tbl)$get_models[["h"]]) {

        warning("The forecast horizon should be the same as the '.assess' argument in get_models(). ",
                "Setting h = ", attributes(.tbl)$get_models[["h"]], ".", immediate. = TRUE)

        .dots_expr$h <- attributes(.tbl)$get_models[["h"]]

      }

    }

    # extract attributes from .tbl
    .fc_tbl <- attributes(.tbl)[["get_models"]][["attr_tbl"]]

    # tidy_eval
    .index_col <- .index_name # tibbletime::get_index_char(.tbl)
    .group_col <- attributes(.tbl)$get_models$.group_col


    # wrangle
    .fc_tbl <- .fc_tbl %>%
      tidyr::unnest(.data$assessment_tbl) %>%
      dplyr::group_by(!!.group_col) %>%

      # forecast
      dplyr::mutate(
        forecast_col = purrr::map(
          .x = .data$models,
          .f = ~ forecast::forecast(., !!!.dots_expr) %>%
            sweep::sw_sweep() %>%
            dplyr::slice(nrow(.)) #%>%
            #dplyr::select(-c(.data$index:.data$key))
        )
      ) %>%

      # reorganize
      tidyr::unnest(.data$forecast_col) %>%
      dplyr::select(-c(.data$splits:.data$analysis_xts, .data$models)) %>%
      dplyr::select(.data[[!!.index_col]], !!.group_col, dplyr::everything()) %>%
      dplyr::select(-c(4, 5)) %>%
      dplyr::rename(point_forecast = "value") %>%
      #dplyr::mutate_if(., purrr::is_character, forcats::as_factor) %>%
      dplyr::distinct(.data[[!!.index_col]], .keep_all = TRUE) %>%
      dplyr::ungroup()

  } else {

    rlang::abort('The object passed to get_forecasts() must be of a class YahooTickers.')

  }

  tibble::new_tibble(x = .fc_tbl, nrow = nrow(.fc_tbl), class = 'YahooTickers')

}
