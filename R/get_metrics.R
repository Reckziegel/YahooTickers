#' Summarise Forecasting Models Errror Metrics
#'
#' Summarise the out-of-sample stocks forecasts by asset and time periods.
#'
#'
#' @param .tbl A \code{tibble} of the \code{tbl_time} class.
#' @param .group The column in which the data should be grouped. This will often be a column with stock tickers or stocks names.
#' @param .truth The truth results of a time series realization.
#' @param .forecast The out-of-sample forecasts from the chosen model.
#'
#' @return A \code{tibble} with the following summary statistics by asset: mse, rmse, mae, mape and mase.
#'
#' @export
#'
#' @examples
#' library(YahooTickers)
#' library(dplyr)
#' library(forecast)
#'
#' get_tickers(dow) %>%
#'   slice(2:3) %>%
#'   get_stocks(., periodicity = "monthly") %>%
#'   get_returns(., tickers, log, TRUE, adjusted) %>%
#'   get_models(., tickers, adjusted, 100, 1, FALSE, auto.arima,
#'              seasonal = FALSE, stationary = TRUE) %>%
#'   get_forecasts(.) %>%
#'   get_metrics(., tickers, adjusted, point_forecast)
get_metrics <- function(.tbl, .group, .truth, .forecast) {

  # tidy eval
  .group_expr    <- dplyr::enquo(.group)
  .truth_expr    <- dplyr::enquo(.truth)
  .forecast_expr <- dplyr::enquo(.forecast)
  #.aggregate_arg <- lazyeval::expr_text(.aggregate_by)

  # if (!is.null(.aggregate_by)) {
  #
  #   if (!any(purrr::map_lgl(.tbl, lubridate::is.Date))) {
  #     warning("There is no date column in the provided tibble. The entire period will be used.",
  #             immediate. = TRUE)
  #   } else if (.aggregate_arg %in% c("daily", "weekly", "monthly")) {
  #     if (.aggregate_arg == "daily") {
  #       .tbl <- .tbl %>%
  #         dplyr::mutate(time_col = lubridate::day(get_index_col(.tbl)))
  #     }
  #
  #   }
  #
  # }




  # calculate error metrics
  .tbl %>%
    dplyr::select(1:4) %>%
    dplyr::mutate(error = (!!.truth_expr) - (!!.forecast_expr)) %>%
    dplyr::summarise(

      # scale dependent errors
      mse  = mean(.data$error ^ 2),
      rmse = sqrt(.data$mse),
      mae  = mean(abs(.data$error)),

      # percentage errors
      mape = mean(abs((100 * .data$error) / (!!.truth_expr))),

      # scaled errors
      mase = mean(abs(.data$error / sum(.data$error)))

    ) %>%
    dplyr::ungroup()

}
