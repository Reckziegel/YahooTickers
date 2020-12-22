#' Summarise Forecasting Models Errror Metrics
#'
#' Summarise the out-of-sample stocks forecasts by asset and time periods.
#'
#' \code{...} wrappers around \code{collapse_by()}. It accepts any of the following arguments:
#'
#' \itemize{
#'   \item \code{period}: A character specification used for time-based grouping. The
#'                        general format to use is `"frequency period"` where frequency
#'                        is a number like 1 or 2, and period is an interval like weekly
#'                        or yearly. There must be a space between the two.
#'
#'                        Note that you can pass the specification in a flexible way:
#'
#'                        * 1 Year: `'1 year'` / `'1 Y'` / `'1 yearly'` / `'yearly'`
#'
#'                        This shorthand is available for year, quarter, month, day, hour, minute,
#'                        second, millisecond and microsecond periodicities. Additionally,
#'                        you have the option of passing in a vector of dates to use as custom
#'                        and more flexible boundaries.
#'
#'  \item \code{start_date}: Optional argument used to specify the start date for the
#'                           first group. The default is to start at the closest period
#'                           boundary below the minimum date in the supplied index.
#'
#'  \item \code{side}: Whether to return the date at the beginning or the end of the new period.
#'                     By default, the "end" of the period. Use "start" to change to the start of the period.
#'
#'  \item \code{clean}: Whether or not to round the collapsed index up / down to the next period boundary.
#'                      The decision to round up / down is controlled by the side argument.
#'
#'}
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
#'   slice(1:2) %>%
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
  #.dots_expr     <- dplyr::enquos(..., .named = TRUE)

  # # should the index be agreggated as well?
  # if (!rlang::is_empty(!!!.dots_expr)) {
  #
  #   .tbl       <- tibbletime::collapse_by(.tbl, !!!.dots_expr)
  #   .index_quo <- tibbletime::get_index_quo(.tbl)
  #
  #   .tbl_grouped <- .tbl %>%
  #     dplyr::group_by(!!.index_quo, !!.group_expr)
  #
  # } else {
  #
  #   .tbl_grouped <- .tbl %>%
  #     dplyr::group_by(!!.group_expr)
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
