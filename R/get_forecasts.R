#' Automatic Cross Validation for Time Series Forecasting
#'
#'
#'
#' @param .tbl A tidy \code{tibble} of the \code{tbl_time} class.
#' @param .group The column in which the data should be grouped. This will often be a column containing the tickers or stocks names.
#' @param .col The column names in which the calculation shoud be conducted.
#' @param .fun A forecasting function from the \code{forecast} package.
#' @param .initial The period used to train the model in each split.
#' @param .assess The forecasting horizon.
#' @param .cumulative If \code{FALSE} forecasts are evaluated in a rolling windown; if \code{TRUE} forecasts accumulate from the origin of each split. The default is \code{TRUE}.
#' @param ... Additional parameters to be passed to \code{.fun}.
#'
#' @return A tidy \code{tibble}.
#'
#' @importFrom rlang ":="
#' @importFrom magrittr "%>%"
#'
#' @export
#'
#' @examples
#' library(YahooTickers)
#' library(dplyr)
#' library(forecast)
#'
#' # Download and forecast time series using the "auto.arima" function from the forecast package
#' get_tickers(dow) %>%
#'   slice(1:5) %>%
#'   get_stocks(., periodicity = "monthly") %>%
#'   get_returns(., tickers, arithmetic, adjusted) %>%
#'   get_forecasts(., tickers, return_adjusted, 60, 1, FALSE, auto.arima,
#'                 seasonal = FALSE, stationary = TRUE)
get_forecasts <- function(.tbl, .group, .col, .initial, .assess, .cumulative, .fun, ...) {

  if (!("tbl_time" %in% class(.tbl))) {
    rlang::abort(".tbl must be a 'tbl_time' object. Check the package 'tibbletime' for details.")
  }

  # tidy eval
  .col_var    <- dplyr::enquo(.col)
  .group_var  <- dplyr::enquo(.group)
  .col_call   <- dplyr::quo_name(.col_var)
  .group_call <- dplyr::quo_name(.group_var)
  .index      <- tibbletime::get_index_quo(.tbl)
  .index_call <- dplyr::quo_name(.index)
  .dots_expr  <- dplyr::enquos(..., .named = TRUE)
  .fun        <- purrr::as_mapper(.fun)

  # build the splits partition
  .tbl %>%
    dplyr::group_by(!!.group_var) %>%
    tidyr::nest(!!.index, !!.col_var) %>%
    dplyr::mutate(samples = purrr::map(
      .x = data,
      .f = ~  rsample::rolling_origin(
        data       = .x,
        initial    = .initial,
        assess     = .assess,
        cumulative = .cumulative
      )
    )
    ) %>%
    tidyr::unnest(samples) %>%

    # fit the models
    dplyr::group_by(!!.group_var) %>%
    dplyr::mutate(fits = purrr::map(
      .x = splits,
      .f = ~ ts_fitter(., .fun = .fun, !!!.dots_expr)
    )
    ) %>%

    # forecast
    dplyr::mutate(oos = purrr::map2(
      .x = splits,
      .y = fits,
      .f = ~ ts_forecaster(.x, .y)
    )
    ) %>%

    # extract out-of-sample CV statistics
    dplyr::group_by(!!.group_var, id) %>%
    dplyr::mutate(

      # add an index from the assessment perspective
      !!.index_call := purrr::map(
        .x = oos,
        .f = ~ min(rsample::assessment(.)[[!!.index_call]])
        ),

      # scale dependent errors
      rmse = purrr::map_dbl(
        .x = oos,
        .f = ~ yardstick::rmse(., truth = !!.col_var, estimate = out_of_sample)),
      mse  = rmse ^ 2,
      mae  = purrr::map_dbl(
        .x = oos,
        .f = ~ yardstick::mae(., truth = !!.col_var, estimate = out_of_sample)),

      # percentage errors
      mape  = purrr::map_dbl(
        .x = oos,
        .f = ~ mean(abs(((.[[!!.col_call]]) - .$out_of_sample) / (.[[!!.col_call]])) / 100))
    ) %>%

    dplyr::ungroup() %>%
    dplyr::group_by(!!.group_var) %>%
    dplyr::mutate(

      # scaled errors
      mase = purrr::map_dbl(
        .x = oos,
        .f = ~ mean(abs((.[[!!.col_call]] - .$out_of_sample) / sum(.[[!!.col_call]] - .$out_of_sample)))
      ),

      # unnest y_t and the forecasts
      out_of_sample = purrr::map_dbl(
        .x = oos,
        .f = ~ dplyr::last(.[["out_of_sample"]])
      ),
      !!.col_call := purrr::map_dbl(
        .x = oos,
        .f = ~ dplyr::last(.[[!!.col_call]])
      )
    ) %>%

    # tidy
    tidyr::unnest(!!.index) %>%
    dplyr::ungroup() %>%

    # reorder variables
    dplyr::select(!!.index_call, !!.group_call, !!.col_call, out_of_sample, rmse, mse, mae, mape, mase) %>%
    dplyr::mutate_if(., .predicate = purrr::is_character, .funs = forcats::as_factor)

}
