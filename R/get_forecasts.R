#' Tidy Automatic Time Series Forecasting
#'
#' @param .tbl
#' @param .group
#' @param .col
#' @param .fun
#' @param initial
#' @param assess
#' @param cumulative
#' @param ... Additional parameters to be passed to \code{.fun}.
#'
#' @return
#' @export
#'
#' @examples
get_forecasts <- function(.tbl, .group, .col, .fun, initial, assess, cumulative, ...) {

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
        initial    = initial,
        assess     = assess,
        cumulative = cumulative
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

      # add an index
      !!.index_call := purrr::map(.x = oos, .f = ~ min(.[[!!.index_call]])),

      # scale dependent errors
      rmse = purrr::map_dbl(
        .x = oos,
        .f = ~ yardstick::rmse(., truth = !!.col_var, estimate = out_of_sample)),
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
    dplyr::select(!!.index_call, !!.group_call, !!.col_call, out_of_sample, rmse, mae, mape, mase) %>%
    dplyr::mutate_if(., .predicate = purrr::is_character, .funs = forcats::as_factor)

}
