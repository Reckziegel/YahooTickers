#' Extract in-sample statistics estimated in "real-time"
#'
#' \code{get_models()} recursively estimates time-series models by using rolling and expanding time windows.
#'
#' Under the hood, get_forecasts uses \code{rsample} infrastructure to sequentially access the model passed to \code{.fun}. The data y1, y2, ..., yt is fitted and the process repeats itself for each stock in the \code{.group} column.
#'
#' @param .tbl A tidy \code{tibble} or an object of the \code{tbl_time} class.
#' @param .group The column in which the data should be grouped. This will often be a column with stock tickers or stocks names.
#' @param .col The reference column in which the operations should be based on.
#' @param .initial The number of periods used to train the model in each split.
#' @param .assess The forecast horizon.
#' @param .cumulative If \code{TRUE} forecasts accumulate from the origin of each split. If \code{FALSE} forecasts are evaluated in a rolling windown. The default is TRUE.
#' @param .fun Currently supports only a forecasting function from the \code{forecast} package.
#' @param ... Additional parameters to be passed to \code{.fun}.
#'
#' @return A tidy \code{tibble} contaning the following in-sample statistics for each period:
#'
#'   \itemize{
#'     \item{term}: The parameters estimated by \code{.fun}
#'     \item{estimate}: The values of the estimated parameters
#'     \item{model.desc}: A description of the model including the three integer components (p, d, q) are the AR order, the degree of differencing, and the MA order.
#'     \item{sigma}: The square root of the estimated residual variance
#'     \item{logLik}: The data's log-likelihood under the model
#'     \item{AIC}: The Akaike Information Criterion
#'     \item{BIC}: The Bayesian Information Criterion
#'     \item{ME}: Mean error
#'     \item{RMSE}: Root mean squared error
#'     \item{MAE}: Mean absolute error
#'     \item{MPE}: Mean percentage error
#'     \item{MAPE}: Mean absolute percentage error
#'     \item{MASE}: Mean absolute scaled error
#'     \item{ACF1}: Autocorrelation of errors at lag 1
#' }
#'
#' @seealso \href{https://business-science.github.io/sweep/reference/sw_tidy.html}{sw_tidy()}
#'          \href{https://business-science.github.io/sweep/reference/tidiers_arima.html}{sw_glance()}
#'          \href{https://tidymodels.github.io/rsample/reference/rolling_origin.html}{rolling_origin()}
#'          \href{https://pkg.robjhyndman.com/forecast/}{forecast}
#'
#' @export
#'
#' @examples
#' library(YahooTickers)
#' library(dplyr)
#' library(forecast)
#' # Download and forecast time series using the "auto.arima" function
#' # from the forecast package
#' get_tickers(dow) %>%
#'   slice(1:2) %>%
#'   get_stocks(., periodicity = "monthly") %>%
#'   get_returns(., tickers, arithmetic, TRUE, ret_adj = adjusted) %>%
#'   get_models(., tickers, ret_adj, 60, 1, FALSE, auto.arima,
#'              seasonal = FALSE, stationary = TRUE,
#'              max.p = 1, max.q = 1)
get_models <- function(.tbl, .group, .col, .initial, .assess, .cumulative, .fun, ...) {

  UseMethod("get_models", .tbl)

}



# Method default ----------------------------------------------------------

#' @rdname get_models
#' @export
get_models.default <- function(.tbl, .group, .col, .initial, .assess, .cumulative, .fun, ...) {

  rlang::abort(".tbl must be a tibble object.")

}


# Method for tbl ----------------------------------------------------------

#' @rdname get_models
#' @export
get_models.tbl_df <- function(.tbl, .group, .col, .initial, .assess, .cumulative, .fun, ...) {


  # stop if `.fun` is not in the forecast package
  validate_funs(.fun)
  assertthat::assert_that(assertthat::is.number(.initial))
  assertthat::assert_that(assertthat::is.number(.assess))
  assertthat::assert_that(assertthat::is.flag(.cumulative))


  #tidy eval
  .group_expr <- dplyr::enquo(.group)
  .col_expr   <- dplyr::enquo(.col)
  .col_name   <- dplyr::quo_name(.col_expr)
  .index_name <- get_index_char(.tbl)
  .dots_expr  <- dplyr::enquos(...)
  .fun_mapper <- purrr::as_mapper(.fun)

  # manipulate
  .tbl <- .tbl %>%
    dplyr::group_by(!!.group_expr) %>%
    tidyr::nest(data = c(!!.index_name, !!.col_expr)) %>% #TODO verify if my get_index_col works here
    dplyr::mutate(samples = purrr::map(
      .x = .data$data,
      .f = ~ rsample::rolling_origin(., initial = .initial, assess = .assess, cumulative = .cumulative)
      )
    ) %>%
    tidyr::unnest(.data$samples) %>%
    dplyr::mutate(
      analysis_tbl   = purrr::map(.x = .data$splits,
                                  .f = ~ rsample::analysis(.x)),
      analysis_xts   = purrr::map(.x = .data$analysis_tbl,
                                  .f = ~ timetk::tk_xts(., silent = TRUE)), # FIXME
      assessment_tbl = purrr::map(.x = .data$splits,
                                  .f = ~ rsample::assessment(.x)),
      models         = purrr::map(.x = .data$analysis_xts,
                                  .f = ~ .fun_mapper(., !!!.dots_expr)),
      !!.index_name  := purrr::map(.x = .data$assessment_tbl,
                                   .f = ~ get_index_col(.) %>% dplyr::pull(.) %>% min(.)),
      !!.col_name    := purrr::map_dbl(.x = .data$assessment_tbl,
                                       .f = ~ dplyr::last(.[[!!.col_name]])),
      tidy_col        = purrr::map(.x = .data$models, .f = sweep::sw_tidy),
      glance_col      = purrr::map(.x = .data$models, .f = sweep::sw_glance)
    )

  # if `tidy_col` or `glance_col` have NA's the unnest function bugs.

  #.tbl %>% tidyr::unnest_wider(col = .data$tidy_col)
    # dplyr::mutate(
    #   tidy_col = purrr::map(.x = .data$tidy_col, .f = clean_bad_model_output))

                            #
                            # .f = ~ purrr::modify_if(
                            #   .x = .x,
                            #   .p = ~ NA %in% .x,
                            #   .f = ~ tidyr::replace_na(
                            #     data    = .x,
                            #     replace = list(NA_character_)) %>% unlist())))

    #   glance_col = purrr::map(.x = .data$glance_col,
    #                           .f = ~ purrr::modify_if(
    #                             .x = .x,
    #                             .p = purrr::map_lgl(.x = ., .f = ~ NA %in% .x),
    #                             .f = ~ .x %>%
    #                               tidyr::replace_na(list(0)r) %>%
    #                               unlist()
    #                             )
    #                           )
    # )

  .tbl <- .tbl %>%
    dplyr::group_by(.data[[!!dplyr::quo_name(.group_expr)]]) %>%
    tidyr::unnest(cols = .data[[!!.index_name]]) %>%
    tidyr::unnest(cols = .data$tidy_col) %>%
    tidyr::unnest(cols = .data$glance_col) %>%
    dplyr::ungroup() %>%

    dplyr::mutate_if(., .predicate = purrr::is_character, .funs = forcats::as_factor)


  # save attributes for forecasting
  .attr_tbl <- .tbl %>%
    dplyr::select(.data$splits:.data$models, !!.group_expr)

  attr(.tbl, "get_models") <- list(h = .assess, attr_tbl = .attr_tbl, .group_col = .group_expr)


  # make it clean and print
  .tbl <- .tbl %>%
    dplyr::select(!!.index_name, !!.group_expr, dplyr::everything(), -!!.col_expr, -c(.data$splits:.data$models))


  # return
  tibble::new_tibble(x = .tbl, nrow = nrow(.tbl), class = 'YahooTickers', .attr_tbl = .attr_tbl)


}
