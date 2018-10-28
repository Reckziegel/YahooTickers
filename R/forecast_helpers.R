#' A Collection of Utility Functions
#'
#' @importFrom magrittr "%>%"
#'
#' @name forecast_helpers
NULL


#' @rdname forecast_helpers
#' @param .tbl The \code{tibble} to be analized
#' @param .fun The function to be applied
#' @param ... Additional arguments to be passed to \code{...}.
ts_fitter <- function(.tbl, .fun, ...) {

  .fun <- purrr::as_mapper(.fun)

  .index_name <- .tbl %>%
    rsample::analysis(.) %>%
    dplyr::select_if(., .predicate = lubridate::is.Date) %>%
    names() %>%
    dplyr::sym(.)

  .tbl_index <- .tbl %>%
    rsample::analysis(.) %>%
    tibbletime::as_tbl_time(., index = !!.index_name) %>%
    tibbletime::get_index_col(.)


  .tbl %>%
    rsample::analysis(.) %>%
    timetk::tk_ts(., start = min(.tbl_index), end = max(.tbl_index), silent = TRUE) %>%
    .fun(...)

}



#' @rdname forecast_helpers
#' @param .split The columns that contains the splits
#' @param .model The model to be forecasted
ts_forecaster <- function(.split, .model) {

  n <- nrow(rsample::assessment(.split))

  .split %>%
    rsample::assessment(.) %>%
    dplyr::mutate(out_of_sample = forecast::forecast(.model, h = n)$mean)

}

