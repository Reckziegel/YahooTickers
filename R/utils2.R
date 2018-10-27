
# super fitter ------------------------------------------------------------

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



# forecaster --------------------------------------------------------------

ts_forecaster <- function(.split, .model) {

  n <- nrow(rsample::assessment(.split))

  .split %>%
    rsample::assessment(.) %>%
    dplyr::mutate(out_of_sample = forecast::forecast(.model, h = n)$mean)

}

