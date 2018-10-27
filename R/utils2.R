
# super fitter ------------------------------------------------------------

ts_fitter <- function(.tbl, .fun, ...) {

  .fun <- purrr::as_mapper(.fun)

  .tbl %>%
    rsample::analysis(.) %>%
    timetk::tk_ts(., frequency = 12, silent = TRUE) %>%
    .fun(...)

}



# forecaster --------------------------------------------------------------

ts_forecaster <- function(.split, .model) {

  n <- nrow(rsample::assessment(.split))

  .split %>%
    rsample::assessment(.) %>%
    dplyr::mutate(out_of_sample = forecast::forecast(.model, h = n)$mean)

}

