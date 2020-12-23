#' Getters
#'
#' Accessors to attributes of `YahooTickers` objects.
#'
#' @param .tbl A `YahooTickers` object.
#' @param .index The date column.
#'
#' @name getters
#' @export
get_index_col <- function(.tbl) {

  # FIXME fix those gatters

  #if (inherits(.tbl, "YahooTickers")) {

  index_col <- purrr::map_lgl(.x = .tbl, .f = lubridate::is.Date)
  #index_col <- dplyr::select_if(.tbl, lubridate::is.Date)

  if (is.null(index_col)) {
    rlang::abort("No index was found in the object provided. \n",
                 "Be sure of not diselect this column if you want to work with get_models().")
  }

  # } else {
  #
  #   rlang::abort("Object must be of a class `YahooTickers`.")
  #
  # }

  .tbl[index_col] #%>% dplyr::pull()

}


#' @rdname getters
#' @export
get_index_char <- function(.tbl) {

  #if (inherits(.tbl, 'YahooTickers')) {

  .tbl_names <- names(get_index_col(.tbl))

  if (is.null(.tbl_names)) {

    warning('No name found for the index column. \n',
            'Column name being arbitrarily set to `index`',
            immediate. = TRUE)
    .tbl_names <- 'date'

  }

  # } else {
  #
  #   rlang::abort("Object must be of a class `YahooTickers`.")
  #
  # }

  .tbl_names

}


#' @rdname getters
#' @export
get_index_quo <- function(.tbl, .index = get_index_char(.tbl)) {

  #.index <- get_index_char(.tbl)
  .index <- rlang::quo(.index)
  .index
}


