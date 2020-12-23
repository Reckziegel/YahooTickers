#' A collection of utility functions
#'
#' A collection of utility functions to get market data from the Web.
#'
#' @details NULL
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @name utils
NULL


# Web-scrapping functions -------------------------------------------------

#' @rdname utils
get_sp500 <- function() {

  path <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

  message(
    "Yahoo does not provide the S&P 500 components. Tickers been downloaded from: ",
    crayon::cyan("en.wikipedia.org/wiki/List_of_S%26P_500_companies"),
    "."
    )

  # rvest functions: Get table of stocks
  insistently_read_html <- purrr::insistently(f = xml2::read_html)

  path %>%
    insistently_read_html() %>%
    rvest::html_node(css = "table") %>%
    rvest::html_table(., fill = TRUE) %>%
    vctrs::vec_as_names_legacy(.) %>%
    dplyr::rename(tickers = "Symbol") %>%
    dplyr::select(.data$tickers) %>%
    dplyr::as_tibble()

}


#' @rdname utils
get_dow <- function() {

  path <- "https://finance.yahoo.com/quote/%5EDJI/components?p=%5EDJI"

  insistently_read_html <- purrr::insistently(f = xml2::read_html)

  path %>%
    insistently_read_html() %>%
    rvest::html_node(css = "table") %>%
    rvest::html_table() %>%
    data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = .data$Symbol) %>%
    dplyr::select(.data$tickers)

}


# @rdname utils
# get_nasdaq <- function() {
#
#   ## TODO fix this code
#   #path <- "https://www.nasdaq.com/screening/companies-by-industry.aspx?exchange##=NASDAQ&render=download"
#
#   message(
#     "Yahoo does not provide the NASDAQ components. Tickers been downloaded from: ",
#     crayon::cyan("www.nasdaq.com"),
#     "."
#     )
#
#   suppressMessages(
#     suppressWarnings(
#       readr::read_csv(file = path)
#     )
#   ) %>%
#     dplyr::rename(tickers = .data$Symbol) %>%
#     dplyr::select(.data$tickers)
#
# }


#' @rdname utils
get_nyse <- function() {

  path <- "https://money.cnn.com/data/markets/nyse/?page="
  pages <- 1:129
  paths <- stringr::str_c(path, pages)

  message(
    "Yahoo does not provide the NYSE components. Tickers been downloaded from: ",
    crayon::cyan("money.cnn.com/data/markets/nyse"),
    "."
    )

  map_paths <- function(paths) {

    insistently_read_html <- purrr::insistently(f = xml2::read_html)

    paths %>%
      insistently_read_html() %>%
      rvest::html_nodes(css = "table") %>%
      rvest::html_table() %>%
      purrr::chuck(4) %>%
      dplyr::select(.data$Company) %>%
      tidyr::separate(.data$Company, into = c("tickers", "company"), extra = "drop") %>%
      dplyr::select(.data$tickers) %>%
      dplyr::as_tibble()

  }

  purrr::map_df(.x = paths, .f = map_paths)

}


#' @rdname utils
get_amex <- function() {

  path <- "https://money.cnn.com/data/markets/amex/?page="
  pages <- 1:14
  paths <- stringr::str_c(path, pages)

  message(
    "Yahoo does not provide the AMEX components. Tickers been downloaded from: ",
    crayon::cyan("money.cnn.com/data/markets/amex")
  )

  map_paths <- function(paths) {

    insistently_read_html <- purrr::insistently(f = xml2::read_html)

    paths %>%
      insistently_read_html() %>%
      rvest::html_nodes(css = "table") %>%
      rvest::html_table() %>%
      purrr::chuck(4) %>%
      dplyr::select(.data$Company) %>%
      tidyr::separate(.data$Company, into = c("tickers", "company"), extra = "drop") %>%
      dplyr::select(.data$tickers) %>%
      dplyr::as_tibble()

  }

  purrr::map_df(.x = paths, .f = map_paths)

}


#' @rdname utils
get_russell2000 <- function() {

  path <- "https://money.cnn.com/data/markets/russell/?page="
  pages <- 1:10
  paths <- stringr::str_c(path, pages)

  message(
    "Yahoo does not provide the Russell 2000 components. Tickers been downloaded from: ",
    crayon::cyan("money.cnn.com/data/markets/russell"),
    "."
  )

  map_paths <- function(paths) {

    insistently_read_html <- purrr::insistently(f = xml2::read_html)

    paths %>%
      insistently_read_html() %>%
      rvest::html_nodes(css = "table") %>%
      rvest::html_table() %>%
      purrr::chuck(4) %>%
      dplyr::select(.data$Company) %>%
      tidyr::separate(.data$Company, into = c("tickers", "company"), extra = "drop") %>%
      dplyr::select(.data$tickers) %>%
      dplyr::as_tibble()

  }

  purrr::map_df(.x = paths, .f = map_paths)

}


#' @rdname utils
get_ftse100 <- function() {

  path <- "https://www.londonstockexchange.com/exchange/prices-and-markets/stocks/indices/summary/summary-indices-constituents.html?index=UKX&page="
  pages <- 1:6
  paths <- stringr::str_c(path, pages)

  message(
    "Yahoo does not provide the FTSE 100 components. Tickers been downloaded from: ",
    crayon::cyan("www.londonstockexchange.com"),
    "."
    )

  map_paths <- function(paths) {

    insistently_read_html <- purrr::insistently(f = xml2::read_html)

    paths %>%
      insistently_read_html() %>%
      rvest::html_nodes(css = "table") %>%
      rvest::html_table() %>%
      data.frame() %>%
      vctrs::vec_as_names_legacy() %>%
      dplyr::as_tibble() %>%
      dplyr::rename(tickers = 1) %>%
      dplyr::select(.data$tickers) %>%
      dplyr::mutate(tickers = stringr::str_c(.data$tickers, ".L") %>%
                      stringr::str_replace(., "\\..L", "\\.L")
                    )

  }

  purrr::map_df(.x = paths, .f = map_paths)

}


#' @rdname utils
get_dax <- function() {

  path <- "https://finance.yahoo.com/quote/%5EGDAXI/components?p=%5EGDAXI"

  insistently_read_html <- purrr::insistently(f = xml2::read_html)

  path %>%
    insistently_read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = .data$Symbol) %>%
    dplyr::select(.data$tickers)

}


#' @rdname utils
get_cac40 <- function() {

  # TODO for now get the first 30 components from yahoo website.
  # TODO find a website that offers the list of stocks with the right tickers

  # path <- "http://topforeignstocks.com/indices/components-of-the-cac-40-index/"
  #
  # message(
  #   "Yahoo does not provide the CAC 40 components. Tickers been downloaded from: ",
  #   crayon::cyan("topforeignstocks.com"),
  #   "."
  #   )

  path <- 'https://finance.yahoo.com/quote/%5EFCHI/components?p=%5EFCHI'

  message(
    "Yahoo does not provide the CAC 40 components, but it's website it's beeing used temporarily while no other reliable website with the tickers is found."
  )

  insistently_read_html <- purrr::insistently(f = xml2::read_html)

  # path %>%
  #   insistently_read_html() %>%
  #   rvest::html_nodes(css = "table") %>%
  #   rvest::html_table() %>%
  #   data.frame() %>%
  #   dplyr::as_tibble() %>%
  #   dplyr::rename(tickers = .data$`Ticker..in.Yahoo.Finance.`) %>%
  #   dplyr::select(.data$tickers)

  path %>%
    insistently_read_html() %>%
    rvest::html_node(css = "table") %>%
    rvest::html_table() %>%
    data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = .data$Symbol) %>%
    dplyr::select(.data$tickers)

}


#' @rdname utils
get_bel20 <- function() {

  path <- "https://finance.yahoo.com/quote/%5EBFX/components?p=%5EBFX"

  insistently_read_html <- purrr::insistently(f = xml2::read_html)

  path %>%
    insistently_read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    as.data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = .data$Symbol) %>%
    dplyr::select(.data$tickers)

}


#' @rdname utils
get_topix <- function() {

  message(
    "Yahoo does not provide the NYSE components. Tickers downloaded from: ",
    crayon::cyan("www.jpx.co.jp/english/markets/indices/topix"),
    "."
    )

  url <- "https://www.jpx.co.jp/english/markets/indices/topix/tvdivq00000030ne-att/TOPIX_weight_en.xlsx"
  destfile <- "TOPIX_weight_en.xlsx"

  insistently_curl_download <- purrr::insistently(f = curl::curl_download)

  insistently_curl_download(url, destfile)

  suppressWarnings(readxl::read_excel(destfile)) %>%
    dplyr::filter(.data$`New Index Series Code` == "TOPIX Core30") %>%
    dplyr::select(.data$Code) %>%
    dplyr::rename(tickers = .data$Code) %>%
    dplyr::mutate(tickers = stringr::str_c(.data$tickers, ".T")) %>%
    dplyr::distinct()

}


#' @rdname utils
get_hangseng <- function() {

  # path <- "http://topforeignstocks.com/indices/components-of-the-hang-seng-index/"
  #
  # message(
  #   "Yahoo does not provide the Hang Seng components. Tickers been downloaded from: ",
  #   crayon::cyan("topforeignstocks.com"),
  #   "."
  #   )
  #
  # insistently_read_html <- purrr::insistently(f = xml2::read_html)
  #
  # path %>%
  #   insistently_read_html() %>%
  #   rvest::html_nodes(css = "table") %>%
  #   rvest::html_table() %>%
  #   data.frame() %>%
  #   dplyr::as_tibble() %>%
  #   dplyr::rename(tickers = .data$Ticker) %>%
  #   dplyr::select(.data$tickers)
  #
  #    path <- 'https://finance.yahoo.com/quote/%5EFCHI/components?p=%5EFCHI'

  message(
    "Yahoo does not provide all the hangseng components, but it's website it's beeing used temporarily while no other reliable website with the tickers is found."
  )

  path <- "https://finance.yahoo.com/quote/%5EHSI/components"

  insistently_read_html <- purrr::insistently(f = xml2::read_html)

  path %>%
    insistently_read_html() %>%
    rvest::html_node(css = "table") %>%
    rvest::html_table() %>%
    data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = .data$Symbol) %>%
    dplyr::select(.data$tickers)

}


#' @rdname utils
get_sensex <- function() {

  # path <- "http://topforeignstocks.com/indices/components-of-the-sensex-index"
  #
  # message(
  #   "Yahoo does not provide the Sensex components. Tickers been downloaded from: ",
  #   crayon::cyan("topforeignstocks.com"),
  #   "."
  #   )
  #
  path <- "https://in.finance.yahoo.com/quote/%5EBSESN/components?p=%5EBSESN"

  insistently_read_html <- purrr::insistently(f = xml2::read_html)

  # path %>%
  #   insistently_read_html() %>%
  #   rvest::html_nodes(css = "table") %>%
  #   rvest::html_table() %>%
  #   data.frame() %>%
  #   dplyr::as_tibble() %>%
  #   dplyr::rename(tickers = .data$Scrip.Code) %>%
  #   dplyr::select(.data$tickers) %>%
  #   dplyr::mutate(tickers = stringr::str_c(.data$tickers, ".BO"))

  path %>%
    insistently_read_html() %>%
    rvest::html_node(css = "table") %>%
    rvest::html_table() %>%
    data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = .data$Symbol) %>%
    dplyr::select(.data$tickers)


}


#' @rdname utils
get_jakarta <- function() {

  path <- "http://topforeignstocks.com/indices/the-components-of-the-jakarta-composite-index-index"

  message(
    "Yahoo does not provide the Jakarta Index components. Tickers been downloaded from: ",
    crayon::cyan("topforeignstocks.com"),
    "."
  )

  insistently_read_html <- purrr::insistently(f = xml2::read_html)

  path %>%
    insistently_read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = .data$Code) %>%
    dplyr::select(.data$tickers) %>%
    dplyr::mutate(tickers = stringr::str_c(.data$tickers, ".JK"))

}


#' @rdname utils
get_bursa <- function() {

  # path <- "http://topforeignstocks.com/indices/the-components-of-the-ftse-bursa-malaysia-klci-index"
  #
  # message(
  #   "Yahoo does not provide the FTSE Bursa Index components. Tickers been downloaded from: ",
  #   crayon::cyan("topforeignstocks.com"),
  #   "."
  #   )
  #
  # insistently_read_html <- purrr::insistently(f = xml2::read_html)
  #
  # path %>%
  #   insistently_read_html() %>%
  #   rvest::html_nodes(css = "table") %>%
  #   rvest::html_table() %>%
  #   data.frame() %>%
  #   dplyr::as_tibble() %>%
  #   dplyr::rename(tickers = .data$Ticker) %>%
  #   stats::na.omit() %>%
  #   dplyr::select(.data$tickers)

  path <- "https://in.finance.yahoo.com/quote/%5EBSESN/components?p=%5EBSESN"

  insistently_read_html <- purrr::insistently(f = xml2::read_html)

  path %>%
    insistently_read_html() %>%
    rvest::html_node(css = "table") %>%
    rvest::html_table() %>%
    data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = .data$Symbol) %>%
    dplyr::select(.data$tickers)

}


#' @rdname utils
get_nzx50 <- function() {

  # path <- "http://topforeignstocks.com/indices/components-of-the-nzsx-50-index"
  #
  # message(
  #   "Yahoo does not provide the NZX 50 Index components. Tickers been downloaded from: ",
  #   crayon::cyan("topforeignstocks.com"),
  #   "."
  # )
  #
  # insistently_read_html <- purrr::insistently(f = xml2::read_html)
  #
  # path %>%
  #   insistently_read_html() %>%
  #   rvest::html_nodes(css = "table") %>%
  #   rvest::html_table() %>%
  #   purrr::chuck(1) %>%
  #   data.frame() %>%
  #   dplyr::as_tibble() %>%
  #   dplyr::rename(tickers = .data$Ticker) %>%
  #   dplyr::select(.data$tickers) #%>%
    #dplyr::mutate(tickers = stringr::str_c(.data$tickers, ".NZ"))

  path <- "https://nz.finance.yahoo.com/quote/%5ENZ50/components?p=%5ENZ50"

  insistently_read_html <- purrr::insistently(f = xml2::read_html)

  path %>%
    insistently_read_html() %>%
    rvest::html_node(css = "table") %>%
    rvest::html_table() %>%
    data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = .data$Symbol) %>%
    dplyr::select(.data$tickers)

}


#' @rdname utils
get_kospi <- function() {

  path <- "http://topforeignstocks.com/indices/the-components-of-the-korea-stock-exchange-kospi-index"

  message(
    "Yahoo does not provide the KOSPI Index components. Tickers been downloaded from: ",
    crayon::cyan("topforeignstocks.com"),
    "."
  )

  insistently_read_html <- purrr::insistently(f = xml2::read_html)

  path %>%
    insistently_read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = .data$Code) %>%
    dplyr::select(.data$tickers) %>%
    dplyr::mutate(tickers = stringr::str_c(.data$tickers, ".KS"))

}


#' @rdname utils
get_taiex  <- function() {

  path <- "http://topforeignstocks.com/indices/the-components-of-the-taiwan-stock-exchange-weighted-index"

  message(
    "Yahoo does not provide the TAIEX Index components. Tickers been downloaded from: ",
    crayon::cyan("topforeignstocks.com"),
    "."
    )

  insistently_read_html <- purrr::insistently(f = xml2::read_html)

  path %>%
    insistently_read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = .data$Code) %>%
    dplyr::select(.data$tickers) %>%
    dplyr::mutate(tickers = stringr::str_c(.data$tickers, ".TW"))

}


#' @rdname utils
get_tsx  <- function() {

  path <- "http://topforeignstocks.com/indices/the-components-of-the-sptsx-composite-index"

  message(
    "Yahoo does not provide the TSX Index components. Tickers been downloaded from: ",
    crayon::cyan("topforeignstocks.com"),
    "."
    )

  insistently_read_html <- purrr::insistently(f = xml2::read_html)

  path %>%
    insistently_read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = .data$Ticker) %>%
    dplyr::select(.data$tickers)

}


#' @rdname utils
get_ibov  <- function() {

  path <- "http://topforeignstocks.com/indices/components-of-the-bovespa-index"

  message(
    "Yahoo does not provide the Ibovespa Index components. Tickers been downloaded from: ",
    crayon::cyan("topforeignstocks.com"),
    "."
  )

  insistently_read_html <- purrr::insistently(f = xml2::read_html)

  path %>%
    insistently_read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = .data$Ticker) %>%
    dplyr::select(.data$tickers) %>%
    dplyr::mutate(tickers = stringr::str_c(.data$tickers, ".SA"))

}


#' @rdname utils
get_ipc  <- function() {

  url <- "https://latam.spindices.com/idsexport/file.xls?hostIdentifier=48190c8c-42c4-46af-8d1a-0cd5db894797&selectedModule=Constituents&selectedSubModule=ConstituentsFullList&indexId=92330739"

  message(
    "Yahoo does not provide the IPC Index components. Tickers been downloaded from: ",
    crayon::cyan("www.bmv.com.mx/en"),
    "."
    )

  destfile <- "ipc.xls"

  insistently_curl_download <- purrr::insistently(f = curl::curl_download)

  insistently_curl_download(url, destfile)

  ipc_tbl <- suppressWarnings(

    readxl::read_excel(destfile, skip = 9) %>%
    dplyr::mutate(Symbol = stringr::str_remove(.data$Symbol, " \\*")) %>%
    tidyr::separate(.data$Symbol, into = c("part1", "part2"), sep = " ", extra = "drop")

    )

  ipc_tbl %>%
    dplyr::mutate(part2 = dplyr::if_else(
      condition = .data$part2 %in% NA,
      true = "",
      false = .data$part2
      )
    ) %>%
    tidyr::unite(col = "tickers", .data$part1, .data$part2, sep = "") %>%
    dplyr::select(.data$tickers) %>%
    dplyr::filter(.data$tickers != "NA") %>%
    dplyr::mutate(tickers = stringr::str_c(.data$tickers, ".MX"))

}


#' @rdname utils
get_ipsa <- function() {

  path <- "https://finance.yahoo.com/quote/%5EIPSA/components?p=%5EIPSA"

  insistently_read_html <- purrr::insistently(f = xml2::read_html)

  path %>%
    insistently_read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = .data$Symbol) %>%
    dplyr::select(.data$tickers)

}


#' @rdname utils
get_merval <- function() {

  path <- "https://finance.yahoo.com/quote/%5EMERV/components?p=%5EMERV"

  insistently_read_html <- purrr::insistently(f = xml2::read_html)

  path %>%
    insistently_read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    data.frame() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = .data$Symbol) %>%
    dplyr::select(.data$tickers)

}


# Other Auxiliary Functions -----------------------------------------------

#' @rdname utils
#' @param .fun A valid function from the forecast package.
validate_funs <- function(.fun) {

  fun_name <- lazyeval::expr_text(.fun)

  acceptable_funs <- c("arfima", "Arima", "auto.arima", "ets", "baggedModel",
                       "baggedETS", "bats", "tbats", "nnetar", "tslm")

  if (!(fun_name %in% acceptable_funs)) {

    message("YahooTickers currently only support functions from the forecast package.",
            "\n",
            "Please, change the argument ", crayon::cyan(".fun"), " to one of the options bellow:"
            )

    purrr::iwalk(
      .x = acceptable_funs,
      .f = ~ cat(.y, ":", .x, "\n")
      )

    stop()

  }

}


