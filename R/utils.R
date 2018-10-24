#' A Collection of Utility Functions
#'
#' @importFrom magrittr "%>%"
#'
#' @name utils
NULL


#' @rdname utils
get_sp500 <- function() {

  path <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

  message("Yahoo does not provide the S&P 500 components. Tickers been downloaded from: en.wikipedia.org/wiki/List_of_S%26P_500_companies")

  # rvest functions: Get table of stocks
  path %>%
    xml2::read_html() %>%
    rvest::html_node(., css = "table") %>%
    rvest::html_table(.) %>%
    dplyr::rename(tickers = `Ticker symbol`) %>%
    dplyr::select(tickers) %>%
    dplyr::as_tibble()

}


#' @rdname utils
get_dow <- function() {

  path <- "https://finance.yahoo.com/quote/%5EDJI/components?p=%5EDJI"

  path %>%
    xml2::read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    .[[1]] %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = "Symbol") %>%
    dplyr::select(tickers)

}


#' @rdname utils
get_nasdaq <- function() {

  path <- "https://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=NASDAQ&render=download"

  message("Yahoo does not provide the NASDAQ components. Tickers been downloaded from: www.nasdaq.com")

  suppressMessages(
    suppressWarnings(
      readr::read_csv(file = path)
    )
  ) %>%
    dplyr::rename(tickers = "Symbol") %>%
    dplyr::select(tickers)

}


#' @rdname utils
get_nyse <- function() {

  path <- "https://money.cnn.com/data/markets/nyse/?page="
  pages <- 1:128
  paths <- stringr::str_c(path, pages)

  message("Yahoo does not provide the NYSE components. Tickers been downloaded from: money.cnn.com/data/markets/nyse")

  map_paths <- function(paths) {

    paths %>%
      xml2::read_html() %>%
      rvest::html_nodes(css = "table") %>%
      rvest::html_table(.) %>%
      .[[5]] %>%
      dplyr::select(Company) %>%
      tidyr::separate(Company, into = c("tickers", "company"), extra = "drop") %>%
      dplyr::select(tickers) %>%
      dplyr::as_tibble()

  }

  purrr::map_df(.x = paths, .f = map_paths)

}


#' @rdname utils
get_amex <- function() {

  path <- "https://money.cnn.com/data/markets/amex/?page="
  pages <- 1:17
  paths <- stringr::str_c(path, pages)

  message("Yahoo does not provide the AMEX components. Tickers been downloaded from: money.cnn.com/data/markets/amex")

  map_paths <- function(paths) {

    paths %>%
      xml2::read_html() %>%
      rvest::html_nodes(css = "table") %>%
      rvest::html_table(.) %>%
      .[[4]] %>%
      dplyr::select(Company) %>%
      tidyr::separate(Company, into = c("tickers", "company"), extra = "drop") %>%
      dplyr::select(tickers) %>%
      dplyr::as_tibble()

  }

  purrr::map_df(.x = paths, .f = map_paths)

}


#' @rdname utils
get_russell2000 <- function() {

  path <- "https://money.cnn.com/data/markets/russell/?page="
  pages <- 1:87
  paths <- stringr::str_c(path, pages)

  message("Yahoo does not provide the Russell 2000 components. Tickers been downloaded from: money.cnn.com/data/markets/russell")

  map_paths <- function(paths) {

    paths %>%
      xml2::read_html() %>%
      rvest::html_nodes(css = "table") %>%
      rvest::html_table(.) %>%
      .[[4]] %>%
      dplyr::select(Company) %>%
      tidyr::separate(Company, into = c("tickers", "company"), extra = "drop") %>%
      dplyr::select(tickers) %>%
      dplyr::as_tibble()

  }

  purrr::map_df(.x = paths, .f = map_paths)

}


#' @rdname utils
get_ftse100 <- function() {

  path <- "https://www.londonstockexchange.com/exchange/prices-and-markets/stocks/indices/summary/summary-indices-constituents.html?index=UKX&page="
  pages <- 1:6
  paths <- stringr::str_c(path, pages)

  message("Yahoo does not provide the FTSE 100 components. Tickers been downloaded from: www.londonstockexchange.com")

  map_paths <- function(paths) {

    paths %>%
      xml2::read_html() %>%
      rvest::html_nodes(., css = "table") %>%
      rvest::html_table(.) %>%
      .[[1]] %>%
      tibble::set_tidy_names(., quiet = TRUE) %>%
      dplyr::as_tibble() %>%
      dplyr::select(Code) %>%
      dplyr::rename(tickers = "Code") %>%
      dplyr::mutate(tickers = stringr::str_c(tickers, ".L") %>%
                      stringr::str_replace(., "\\..L", "\\.L")
                    )

  }

  purrr::map_df(.x = paths, .f = map_paths)

}


#' @rdname utils
get_dax <- function() {

  path <- "https://finance.yahoo.com/quote/%5EGDAXI/components?p=%5EGDAXI"

  path %>%
    xml2::read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    .[[1]] %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = "Symbol") %>%
    dplyr::select(tickers)

}


#' @rdname utils
get_cac40 <- function() {

  path <- "http://topforeignstocks.com/indices/components-of-the-cac-40-index/"

  message("Yahoo does not provide the CAC 40  components. Tickers been downloaded from: topforeignstocks.com")

  path %>%
    xml2::read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    .[[1]] %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = `Ticker (in Yahoo Finance)`) %>%
    dplyr::select(tickers)

}


#' @rdname utils
get_bel20 <- function() {

  path <- "https://finance.yahoo.com/quote/%5EBFX/components?p=%5EBFX"

  path %>%
    xml2::read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    .[[1]] %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = "Symbol") %>%
    dplyr::select(tickers)

}


#' @rdname utils
get_topix <- function() {

  message("Yahoo does not provide the NYSE components. Tickers downloaded from: www.jpx.co.jp/english/markets/indices/topix")

  url <- "https://www.jpx.co.jp/english/markets/indices/topix/tvdivq00000030ne-att/TOPIX_weight_en.xlsx"
  destfile <- "TOPIX_weight_en.xlsx"
  curl::curl_download(url, destfile)

  suppressWarnings(readxl::read_excel(destfile)) %>%
    dplyr::filter(`New Index Series Code` == "TOPIX Core30") %>%
    dplyr::select(Code) %>%
    dplyr::rename(tickers = "Code") %>%
    dplyr::mutate(tickers = stringr::str_c(tickers, ".T")) %>%
    dplyr::distinct()

}


#' @rdname utils
get_hangseng <- function() {

  path <- "http://topforeignstocks.com/indices/components-of-the-hang-seng-index/"

  message("Yahoo does not provide the Hang Seng components. Tickers been downloaded from: topforeignstocks.com")

  path %>%
    xml2::read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    .[[1]] %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = "Ticker") %>%
    dplyr::select(tickers)

}


#' @rdname utils
get_sensex <- function() {

  path <- "http://topforeignstocks.com/indices/components-of-the-sensex-index"

  message("Yahoo does not provide the Sensex components. Tickers been downloaded from: topforeignstocks.com")

  path %>%
    xml2::read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    .[[1]] %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = `Scrip Code (on BSE)`) %>%
    dplyr::select(tickers) %>%
    dplyr::mutate(tickers = stringr::str_c(tickers, ".BO"))

}


#' @rdname utils
get_jakarta <- function() {

  path <- "http://topforeignstocks.com/indices/the-components-of-the-jakarta-composite-index-index"

  message("Yahoo does not provide the Jakarta Index components. Tickers been downloaded from: topforeignstocks.com")

  path %>%
    xml2::read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    .[[1]] %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = `Stock Code`) %>%
    dplyr::select(tickers) %>%
    dplyr::mutate(tickers = stringr::str_c(tickers, ".JK"))

}


#' @rdname utils
get_bursa <- function() {

  path <- "http://topforeignstocks.com/indices/the-components-of-the-ftse-bursa-malaysia-klci-index"

  message("Yahoo does not provide the FTSE Bursa Index components. Tickers been downloaded from: topforeignstocks.com")

  path %>%
    xml2::read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    .[[1]] %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = `Ticker (in Yahoo Finance)`) %>%
    dplyr::select(tickers)

}


#' @rdname utils
get_nzx50 <- function() {

  path <- "http://topforeignstocks.com/indices/components-of-the-nzsx-50-index"

  message("Yahoo does not provide the NZX 50 Index components. Tickers been downloaded from: topforeignstocks.com")

  path %>%
    xml2::read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    .[[1]] %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = "Ticker") %>%
    dplyr::select(tickers) %>%
    dplyr::mutate(tickers = stringr::str_c(tickers, ".NZ"))

}


#' @rdname utils
get_kospi <- function() {

  path <- "http://topforeignstocks.com/indices/the-components-of-the-korea-stock-exchange-kospi-index"

  message("Yahoo does not provide the KOSPI Index components. Tickers been downloaded from: topforeignstocks.com")

  path %>%
    xml2::read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    .[[1]] %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = "Code") %>%
    dplyr::select(tickers) %>%
    dplyr::mutate(tickers = stringr::str_c(tickers, ".KS"))

}


#' @rdname utils
get_taiex  <- function() {

  path <- "http://topforeignstocks.com/indices/the-components-of-the-taiwan-stock-exchange-weighted-index"

  message("Yahoo does not provide the TAIEX Index components. Tickers been downloaded from: topforeignstocks.com")

  path %>%
    xml2::read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    .[[1]] %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = "Code") %>%
    dplyr::select(tickers) %>%
    dplyr::mutate(tickers = stringr::str_c(tickers, ".TW"))

}


#' @rdname utils
get_tsx  <- function() {

  path <- "http://topforeignstocks.com/indices/the-components-of-the-sptsx-composite-index"

  message("Yahoo does not provide the TSX Index components. Tickers been downloaded from: topforeignstocks.com")

  path %>%
    xml2::read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    .[[1]] %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = `Ticker (on TSX)`) %>%
    dplyr::select(tickers)

}


#' @rdname utils
get_ibov  <- function() {

  path <- "http://topforeignstocks.com/indices/components-of-the-bovespa-index"

  message("Yahoo does not provide the Ibovespa Index components. Tickers been downloaded from: topforeignstocks.com")

  path %>%
    xml2::read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    .[[1]] %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = "Ticker") %>%
    dplyr::select(tickers) %>%
    dplyr::mutate(tickers = stringr::str_c(tickers, ".SA"))

}


#' @rdname utils
get_ipc  <- function() {

  url <- "https://latam.spindices.com/idsexport/file.xls?hostIdentifier=48190c8c-42c4-46af-8d1a-0cd5db894797&selectedModule=Constituents&selectedSubModule=ConstituentsFullList&indexId=92330739"

  message("Yahoo does not provide the IPC Index components. Tickers been downloaded from: www.bmv.com.mx/en")

  destfile <- "ipc.xls"

  curl::curl_download(url, destfile)

  readxl::read_excel(destfile, skip = 9) %>%
    dplyr::mutate(Symbol = stringr::str_remove(Symbol, c(" \\*"))) %>%
    tidyr::separate(Symbol, into = c("part1", "part2"), sep = " ") %>%
    dplyr::mutate(part2 = dplyr::if_else(
      condition = part2 %in% NA,
      true = "",
      false = part2
      )
    ) %>%
    tidyr::unite(tickers, part1, part2, sep = "") %>%
    dplyr::select(tickers) %>%
    dplyr::filter(tickers != "NA") %>%
    dplyr::mutate(tickers = stringr::str_c(tickers, ".MX"))

}


#' @rdname utils
get_ipsa <- function() {

  path <- "https://finance.yahoo.com/quote/%5EIPSA/components?p=%5EIPSA"

  path %>%
    xml2::read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    .[[1]] %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = "Symbol") %>%
    dplyr::select(tickers)

}


#' @rdname utils
get_merval <- function() {

  path <- "https://finance.yahoo.com/quote/%5EMERV/components?p=%5EMERV"

  path %>%
    xml2::read_html() %>%
    rvest::html_nodes(css = "table") %>%
    rvest::html_table() %>%
    .[[1]] %>%
    dplyr::as_tibble() %>%
    dplyr::rename(tickers = "Symbol") %>%
    dplyr::select(tickers)

}
