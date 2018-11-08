#' Get The Tickers Required by the Yahoo API
#'
#' \code{get_tickers()} helps you to get the tickers of a specific stock index.
#'
#' Currently \code{get_tickers} supports the following stock indexes:
#'
#' \itemize{
#'   \item \code{dow}: Dow Jones Index (United States)
#'   \item \code{sp500}: S&P 500 Index (United States)
#'   \item \code{nasdaq}: Nasdaq Composite Index (United States)
#'   \item \code{nyse}: NYSE Composite Index (United States)
#'   \item \code{amex}: AMEX Composite Index (United States)
#'   \item \code{russell2000}: Russell 2000 Index (United States)
#'   \item \code{ftse100}: FTSE 100 Index (United Kingdom)
#'   \item \code{dax}: Deutsche Boerse AG Index (Germany)
#'   \item \code{cac40}: CAC 40 Index (France)
#'   \item \code{bel20}: BEL 20 (Belgium)
#'   \item \code{topix}: Topix Index (Tokyo)
#'   \item \code{hangseng}: Hang Seng Index (Hong Kong)
#'   \item \code{sensex}: BSE Sensex Index (India)
#'   \item \code{jakarta}: Jakarta Stock Exchange Composite Index (Indonesia)
#'   \item \code{bursa}: FTSE Bursa Malaysia KLCI Index (Malaysya)
#'   \item \code{nzx50}: NZX 50 Index (New Zealand)
#'   \item \code{kospi}: KOSPI Composite Index (South Korea)
#'   \item \code{taiex}: TWSE Taiwan
#'   \item \code{tsx}: Toronto Composite Index (Canada)
#'   \item \code{ibovespa}: Ibovespa Index (Brazil)
#'   \item \code{ipc}: BMV IPC Index (Mexico)
#'   \item \code{ipsa}: IPSA Index (Chile)
#'   \item \code{merval}: Merval Index (Argentina)
#'}
#'
#' @param exchange A single string.
#'
#' @return A vector of character strings.
#'
#' @importFrom magrittr "%>%"
#'
#' @export
#'
#' @examples
#' library(YahooTickers)
#'
#' # get the brazilian stocks index tickers
#' get_tickers(ibovespa)
#'
#' # get the sp500 stocks tickers
#' get_tickers(sp500)
#'
#' # The exchange argument should be unquoted. This throws an error.
#' \dontrun{
#' get_tickers("sp500")
#' }
get_tickers <- function(exchange) {

  exchange <- lazyeval::expr_text(exchange)

  exchange_arg <- rlang::arg_match(
    arg    = exchange,
    values = c("dow", "sp500", "nasdaq", "nyse", "amex", "russell2000", "ftse100",
               "dax", "cac40", "bel20", "topix", "hangseng", "sensex", "jakarta",
               "bursa", "nzx50", "kospi", "taiex", "tsx", "ibovespa", "ipc", "ipsa",
               "merval")
  )

  exchange <- exchange_arg %>%
    stringr::str_trim(., side = "both") %>%
    stringr::str_remove(., " ") %>%
    stringr::str_to_lower(.)

  tickers <- switch(exchange_arg,
                    sp500       = get_sp500(),
                    dow         = get_dow(),
                    nasdaq      = get_nasdaq(),
                    nyse        = get_nyse(),
                    amex        = get_amex(),
                    russell2000 = get_russell2000(),
                    ftse100     = get_ftse100(),
                    dax         = get_dax(),
                    cac40       = get_cac40(),
                    bel20       = get_bel20(),
                    topix       = get_topix(),
                    hangseng    = get_hangseng(),
                    sensex      = get_sensex(),
                    jakarta     = get_jakarta(),
                    bursa       = get_bursa(),
                    nzx50       = get_nzx50(),
                    kospi       = get_kospi(),
                    taiex       = get_taiex(),
                    tsx         = get_tsx(),
                    ibovespa    = get_ibov(),
                    ipc         = get_ipc(),
                    ipsa        = get_ipsa(),
                    merval      = get_merval()
  )

  tickers

}
