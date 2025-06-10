#' PxAPI
#'
#' Minimal R6 client for Statistics Sweden PxAPI.
#'
#' @examples
#' api <- PxAPI$new("https://api.scb.se/pxweb/api/v2")
#' conf <- api$get_config()
#'
#' @export
PxAPI <- R6::R6Class(
  "PxAPI",
  public = list(
    base_url = NULL,
    api_key = NULL,
    language = "en",
    initialize = function(base_url, api_key = NULL, language = "en") {
      self$base_url <- sub("/$", "", base_url)
      self$api_key <- api_key
      self$language <- language
    },
    get_config = function() {
      url <- paste0(self$base_url, "/config")
      r <- httr::GET(url, query = list(lang = self$language),
                     httr::add_headers(Authorization = paste("Bearer", self$api_key)))
      httr::stop_for_status(r)
      jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"))
    },
    get_table_data = function(table_id, output_format = "json-stat2") {
      endpoint <- paste0("/tables/", table_id, "/data")
      url <- paste0(self$base_url, endpoint)
      r <- httr::GET(url,
                     query = list(outputFormat = output_format, lang = self$language),
                     httr::add_headers(Authorization = paste("Bearer", self$api_key)))
      httr::stop_for_status(r)
      jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"))
    }
  )
)
