#' PxAPI
#'
#' Lightweight R6 client for Statistics Sweden PxAPI.  This mirrors a subset
#' of the functions provided by the python package `pxstatpy` and allows
#' fetching configuration, navigating the API tree and retrieving table data.
#'
#' @examples
#' api <- PxAPI$new("https://api.scb.se/pxweb/api/v2")
#' conf <- api$get_config()
#' nav  <- api$get_navigation_root()
#' tbl  <- api$get_table_metadata("AM0110A1")
#'
#' @export
PxAPI <- R6::R6Class(
  "PxAPI",
  public = list(
    base_url = NULL,
    api_key = NULL,
    language = "en",

    #' @description
    #' Create a new PxAPI instance.
    #' @param base_url Base URL of the PxAPI service.
    #' @param api_key Optional API key.
    #' @param language Language used for responses.
    initialize = function(base_url, api_key = NULL, language = "en") {
      self$base_url <- sub("/$", "", base_url)
      self$api_key  <- api_key
      self$language <- language
    },

    #' @description
    #' Retrieve API configuration settings.
    get_config = function() {
      url <- paste0(self$base_url, "/config")
      r <- httr::GET(url, query = list(lang = self$language),
                     httr::add_headers(Authorization = paste("Bearer", self$api_key)))
      httr::stop_for_status(r)
      jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"))
    },

    #' @description
    #' Get navigation root folders and tables.
    get_navigation_root = function() {
      url <- paste0(self$base_url, "/navigation")
      r <- httr::GET(url, query = list(lang = self$language),
                     httr::add_headers(Authorization = paste("Bearer", self$api_key)))
      httr::stop_for_status(r)
      jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"))
    },

    #' @description
    #' Navigate to a specific folder.
    #' @param folder_id Folder identifier returned by `get_navigation_root`.
    get_navigation_by_id = function(folder_id) {
      url <- paste0(self$base_url, "/navigation/", folder_id)
      r <- httr::GET(url, query = list(lang = self$language),
                     httr::add_headers(Authorization = paste("Bearer", self$api_key)))
      httr::stop_for_status(r)
      jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"))
    },

    #' @description
    #' Retrieve metadata for a table.
    get_table_metadata = function(table_id) {
      url <- paste0(self$base_url, "/tables/", table_id, "/metadata")
      r <- httr::GET(url, query = list(lang = self$language, outputFormat = "json-px"),
                     httr::add_headers(Authorization = paste("Bearer", self$api_key)))
      httr::stop_for_status(r)
      jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"))
    },

    #' @description
    #' Download table data.
    #' @param table_id Table identifier.
    #' @param value_codes Optional list of variable selections.
    #' @param output_format Desired output format ("json-stat2" by default).
    #' @param output_format_params Additional format parameters as character vector.
    get_table_data = function(table_id, value_codes = NULL,
                              output_format = "json-stat2",
                              output_format_params = NULL) {
      endpoint <- paste0("/tables/", table_id, "/data")
      url <- paste0(self$base_url, endpoint)

      query <- list(outputFormat = output_format, lang = self$language)
      if (!is.null(output_format_params)) {
        query$outputFormatParams <- paste(output_format_params, collapse = ",")
      }

      if (!is.null(value_codes)) {
        for (nm in names(value_codes)) {
          query[[paste0("valuecodes[", nm, "]")]] <- paste(value_codes[[nm]], collapse = ",")
        }
      }

      r <- httr::GET(url, query = query,
                     httr::add_headers(Authorization = paste("Bearer", self$api_key)))
      httr::stop_for_status(r)
      jsonlite::fromJSON(httr::content(r, "text", encoding = "UTF-8"))
    }
  )
)

#' Supported output formats
OutputFormat <- c(
  PX = "px",
  JSON_STAT2 = "json-stat2",
  CSV = "csv",
  XLSX = "xlsx",
  HTML = "html",
  JSON_PX = "json-px",
  PARQUET = "parquet"
)

#' Output format parameters
OutputFormatParam <- c(
  USE_CODES = "UseCodes",
  USE_TEXTS = "UseTexts",
  USE_CODES_AND_TEXTS = "UseCodesAndTexts",
  INCLUDE_TITLE = "IncludeTitle",
  SEPARATOR_TAB = "SeparatorTab",
  SEPARATOR_SPACE = "SeparatorSpace",
  SEPARATOR_SEMICOLON = "SeparatorSemicolon"
)
