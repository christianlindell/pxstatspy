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
    rate_limiter = NULL,

    #' @description
    #' Create a new PxAPI instance.
    #' @param base_url Base URL of the PxAPI service.
    #' @param api_key Optional API key.
    #' @param language Language used for responses.
    initialize = function(base_url, api_key = NULL, language = "en",
                          max_calls = 30, time_window = 10) {
      self$base_url <- sub("/$", "", base_url)
      self$api_key  <- api_key
      self$language <- language
      self$rate_limiter <- RateLimiter$new(max_calls, time_window)
    },

    #' @description
    #' Internal helper for making GET requests with rate limiting
    #' @param endpoint Endpoint path beginning with '/'
    #' @param query Named list of query parameters
    make_request = function(endpoint, query = list()) {
      self$rate_limiter$wait_if_needed()
      url <- paste0(self$base_url, endpoint)
      query$lang <- self$language
      headers <- list()
      if (!is.null(self$api_key)) {
        headers$Authorization <- paste("Bearer", self$api_key)
      }
      r <- httr::GET(url, query = query, httr::add_headers(.headers = headers))
      httr::stop_for_status(r)
      httr::content(r, "text", encoding = "UTF-8")
    },

    #' @description
    #' Retrieve API configuration settings.
    get_config = function() {
      txt <- self$make_request("/config")
      jsonlite::fromJSON(txt)
    },

    #' @description
    #' Get navigation root folders and tables.
    get_navigation_root = function() {
      txt <- self$make_request("/navigation")
      jsonlite::fromJSON(txt)
    },

    #' @description
    #' Navigate to a specific folder.
    #' @param folder_id Folder identifier returned by `get_navigation_root`.
    get_navigation_by_id = function(folder_id) {
      txt <- self$make_request(paste0("/navigation/", folder_id))
      jsonlite::fromJSON(txt)
    },

    #' @description
    #' Retrieve metadata for a table.
    get_table_metadata = function(table_id) {
      txt <- self$make_request(paste0("/tables/", table_id, "/metadata"),
                               query = list(outputFormat = "json-px"))
      jsonlite::fromJSON(txt)
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
      query <- list(outputFormat = output_format)
      if (!is.null(output_format_params)) {
        query$outputFormatParams <- paste(output_format_params, collapse = ",")
      }
      if (!is.null(value_codes)) {
        for (nm in names(value_codes)) {
          query[[paste0("valuecodes[", nm, "]")]] <- paste(value_codes[[nm]], collapse = ",")
        }
      }
      txt <- self$make_request(endpoint, query)
      jsonlite::fromJSON(txt)
    },

    #' @description
    #' Download table data as a data.frame using JSON-stat2
    #' @param table_id Table identifier.
    #' @param value_codes Optional list of variable selections.
    #' @param output_format_param Output format parameter controlling value format.
    get_data_as_dataframe = function(table_id, value_codes = NULL,
                                     output_format_param = OutputFormatParam["USE_TEXTS"]) {
      endpoint <- paste0("/tables/", table_id, "/data")
      query <- list(outputFormat = "json-stat2",
                    outputFormatParams = output_format_param)
      if (!is.null(value_codes)) {
        for (nm in names(value_codes)) {
          query[[paste0("valuecodes[", nm, "]")]] <- paste(value_codes[[nm]], collapse = ",")
        }
      }
      txt <- self$make_request(endpoint, query)
      out <- rjstat::fromJSONstat(txt)
      out[[1]]
    },

    #' @description
    #' List tables with simple filtering. Returns raw response when `display=FALSE`.
    find_tables = function(query = NULL, past_days = NULL,
                           include_discontinued = FALSE,
                           page_number = 1, page_size = NULL,
                           display = TRUE) {
      params <- list(query = query,
                     pastDays = past_days,
                     includeDiscontinued = include_discontinued,
                     pageNumber = page_number,
                     pageSize = page_size)
      params <- params[!vapply(params, is.null, logical(1))]
      txt <- self$make_request("/tables", params)
      data <- jsonlite::fromJSON(txt)
      if (display) {
        message(sprintf("Found %d tables (page %d of %d)",
                        data$page$totalElements,
                        data$page$pageNumber,
                        data$page$totalPages))
        return(invisible(NULL))
      }
      data
    },

    #' @description
    #' Convenience helper returning tables as a data.frame.
    find_tables_as_dataframe = function(query = NULL, past_days = NULL,
                                        include_discontinued = FALSE,
                                        all_pages = FALSE) {
      first <- self$find_tables(query, past_days, include_discontinued,
                                page_number = 1, display = FALSE)
      tables <- first$tables
      total_pages <- first$page$totalPages
      if (all_pages && total_pages > 1) {
        for (pg in 2:total_pages) {
          next_pg <- self$find_tables(query, past_days, include_discontinued,
                                     page_number = pg, display = FALSE)
          tables <- append(tables, next_pg$tables)
        }
      }
      df <- jsonlite::fromJSON(jsonlite::toJSON(tables))
      df
    },

    #' @description
    #' Fetch metadata for a table by id.
    get_table_by_id = function(table_id) {
      txt <- self$make_request(paste0("/tables/", table_id))
      jsonlite::fromJSON(txt)
    },

    #' @description
    #' Print available variables for a table
    print_table_variables = function(table_id, max_values = 10) {
      meta <- self$get_table_metadata(table_id)
      if (is.null(meta$variables)) return(invisible(NULL))
      for (var in meta$variables) {
        cat(sprintf("\n%s (%s):\n", var$label, var$id))
        vals <- head(var$values$label, max_values)
        cat(paste("  -", vals), sep = "\n")
      }
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
