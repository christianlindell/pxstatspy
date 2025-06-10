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
    max_data_cells = 150000,

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
      self$max_data_cells <- 150000
    },

    #' @description
    #' Internal helper for making requests with rate limiting
    #' @param endpoint Endpoint path beginning with '/'
    #' @param query Named list of query parameters
    #' @param method HTTP method (GET or POST)
    #' @param body Optional request body for POST
    make_request = function(endpoint, query = list(), method = "GET", body = NULL) {
      self$rate_limiter$wait_if_needed()
      url <- paste0(self$base_url, endpoint)
      query$lang <- self$language
      headers <- list()
      if (!is.null(self$api_key)) {
        headers$Authorization <- paste("Bearer", self$api_key)
      }
      verb <- toupper(method)
      if (identical(verb, "GET")) {
        r <- httr::GET(url, query = query, httr::add_headers(.headers = headers))
      } else {
        r <- httr::VERB(verb, url, query = query, body = body,
                        encode = "json", httr::add_headers(.headers = headers))
      }
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
    #' Download table data using POST method with a selection body.
    #' @param table_id Table identifier.
    #' @param selection Named list describing the selection body.
    #' @param output_format Desired output format ("json-stat2" by default).
    #' @param output_format_params Additional format parameters.
    get_table_data_post = function(table_id, selection,
                                   output_format = "json-stat2",
                                   output_format_params = NULL) {
      endpoint <- paste0("/tables/", table_id, "/data")
      query <- list(outputFormat = output_format)
      if (!is.null(output_format_params)) {
        query$outputFormatParams <- paste(output_format_params, collapse = ",")
      }
      txt <- self$make_request(endpoint, query, method = "POST", body = selection)
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
      out <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
      private$process_jsonstat_to_df(out,
                                     output_format_param,
                                     clean_colnames = FALSE)
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
  ),
  private = list(
    format_number = function(num) {
      if (num >= 1e6) sprintf("%.1fM", num / 1e6)
      else if (num >= 1e3) sprintf("%.1fK", num / 1e3)
      else format(num, big.mark = ",", scientific = FALSE)
    },
    parse_selection_expression = function(expr, values) {
      codes <- vapply(values, function(v) v$code, character(1))
      expand_expression(expr, codes)
    },
    expand_selection_expressions = function(var_meta, codes) {
      all_codes <- vapply(var_meta$values, function(v) v$code, character(1))
      get_matching_codes(codes, all_codes)
    },
    count_values_for_code = function(var_meta, code) {
      all_codes <- vapply(var_meta$values, function(v) v$code, character(1))
      count_matching_values(code, all_codes)
    },
    matches_pattern = function(code, pattern) {
      code %in% get_matching_codes(pattern, code)
    },
    extract_variable_metadata = function(metadata_stat) {
      meta <- list()
      for (id in metadata_stat$id) {
        dim <- metadata_stat$dimension[[id]]
        type <- PxVariables$TYPE_REGULAR
        if (!is.null(metadata_stat$role)) {
          if (id %in% metadata_stat$role$time) type <- PxVariables$TYPE_TIME
          else if (id %in% metadata_stat$role$metric) type <- PxVariables$TYPE_CONTENTS
          else if (id %in% metadata_stat$role$geo) type <- PxVariables$TYPE_GEOGRAPHICAL
        } else {
          id_l <- tolower(id)
          if (id == PxVariables$TIME || id_l %in% tolower(PxVariables$TIME_ALTERNATIVES)) {
            type <- PxVariables$TYPE_TIME
          } else if (id == PxVariables$CONTENTS || id_l %in% tolower(PxVariables$CONTENTS_ALTERNATIVES)) {
            type <- PxVariables$TYPE_CONTENTS
          } else if (id == PxVariables$REGION || id_l %in% tolower(PxVariables$REGION_ALTERNATIVES)) {
            type <- PxVariables$TYPE_GEOGRAPHICAL
          }
        }
        elim <- FALSE
        if (!is.null(dim$extension$elimination)) elim <- dim$extension$elimination
        meta[[id]] <- list(id = id, type = type, elimination = elim)
      }
      meta
    },
    calculate_cells = function(table_id, value_codes = NULL, validate_max_cells = TRUE) {
      if (is.null(value_codes)) value_codes <- list()
      metadata <- self$get_table_metadata(table_id, output_format = "json-stat2")
      var_meta <- private$extract_variable_metadata(metadata)
      var_sizes <- as.list(setNames(metadata$size, metadata$id))
      for (nm in names(value_codes)) {
        if (is.null(var_meta[[nm]])) stop(sprintf("Invalid variable '%s' not found", nm))
      }
      values_per_var <- list()
      total_cells <- 1
      for (var in names(var_sizes)) {
        full_size <- var_sizes[[var]]
        if (!is.null(value_codes[[var]])) {
          all_codes <- names(metadata$dimension[[var]]$category$index)
          selected <- 0
          for (pat in value_codes[[var]]) {
            selected <- selected + count_matching_values(pat, all_codes)
            if (pat == "*") {selected <- length(all_codes); break}
          }
          if (selected == 0) stop(sprintf("No matching values for variable '%s'", var))
          values_per_var[[var]] <- selected
        } else {
          can_elim <- !is.null(var_meta[[var]]$elimination) && var_meta[[var]]$elimination
          values_per_var[[var]] <- if (can_elim) 1 else full_size
        }
        total_cells <- total_cells * values_per_var[[var]]
      }
      if (validate_max_cells && total_cells > self$max_data_cells) {
        stop(sprintf("Request would return %s cells which exceeds maximum of %s", private$format_number(total_cells), private$format_number(self$max_data_cells)))
      }
      list(total_cells = total_cells, values_per_var = values_per_var)
    },
    get_chunk_variable = function(cells_per_var, metadata_px, value_codes) {
      var_meta <- setNames(metadata_px$variables, vapply(metadata_px$variables, `[[`, character(1), "id"))
      non_chunkable <- character()
      for (id in names(var_meta)) {
        meta <- var_meta[[id]]
        if (meta$type %in% c(PxVariables$TYPE_TIME, PxVariables$TYPE_CONTENTS)) non_chunkable <- c(non_chunkable, id)
        if (!(id %in% names(value_codes)) && isTRUE(meta$elimination)) non_chunkable <- c(non_chunkable, id)
        if (id %in% names(value_codes)) {
          if (any(grepl("^(TOP|BOTTOM)", toupper(value_codes[[id]])))) non_chunkable <- c(non_chunkable, id)
        }
      }
      candidates <- cells_per_var[setdiff(names(cells_per_var), non_chunkable)]
      if (length(candidates) == 0) stop("Cannot chunk query - no suitable variables found for chunking")
      order_idx <- order(
        sapply(names(candidates), function(x) ifelse(var_meta[[x]]$type == PxVariables$TYPE_GEOGRAPHICAL, 1, 0)),
        unlist(candidates),
        names(candidates),
        decreasing = TRUE
      )
      best <- names(candidates)[order_idx[1]]
      c(best, candidates[[best]])
    },
    prepare_chunks = function(table_id, chunk_var, value_codes, metadata_stat) {
      dimension <- metadata_stat$dimension[[chunk_var]]
      all_codes <- names(dimension$category$index)
      if (!is.null(value_codes[[chunk_var]])) {
        all_values <- get_matching_codes(value_codes[[chunk_var]], all_codes)
      } else {
        all_values <- all_codes
      }
      cells_info <- private$calculate_cells(table_id, value_codes, FALSE)
      cells_per_value <- cells_info$total_cells / length(all_values)
      per_chunk <- max(1, as.integer(self$max_data_cells / cells_per_value))
      chunks <- list()
      i <- 1
      while (i <= length(all_values)) {
        vals <- all_values[i:min(length(all_values), i + per_chunk - 1)]
        vc <- value_codes
        vc[[chunk_var]] <- vals
        chunks[[length(chunks)+1]] <- vc
        i <- i + per_chunk
      }
      chunks
    },
    make_single_request = function(table_id, value_codes, code_lists = NULL,
                                   output_format = "json-stat2",
                                   output_format_params = NULL,
                                   heading = NULL, stub = NULL) {
      endpoint <- paste0("/tables/", table_id, "/data")
      query <- list(outputFormat = output_format)
      if (!is.null(output_format_params)) {
        query$outputFormatParams <- paste(output_format_params, collapse = ",")
      }
      if (length(value_codes)) {
        meta <- self$get_table_metadata(table_id, output_format = "json-stat2")
        for (nm in names(value_codes)) {
          all_codes <- names(meta$dimension[[nm]]$category$index)
          expanded <- get_matching_codes(value_codes[[nm]], all_codes)
          query[[paste0("valuecodes[", nm, "]")]] <- paste(expanded, collapse = ",")
        }
      }
      if (!is.null(code_lists)) {
        for (nm in names(code_lists)) {
          query[[paste0("codelist[", nm, "]")]] <- code_lists[[nm]]
        }
      }
      if (!is.null(heading)) query$heading <- paste(heading, collapse = ",")
      if (!is.null(stub)) query$stub <- paste(stub, collapse = ",")
      txt <- self$make_request(endpoint, query)
      jsonlite::fromJSON(txt, simplifyVector = FALSE)
    },
    process_jsonstat_to_df = function(data, output_format_param, clean_colnames) {
      dims <- data$dimension
      dim_ids <- unlist(data$id)
      codes <- lapply(dims, function(d) names(d$category$index))
      labels <- lapply(dims, function(d) unlist(d$category$label))

      grid <- expand.grid(codes, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
      names(grid) <- dim_ids
      grid$value <- unlist(data$value)

      # Apply label formatting
      if (identical(output_format_param, OutputFormatParam["USE_TEXTS"])) {
        for (nm in dim_ids) {
          grid[[nm]] <- labels[[nm]][match(grid[[nm]], codes[[nm]])]
        }
      } else if (identical(output_format_param, OutputFormatParam["USE_CODES_AND_TEXTS"])) {
        for (nm in dim_ids) {
          lab <- labels[[nm]][match(grid[[nm]], codes[[nm]])]
          grid[[nm]] <- paste(grid[[nm]], lab, sep = " - ")
        }
      }

      # Region dimension handling
      if (PxVariables$REGION %in% dim_ids) {
        region_id <- PxVariables$REGION
        region_lab <- labels[[region_id]][match(grid[[region_id]], codes[[region_id]])]
        grid <- dplyr::rename(grid, region_code = !!region_id)
        grid$region <- region_lab
        dim_ids[dim_ids == region_id] <- "region_code"
      }

      # Pivot on contents dimension
      cont_dim <- dim_ids[grepl(PxVariables$CONTENTS, dim_ids)]
      if (length(cont_dim) > 0) {
        grid <- tidyr::pivot_wider(grid, names_from = cont_dim, values_from = "value")
      }

      if (clean_colnames) {
        clean <- function(x) {
          x <- tolower(x)
          x <- gsub(" ", "_", x)
          x <- chartr("åäö", "aao", x)
          x <- gsub("[^a-z0-9_]+", "", x)
          x <- gsub("_+", "_", x)
          gsub("^_|_$", "", x)
        }
        names(grid) <- vapply(names(grid), clean, character(1))
      }
      grid
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
