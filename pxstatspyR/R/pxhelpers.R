#' RateLimiter
#'
#' Simple sliding window rate limiter for API calls.
#'
#' @export
RateLimiter <- R6::R6Class(
  "RateLimiter",
  public = list(
    max_calls = NULL,
    time_window = NULL,
    calls = NULL,
    initialize = function(max_calls, time_window) {
      self$max_calls <- max_calls
      self$time_window <- time_window
      self$calls <- numeric(0)
    },
    wait_if_needed = function() {
      now <- as.numeric(Sys.time())
      self$calls <- self$calls[self$calls >= now - self$time_window]
      if (length(self$calls) >= self$max_calls) {
        wait <- self$calls[1] + self$time_window - now
        if (wait > 0) Sys.sleep(wait)
        now <- as.numeric(Sys.time())
        self$calls <- self$calls[self$calls >= now - self$time_window]
      }
      self$calls <- c(self$calls, now)
    }
  )
)

#' NavigationExplorer
#'
#' Helper to explore navigation endpoints in a stateful way.
#'
#' @export
NavigationExplorer <- R6::R6Class(
  "NavigationExplorer",
  public = list(
    api = NULL,
    current_folder = NULL,
    history = NULL,
    initialize = function(api) {
      self$api <- api
      self$history <- character()
    },
    get_root = function() {
      self$current_folder <- self$api$get_navigation_root()
      self$history <- character()
      self$categorize(self$current_folder$folderContents)
    },
    navigate_to = function(folder_id) {
      self$current_folder <- self$api$get_navigation_by_id(folder_id)
      self$history <- c(self$history, folder_id)
      self$categorize(self$current_folder$folderContents)
    },
    go_back = function() {
      if (length(self$history) == 0) return(NULL)
      self$history <- head(self$history, -1)
      if (length(self$history) == 0) {
        self$get_root()
      } else {
        self$navigate_to(self$history[length(self$history)])
      }
    },
    categorize = function(contents) {
      list(
        folders = Filter(function(x) x$type == "FolderInformation", contents),
        tables = Filter(function(x) x$type == "Table", contents),
        headings = Filter(function(x) x$type == "Heading", contents)
      )
    }
  )
)

#' PxVariables
#'
#' Constants describing common PxAPI variable names and types. These mirrors
#' the helpers used in the python implementation and are useful when
#' processing metadata.
#'
#' @export
PxVariables <- list(
  TIME = "Tid",
  REGION = "Region",
  CONTENTS = "ContentsCode",
  TYPE_TIME = "TimeVariable",
  TYPE_GEOGRAPHICAL = "GeographicalVariable",
  TYPE_CONTENTS = "ContentsVariable",
  TYPE_REGULAR = "RegularVariable",
  TIME_ALTERNATIVES = c("tid", "\u00e5r", "m\u00e5nad", "kvartal", "period",
                        "time", "year", "quarter", "month"),
  REGION_ALTERNATIVES = c(
    "region", "land", "riket", "l\u00e4n", "kommun", "deso", "regso",
    "nuts2", "nuts3", "fa-region", "la-region", "kommunkod", "l\u00e4nskod",
    "geo", "area", "location", "county", "municipality", "country"
  ),
  CONTENTS_ALTERNATIVES = c(
    "inneh\u00e5ll", "tabellinneh\u00e5ll", "m\u00e5tt", "v\u00e4rde", "variabel",
    "contents", "measure", "metric", "value", "variable"
  )
)

#' Count how many codes match a pattern
#'
#' Supports wildcard globbing ("*" and "?") and selection expressions like
#' `TOP(5)` or `RANGE(2020,2023)`.
#' @param pattern Selection pattern
#' @param all_codes Character vector of all possible codes
#' @export
count_matching_values <- function(pattern, all_codes) {
  if (pattern == "*") {
    return(length(all_codes))
  }

  if (grepl("[\\*?]", pattern)) {
    matches <- grep(utils::glob2rx(pattern), all_codes, value = TRUE)
    return(length(matches))
  }

  if (grepl("\\(", pattern) && grepl("\\)", pattern)) {
    out <- try(expand_expression(pattern, all_codes), silent = TRUE)
    if (!inherits(out, "try-error")) {
      return(length(out))
    }
    return(ifelse(pattern %in% all_codes, 1, 0))
  }

  if (pattern %in% all_codes) 1 else 0
}

#' Expand a selection expression into codes
#'
#' @param expr Expression like 'TOP(5)' or 'RANGE(2020,2023)'
#' @param all_codes Character vector of all possible codes
#' @export
expand_expression <- function(expr, all_codes) {
  func <- toupper(sub("\\(.*", "", expr))
  arg_str <- sub(".*\\(", "", sub("\\).*", "", expr))
  args <- trimws(strsplit(arg_str, ",")[[1]])

  if (func == "TOP") {
    n <- as.integer(args[1])
    offset <- if (length(args) > 1) as.integer(args[2]) else 0
    return(all_codes[(offset + 1):(offset + n)])
  } else if (func == "BOTTOM") {
    n <- as.integer(args[1])
    offset <- if (length(args) > 1) as.integer(args[2]) else 0
    idx_start <- max(length(all_codes) - (n + offset) + 1, 1)
    idx_end <- length(all_codes) - offset
    return(all_codes[idx_start:idx_end])
  } else if (func == "RANGE") {
    if (length(args) != 2) stop("RANGE expression requires 2 arguments")
    start_idx <- match(args[1], all_codes)
    end_idx <- match(args[2], all_codes)
    return(all_codes[start_idx:end_idx])
  } else if (func == "FROM") {
    if (length(args) != 1) stop("FROM expression requires 1 argument")
    start_idx <- match(args[1], all_codes)
    return(all_codes[start_idx:length(all_codes)])
  } else if (func == "TO") {
    if (length(args) != 1) stop("TO expression requires 1 argument")
    end_idx <- match(args[1], all_codes)
    return(all_codes[1:end_idx])
  } else {
    stop(sprintf("Unknown selection expression: %s", func))
  }
}

#' Return all codes matching any of the patterns
#'
#' @param patterns Character vector of patterns
#' @param all_codes Character vector of all possible codes
#' @export
get_matching_codes <- function(patterns, all_codes) {
  matched <- character()
  for (pat in patterns) {
    if (pat == "*") {
      return(all_codes)
    }
    if (grepl("[\\*?]", pat)) {
      matches <- grep(utils::glob2rx(pat), all_codes, value = TRUE)
      matched <- c(matched, matches)
      next
    }
    if (grepl("\\(", pat) && grepl("\\)", pat)) {
      out <- try(expand_expression(pat, all_codes), silent = TRUE)
      if (!inherits(out, "try-error")) {
        matched <- c(matched, out)
        next
      }
    }
    if (pat %in% all_codes) matched <- c(matched, pat)
  }
  unique(matched)
}

