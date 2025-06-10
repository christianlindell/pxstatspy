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
