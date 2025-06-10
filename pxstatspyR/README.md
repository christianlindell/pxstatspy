# pxstatspyR

This package provides an R interface to the PxAPI used by Statistics Sweden.
It mirrors a subset of the functionality available in the Python package
`pxstatpy` and exposes a simple R6 based client.  Helper classes are provided
for API rate limiting and for navigating the API structure in a stateful
fashion. Only a portion of the original Python API has been translated so far.
Selection helpers are included for working with wildcard expressions.

## Example
```r
library(pxstatspyR)
api <- PxAPI$new("https://api.scb.se/pxweb/api/v2")

# Fetch API configuration
conf <- api$get_config()

# Explore navigation
root <- api$get_navigation_root()

# Retrieve metadata for a table
meta <- api$get_table_metadata("AM0110A1")

# Download data in JSON-stat2 format
dat <- api$get_table_data("AM0110A1")

# Convenient helper returning a data.frame
df <- api$get_data_as_dataframe("AM0110A1")

# Search for tables and return a data.frame
tables <- api$find_tables_as_dataframe(query = "population")

# POST request example
body <- list(query = list(list(code = "Region", selection = list(filter = "item", values = "01"))))
dat2 <- api$get_table_data_post("AM0110A1", body)

# Enumerations
OutputFormat
OutputFormatParam
PxVariables
get_matching_codes(c("TOP(5)", "2020"), all_codes)
```
