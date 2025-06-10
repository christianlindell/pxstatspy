# pxstatspyR

This package provides a minimal R interface to the PxAPI used by Statistics Sweden.
It mirrors a subset of the features available in the Python package `pxstatpy`.

## Example
```r
library(pxstatspyR)
api <- PxAPI$new("https://api.scb.se/pxweb/api/v2")
conf <- api$get_config()
```
