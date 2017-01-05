from_json <- function(x, ...) {
  jsonlite::fromJSON(
    x, simplifyDataFrame = FALSE, simplifyVector = FALSE, flatten = TRUE, ...
  )
}

to_json <- function(x, ...) {
  jsonlite::toJSON(x, auto_unbox = TRUE, ...)
}

