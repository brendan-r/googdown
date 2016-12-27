#' Taken from https://github.com/hadley/httr/blob/1fc659856602f60ff75eb01903513244e3491ec2/R/oauth-cache.R#L52
#' @keywords internal
add_line <- function(path, line) {
  if (file.exists(path)) {
    lines <- readLines(path, warn = FALSE)
    lines <- lines[lines != ""]
  } else {
    lines <- character()
  }

  if (line %in% lines) return(TRUE)
  catif("Adding ", line, " to ", path)

  lines <- c(lines, line)
  writeLines(lines, path)
  TRUE
}
