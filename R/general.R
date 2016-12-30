#' Construct Path to File
#'
#' A modified version of \code{\link{file.path}}, which prevents \code{fsep}
#' cropping up more than once.
#'
#' @param ... character vectors
#' @param fsep the path separator to use.
#'
#' @return A character vector
#' @export
file_path <- function(..., fsep = .Platform$file.sep) {
  # Do the file.path thing
  p <- file.path(..., fsep = fsep)

  # Remove any double separators
  gsub(paste0(fsep, "+"), fsep, p)
}

