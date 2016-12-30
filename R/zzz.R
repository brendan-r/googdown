.onLoad <- function(libname, pkgname) {
  op <- options()
  op.googdown <- list(
    gd.token           = NULL,
    gd.cache           = "./.googdown",
    gd.verbose         = TRUE,
    gd.download_format = "ms_word_doc",
    gd.upload_format   = "open_office_doc"
  )
  toset <- !(names(op.googdown) %in% names(op))
  if (any(toset)) options(op.googdown[toset])

  invisible()
}
