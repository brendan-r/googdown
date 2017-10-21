.onLoad <- function(libname, pkgname) {
  op <- options()
  op.googdown <- list(
    gd.token           = NULL,
    gd.cache           = "./.googdown",
    gd.verbose         = TRUE,
    gd.download_format = "ms_word_doc",
    gd.upload_format   = "ms_word_doc",
    gd.wrap            = "none",
    gd.new_image_path  = "./assets",
    gd.opts_chunk      = list(
      dpi        = 600,
      fig.height = 4,
      fig.width  = 4
    )
  )
  toset <- !(names(op.googdown) %in% names(op))
  if (any(toset)) options(op.googdown[toset])

  invisible()
}
