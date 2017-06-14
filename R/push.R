# Push / Render ----------------------------------------------------------------
# Notes: This *should probably* end up as an rmarkdown output_format (which is
# what jenny/noam did in gdoc). However, it's easiest to be lazy for the time
# being

#' Push an Rmarkdown document to Google Docs
#'
#' @param file_name The file path of the Rmarkdown source code
#'
#' @return \code{TRUE} (invisibly) if successfull, otherwise, an error.
#' @export
gd_push <- function(file_name, upload_format = getOption("gd.upload_format"),
                      reference_file = NULL) {
  rmarkdown::render(
    file_name,
    output_format = google_doc(reference_file, upload_format)
  )
}


##' googdown pre-knit hook
##'
##' It simply checks google auths, and then 'standardizes' the Rmarkdown file,
##' using \code{\link{standardize_rmd}}
##'
##' @param input The file path of the Rmarkdown source code
##' @return Used for its side effects
##' @keywords internal
pre_knit <- function(input) {

  # Check auth
  gd_auth()

  # Convert the file to 'standard' markdown
  standardize_rmd(input)

}

##' googdown's post processor (run after pandoc)
##'
##' Uploads the file to google docs, adds the doc_id to the YAML front matter,
##' and caches some local files.
##'
##' @param metadata Supplied by \code{rmarkdown::\link{output_format}}
##' @param input_file Supplied by \code{rmarkdown::\link{output_format}}
##' @param output_file Supplied by \code{rmarkdown::\link{output_format}}
##' @param clean Supplied by \code{rmarkdown::\link{output_format}}
##' @param verbose Supplied by \code{rmarkdown::\link{output_format}}
##' @return Used for its side effects
##' @keywords internal
post_processor <- function(metadata, input_file, output_file, clean, verbose) {

  # Munge around to get the original file name
  #
  # This is actually really bad -- because you can't access the original
  # filename from within this function, you're having to assemble it making some
  # assumptions about how knitr handles it. The biggest / worst assumption here
  # is that this is running in the same directory as the original file.
  rendered_md <- input_file
  source_rmd  <- gsub("\\.utf8\\.md$", ".Rmd", input_file)

  # Extact the doc's body and YAML front matter -- much of this can probably be
  # replaced with the metadata object
  yaml_vars <- rmarkdown::yaml_front_matter(source_rmd)
  body      <- partition_yaml_front_matter(readLines(source_rmd))$body
  doc_id    <- yaml_vars$googdown$doc_id
  doc_title <- yaml_vars$title


  # Init -----------------------------------------------------------------------

  # If it looks like the doc hasn't been uploaded before, init a new one and
  # return it's id
  if (is.null(doc_id)) {

    # Init an empty doc
    init_respb <- init_empty_doc(doc_title)

    catif("No doc_id detected in YAML headers: New Google Doc created")

    # Append the ID to the original file's YAML data
    doc_id   <- init_respb$id
    yaml_vars$googdown$doc_id <- paste0(doc_id)
    yaml_vars <- paste0("---\n", yaml::as.yaml(yaml_vars), "---\n")

    writeLines(c(yaml_vars, body), source_rmd)
    catif("New doc_id added to source file's YAML headers")

  } else {
    # If you could blow comments out, warn the user
    if (!doc_update_warning()) return(NULL)
  }


  # Upload & cache -------------------------------------------------------------

  # Update / add content to the remote file
  up_respb <- gd_update(output_file, doc_id)

  catif("Google document content successfully updated")

  # Versioning  / Caching
  cache_version_files(doc_id, source = source_rmd, rendered_md)



  # Return something -----------------------------------------------------------

  # Return a file path to a '.url' file (I had no idea this was a thing;
  # shamelessly stolen from ropensiclabs' godc pakcage)

  url      <- paste0("https://docs.google.com/document/d/", doc_id, "/edit")
  url_file <- paste0(tools::file_path_sans_ext(source_rmd), ".url")

  writeLines(
    paste0("[InternetShortcut]\nURL=", url, "\n"), url_file
  )

  return(url_file)
}


##' Convert a Local Rmarkdown File to a remote Google Doc
##'
##' This function is intended to be used as an \code{rmarkdown} 'output format',
##' e.g. \code{rmarkdown::render("myfile.Rmd", output_format =
##' googdown::google_doc())}. You can also get the same result via
##' \code{\link{gd_push}}.
##'
##' @param reference_file The reference file to be used for styling. The default
##'   (\code{NULL}) is to use a template with the same styling as a default
##'   Google doc, though with slightly nicer figure captions.
##' @param keep_md Should the intermediate markdown files be retained?
##' @param clean_supporting Should other intermediate files be retained?
##' @return If successful, the URL of a remote Google document
##' @export
google_doc <- function(reference_file = NULL,
                       upload_format = getOption("gd.upload_format"),
                       keep_md = FALSE,
                       clean_supporting = FALSE) {

  # Check that you can work with the format
  if (!upload_format %in% c("ms_word_doc", "open_office_doc")) {
    stop("upload_format must be 'ms_word_doc' or 'open_office_doc'")
  }

  # Do the thing
  if (upload_format == "open_office_doc") {
    google_doc_odt(reference_file, keep_md, clean_supporting)
  } else if (upload_format == "ms_word_doc") {
    google_doc_docx(reference_file, keep_md, clean_supporting)
  }

}


#' @keywords internal
google_doc_odt <- function(reference_odt = NULL, keep_md = FALSE,
                       clean_supporting = FALSE) {

  # If no reference doc, use the package default
  if (is.null(reference_odt)) {
    reference_odt <- system.file("custom-reference.odt", package = "googdown")
  } else {
    if (!file.exists(reference_odt)) stop(reference_odt, " does not exist")
  }

  # Return an Rmarkdown output format
  rmarkdown::output_format(
    knitr            = rmarkdown::knitr_options(
      opts_chunk = getOption("gd.opts_chunk")
    ),
    pandoc           = rmarkdown::pandoc_options(to = "odt"),
    keep_md          = FALSE,
    clean_supporting = TRUE,
    post_processor   = post_processor,
    pre_knit         = pre_knit,
    base_format      = rmarkdown::odt_document(reference_odt = reference_odt)
  )

}


#' @keywords internal
google_doc_docx <- function(reference_docx = NULL, keep_md = FALSE,
                            clean_supporting = FALSE) {

  # If no reference doc, use the package default
  if (is.null(reference_docx)) {
    reference_docx <- system.file("custom-reference.docx", package = "googdown")
  } else {
    if (!file.exists(reference_docx)) stop(reference_docx, " does not exist")
  }

  # Return an Rmarkdown output format
  rmarkdown::output_format(
    knitr            = rmarkdown::knitr_options(
      opts_chunk = getOption("gd.opts_chunk")
    ),
    pandoc           = rmarkdown::pandoc_options(to = "docx"),
    keep_md          = FALSE,
    clean_supporting = TRUE,
    post_processor   = post_processor,
    pre_knit         = pre_knit,
    base_format      = rmarkdown::word_document(reference_docx = reference_docx)
  )

}
