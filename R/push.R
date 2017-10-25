#' Push an Rmarkdown document to Google Docs
#'
#' @param file_name An Rmarkdown file
#' @return \code{TRUE} (invisibly) if successfull, otherwise, an error.
#' @inheritParams google_doc
#' @export
gd_push <- function(file_name, bookdown_md = TRUE,
                    reference_docx = NULL) {
  rmarkdown::render(
    file_name,
    output_format = google_doc(reference_docx, bookdown_md = bookdown_md)
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

  # Remove the annoying and ugly MS Word 'Bookmark links' (helpfully generated
  # by pandoc, but given far too much visual emphasis by Google)
  catif("Removing .docx bookmarks")
  remove_docx_bookmarks(output_file)

  # Update / add content to the remote file
  catif("Uploading to Google")

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
##' @param reference_docx The reference file to be used for styling. The default
##'   (\code{NULL}) is to use a template with the same styling as a default
##'   Google doc, though with slightly nicer figure captions.
##' @param bookdown_md Should markdown extensions by bookdown by used?
##' @param keep_md Should the intermediate markdown files be retained?
##' @param clean_supporting Should other intermediate files be retained?
##' @return If successful, the URL of a remote Google document
##' @export
google_doc <- function(reference_docx = NULL, bookdown_md = TRUE,
                       keep_md = FALSE, clean_supporting = FALSE) {

  # If no reference doc, use the package default
  if (is.null(reference_docx)) {
    reference_docx <- system.file("custom-reference.docx", package = "googdown")
  } else {
    if (!file.exists(reference_docx)) stop(reference_docx, " does not exist")
  }

  # Change the function you use based whether you'd like to support 'markdown
  # extensions by bookdown'

  if (bookdown_md) {

    google_doc_bookdown(
      reference_docx = reference_docx, keep_md = keep_md,
      clean_supporting = clean_supporting
    )

  } else {

    google_doc_rmarkdown(
      reference_docx = reference_docx, keep_md = keep_md,
      clean_supporting = clean_supporting
    )

  }

}



google_doc_rmarkdown <- function(reference_docx = NULL, keep_md = FALSE,
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



bookdown_process_markdown_wrapper <- function(...) {

  # Try and call bookdown:::process_markdown, capture the error message
  attempt <- try(bookdown:::process_markdown(...))

  # If you encountered an error, grep the contents to see if it's the one about
  # multiple labels, which occurs as a result of having multiple plots per chunk
  if ("try-error" %in% class(attempt)) {
    if (grepl("multiple labels", attempt[1])) {
      # If it is, provide something more informative for the user
      stop("bookdown for googdown does not support multiple plots per chunk!")
    } else {
      # Otherwise, fail with something generic, but try and provide a helpful
      # suggestion
      stop("Error processing with bookdown. Try using bookdown_md = FALSE")
    }
  }

  # If no error was encountered, return the result
  return(attempt)
}



google_doc_bookdown <- function(fig_caption = TRUE, md_extensions = NULL,
                                pandoc_args = NULL, ...) {
  # Adapted from
  # https://github.com/rstudio/bookdown/blob/master/R/word.R
  # @ 7fe1b999ef4bd41671af384ce739812542144df1
  #
  # This relies on unexported features from bookdown, so is probably a bad idea
  # in the long run. Still, it works for now!
  from_rmarkdown <- utils::getFromNamespace('from_rmarkdown', 'rmarkdown')
  from           <- from_rmarkdown(fig_caption, md_extensions)

  config <- google_doc_rmarkdown(...)

  pre <- config$pre_processor

  config$pre_processor <- function(metadata, input_file, ...) {
    # Pandoc does not support numbered sections for Word, so figures/tables have
    # to be numbered globally from 1 to n

    bookdown_process_markdown_wrapper(input_file, from, pandoc_args, TRUE)

    if (is.function(pre)) pre(metadata, input_file, ...)
  }

  post <- config$post_processor

  config$post_processor <- function(metadata, input, output, clean, verbose) {

    if (is.function(post)) {
      output <- post(metadata, input, output, clean, verbose)
    }

    bookdown:::move_output(output)

  }

  config$bookdown_output_format <- 'docx'

  config = bookdown:::set_opts_knit(config)

  config
}
