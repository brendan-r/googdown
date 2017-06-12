init_empty_doc <- function(title) {

  # The mime type for Google docs
  gd_mime <- "application/vnd.google-apps.document"

  # Create the new doc, return the raw response
  req <- httr::POST(
    "https://www.googleapis.com/drive/v2/files",
    httr::config(token = getOption("gd.token")),
    body = list(title = title, mimeType = gd_mime),
    encode = "json"
  )

  # Throw an error if there was one
  httr::stop_for_status(req)
  httr::content(req)
}


#' Authorize with Google Documents
#'
#' @param cache passed to httr
#' @param client_id Unimplemented
#' @param client_secret Unimplemented
#'
#' @return Used for it's side effects.
#' @export
gd_auth <- function(client_id, client_secret, cache = "~/.googdown_token") {

  gd_app <- httr::oauth_app(
    key    = Sys.getenv("GOOGLE_CLIENT_ID"),
    secret = Sys.getenv("GOOGLE_CLIENT_SECRET"),
    "google"
  )

  gd_token <- httr::oauth2.0_token(
    httr::oauth_endpoints("google"),
    gd_app,
    scope = "https://www.googleapis.com/auth/drive.file",
    cache = cache
  )

  # Set the token as an option
  options(
    gd.token       = gd_token,
    gd.token.cache = cache
  )

  # TODO: Test the connection, write the environment vars (you can copy all this
  # from: https://github.com/brendan-r/boxr/blob/master/R/boxr_auth.R)

  return(invisible(TRUE))
}


gd_revisions_ls <- function(doc_id) {
  req <- httr::GET(
    paste0(
      "https://www.googleapis.com/drive/v2/files/", doc_id,
       "/revisions?maxResults=1000"
    ),
    httr::config(token = getOption("gd.token"))
  )

  # Throw an error if there was one
  httr::stop_for_status(req)
  httr::content(req)
}


gd_file_resource <- function(doc_id) {
  req <- httr::GET(
    paste0(
      "https://www.googleapis.com/drive/v2/files/", doc_id
    ),
    httr::config(token = getOption("gd.token"))
  )

  # Throw an error if there was one
  httr::stop_for_status(req)
  httr::content(req)
}


#' Download an A Google Doc as an Open Office or MS Word Document
#'
#' @param file_id The Google Doc ID of the document you'd like to download
#' @param file_name The local file you'd like to export to
#' @param format The export format you'd like to use
#'
#' @return The httr response
#' @export
gd_export <- function(
  file_id, file_name, export_format = getOption("gd.download_format"),
  revision = NA
) {

  # Check that you can work with the format
  if (!export_format %in% c("ms_word_doc", "open_office_doc")) {
    stop("export_format must be 'ms_word_doc' or 'open_office_doc'")
  }

  if (is.na(revision)) {
    req <- httr::GET(
      paste0(
        "https://www.googleapis.com/drive/v2/files/",
        file_id,
        "/export?mimeType=", file_types()[[export_format]]$mime_type
      ),
      httr::config(token = getOption("gd.token")),
      httr::write_disk(file_name, TRUE)
    )
  } else {
    req <- httr::GET(
      paste0(
        "https://docs.google.com/feeds/download/documents/export/Export?id=",
        doc_id, "&revision=", remote_rev, "exportFormat=",
        file_types()[[export_format]]$pandoc_type
      ),
      httr::config(token = getOption("gd.token")),
      httr::write_disk(file_name, TRUE)
    )
  }

  # Throw an error if there was one
  httr::stop_for_status(req)

  return(req)
}

##' Download a Google Document as Markdown or JSON
##'
##' A small wrapper function for \code{\link{gd_export}}, which converts the
##' exported file to markdown or JSON using pandoc
##' @param doc_id The ID of the Google document
##' @param file_name File path to download to
##' @param export_format The format the document is exported from google using
##' @param output_format The format for the final outputted file
##' @return If successful, \code{file_name}
gd_download <- function(doc_id,
                        file_name = tempfile(),
                        export_format = getOption("gd.download_format"),
                        output_format = "markdown",
                        revision = NA) {

  # Check that you can work with the export format
  if (!export_format %in% c("open_office_doc", "ms_word_doc")) {
    stop("export_format must be one of 'open_office_doc' or 'ms_word_doc'")
  }

  # Same for the output format
  if (!output_format %in% c("markdown", "json")) {
    stop("output_format must be one of 'json' or 'markdown'")
  }

  tf <- tempfile()

  # 'Export the file from Google docs'
  gd_export(doc_id, tf, export_format, revision)

  # Convert --------------------------------------------------------------------

  # Convert to markdown
  if (output_format == "markdown") {

    if (export_format == "open_office_doc") {
      return(odt_to_md(tf, file_name))
    }

    if (export_format == "ms_word_doc") {
      return(docx_to_md(tf, file_name))
    }

  # Convert to json / AST
  } else if (output_format == "json") {

    if (export_format == "open_office_doc") {
      return(odt_to_ast(tf, file_name))
    }

    if (export_format == "ms_word_doc") {
      return(docx_to_ast(tf, file_name))
    }
  }

}


file_types <- function() {
  list(
    html            = list(
      file_ext = ".html", pandoc_type = "html", mime_type = "text/html",
      rmarkdown_writer = rmarkdown::html_document_base
    ),
    rich_text       =	list(
      file_ext = ".rtf", pandoc_type = "rtf", mime_type = "application/rtf",
      rmarkdown_writer = rmarkdown::rtf_document
    ),
    open_office_doc =	list(
      file_ext = ".odt", pandoc_type = "odt",
      mime_type = "application/vnd.oasis.opendocument.text",
      rmarkdown_writer = rmarkdown::odt_document
    ),
    ms_word_doc     =	list(
      file_ext = ".docx", pandoc_type = "docx",
      mime_type = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
      rmarkdown_writer = rmarkdown::word_document
    )
  )
}

# Note, pandoc cannot read rtf files
reader_list <- function() {
  c("html", "open_office_doc", "ms_word_doc")
}
