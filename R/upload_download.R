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
    scope = "https://www.googleapis.com/auth/drive.file"
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


#' Upload a file to Google Docs
#'
#' @param file_name The name of the local file which you'd like to upload
#' @param format The conversion format you'd like to use to upload it
#'
#' @return The httr response
#' @export
gd_upload <- function(
  file_name, format = defaultUploadFormat()
) {

  library(rmarkdown)

  temp_dir <- tempdir()

  local_file  <- rmarkdown::render(
    file_name, file_types()[[format]]$rmarkdown_writer(), clean = TRUE,
    output_dir = temp_dir
  )

  req <- httr::POST(
    "https://www.googleapis.com/upload/drive/v2/files?convert=true",
    httr::config(token = getOption("gd.token")),
    body = httr::upload_file(local_file)
  )

  # Throw an error if there was one
  httr::stop_for_status(req)
  httr::content(req)
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


#' Download an A Google Doc as Markdown
#'
#' @param file_id The Google Doc ID of the document you'd like to download
#' @param file_name The local file you'd like the Markdown data stored in
#' @param format The conversion format you'd like to use
#'
#' @return The httr response
#' @export
gd_download <- function(
  file_id, file_name, format = defaultDownloadFormat(),
  revision = NA, output_format = "commonmark"
) {

  temp_file <- tempfile()

  if (is.na(revision)) {
    req <- httr::GET(
      paste0(
        "https://www.googleapis.com/drive/v2/files/",
        file_id,
        "/export?mimeType=", file_types()[[format]]$mime_type
      ),
      httr::config(token = getOption("gd.token")),
      httr::write_disk(temp_file, TRUE)
    )
  } else {
    req <- httr::GET(
      paste0(
        "https://docs.google.com/feeds/download/documents/export/Export?id=",
        doc_id, "&revision=", remote_rev, "exportFormat=",
        file_types()[[format]]$pandoc_type
      ),
      httr::config(token = getOption("gd.token")),
      httr::write_disk(temp_file, TRUE)
    )
  }

  # Throw an error if there was one
  httr::stop_for_status(req)

  system(paste0(
    "pandoc ", temp_file, " -f ", file_types()[[format]]$pandoc_type,
    " -t ", output_format, " -o ", file_name
  ))

  return(invisible(TRUE))
}


  req <- httr::GET(
    paste0(
      "https://www.googleapis.com/drive/v2/files/",
      file_id,
      "/export?mimeType=", file_types()[[format]]$mime_type
    ),
    httr::config(token = getOption("gd.token")),
    httr::write_disk(temp_file, TRUE)
  )

  system(
    paste0("pandoc ", temp_file, " -f ", file_types()[[format]]$pandoc_type,
           " -t markdown"),
    intern = TRUE
  ) %>% writeLines(file_name)

  req
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
