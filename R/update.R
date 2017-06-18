##' Update the contents of a Google Doc, with a local file
##'
##' @param local_file The file which you'd like to upload
##' @param doc_id the google doc id
##' @return Used for its side effects.
gd_update <- function(local_file, doc_id) {
  req <- httr::PATCH(
    paste0("https://www.googleapis.com/upload/drive/v3/files/", doc_id),
    httr::config(token = getOption("gd.token")),
    body = httr::upload_file(local_file)
  )

  httr::stop_for_status(req)
  httr::content(req)
}
