
# Upload an md / Rmd file
#' @export
gd_update <- function(local_file, file_id) {
  req <- httr::PATCH(
    paste0("https://www.googleapis.com/upload/drive/v3/files/", file_id),
    httr::config(token = getOption("gd.token")),
    body = httr::upload_file(local_file)
  )

  httr::stop_for_status(req)
  httr::content(req)
}

