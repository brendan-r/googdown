# Delete an md / Rmd file
#' @export
gd_delete <- function(file_id) {
  gd_auth()

  req <- httr::DELETE(
    paste0("https://www.googleapis.com/drive/v3/files/", file_id),
    httr::config(token = getOption("gd.token"))
  )

  httr::content(req)
}

# See the files you have in the drive account
gd_ls <- function() {
  req <- httr::GET(
    paste0("https://www.googleapis.com/drive/v2/files?maxResults=1000"),
    httr::config(token = getOption("gd.token"))
  )

  httr::content(req)
}
