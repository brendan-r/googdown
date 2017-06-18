##' Delete a file in a Google Drive account
##'
##' @param file_id The id of the file / document to delete
##' @return The content of the httr response. Used for its side effects.
##' @author br
gd_delete <- function(file_id) {
  gd_auth()

  req <- httr::DELETE(
    paste0("https://www.googleapis.com/drive/v3/files/", file_id),
    httr::config(token = getOption("gd.token"))
  )

  httr::content(req)
}

##' Get a list of the files in a g Google Drive account
##'
##' @return The content of the httr response
gd_ls <- function() {
  req <- httr::GET(
    paste0("https://www.googleapis.com/drive/v2/files?maxResults=1000"),
    httr::config(token = getOption("gd.token"))
  )

  httr::content(req)
}
