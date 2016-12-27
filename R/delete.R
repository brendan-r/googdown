# Delete an md / Rmd file
gdoc_delete <- function(file_id, token) {
  
  req <- httr::DELETE(
    paste0("https://www.googleapis.com/drive/v3/files/", file_id),
    httr::config(token = token)
  )
  
  httr::content(req)
}

# See the files you have in the drive account
gdoc_ls <- function(token) {
  req <- httr::GET(
    paste0("https://www.googleapis.com/drive/v2/files?maxResults=1000"),
    httr::config(token = token)
  )
  
  httr::content(req)
}
