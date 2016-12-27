
# Upload an md / Rmd file
gdoc_update <- function(
  file_name, file_id, format = defaultUploadFormat()
) {
  
  library(rmarkdown)
  
  local_file  <- rmarkdown::render(file_name, get(format)())
  
  req <- httr::PATCH(
    paste0("https://www.googleapis.com/upload/drive/v3/files/", file_id),
    httr::config(token = getOption("gd.token")),
    body = httr::upload_file(local_file)
  )
  
  req
}

