library(magrittr)


# A function to generate tokens (not *yet* using a custom environment)
gdoc_token <- function(cache = "~/.googdown_token") {
  
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

# Upload an md / Rmd file
gdoc_upload <- function(
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
  
  req
}

# Download an Rmd / md file
gdoc_download <- function(
  file_id, file_name = "./file.md", format = defaultDownloadFormat()
) {
  
  temp_file <- tempfile(fileext = file_types()[[format]]$file_ext)
  
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
