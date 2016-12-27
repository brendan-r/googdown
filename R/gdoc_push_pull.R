# # TODO
# 
# - Try to get an end-to-end process going. Disregarding the JSON approach makes this easier, as there's no diffing for uploads, only downloads.
# 
# The steps:
# 
# - Document is written in .Rmd
# - The AST of the remote doc (at that version) is downloaded for diffing
# - A change is made to the google doc
# - The AST of the new doc is downloaded
# - The old and new ASTs are diffed
# - The diff is somehow passed to the original md
# - The diff is somehow passed to the original Rmd
# 
# For a first-step, enforce common-mark standards on the input Rmd.
# 
# The idea of diffing the remote ASTs is to preserve formatting which may not survive a round-trip, for example, formatting in-line or block code statements.

# TODO: Make all this run off the yaml

gd_pull <- function(file) {
  
  # If there have been changes (as per the Drive API, then investigate)
  
  # If the document AST is unchanged from the last pull, then do nothing
  
  # Try a simple find and replace for 'unknitting'
}


# Note: You need to init the document first, to set the title (I think)

gd_push <- function(file_name) {
  # Convert the file to commonmark standard
  convert_to_commonmark(file_name)

  # Upload ---------------------------------------------------------------------
  
  # Extact the doc's body and YAML front matter
  yaml_vars <- rmarkdown::yaml_front_matter(file_name)
  body      <- partition_yaml_front_matter(readLines(file_name))$body

  # If it looks like the doc hasn't been uploaded before, upload it
  if (is.null(yaml_vars$googdown$doc_id)) {
    
    # Upload the file as new (ideally you'd ensure that the file is saved at
    # this point...)
    up_resp <- gd_upload(file)
    httr::stop_for_status(up_resp, "Failed to upload file to Google")
    catif("Google document successfully created")
    
    # Append the ID to the original file's YAML
    yaml_vars$googdown$doc_id <- resp$id
    yaml_vars <- paste0("---\n", yaml::as.yaml(yaml_vars), "---\n")
    writeLines(c(yaml_vars, body), file_name)
    
  } else {
    
    # Update the remote file
    doc_update_warning()
    
    up_resp <- gd_update(file_name = file, file_id = yaml_vars$googdown$id)
    httr::stop_for_status(up_resp, "Failed to upload file to Google")
    catif("Google document successfully updated")
    
  }
  
  # Versioning -----------------------------------------------------------------
  
  # Create a dir for versioning
  dir.create(".googdown/", showWarnings = FALSE)
  # gitignore it
  add_line(".googdown", ".gitignore")
  
  # Use file_id/iteration-version for the dirs
  
  # Create a local copy of the remote file's AST
  remote_ast <- download_ast(file_id, file_name = "")
  catif("Local copy of remote AST stored")
}



ast_download <- function(
  file_id, file_name = "ast.json", format = defaultDownloadFormat()
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
           " -t json"), 
    intern = TRUE
  ) %>% writeLines(file_name)
  
  req
}
