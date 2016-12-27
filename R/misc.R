
# Check if the YAML front-matter in a document contains valid fields (only)
validateYaml <- function(file) {
  
}


doc_update_warning <- function() {
  
  # If the user has expressed that they don't want to see all this, proceed
  if (!check_doc_update_warning()) {
    return(TRUE)
  }
  
  cat(paste0(
    "WARNING: Updating a document blows away any changes not expressed in \n",
    "  your Rmarkdown. This means losing any:\n\n",
    "    - Open comments\n",
    "    - Pending suggestions\n",
    "    - Formatting changes via UI (e.g. colour of text)\n\n",
    "Please make a selection:"
  ))
  
  response <- menu(c(
    "I understand, proceed with document update",
    "Do not proceed",
    "I understand, proceed, and stop showing me this message"
  ))
  
  # Prevent the warning if they don't want to see it anymore
  if (response == 3L) prevent_doc_update_warning()
  
  # Return TRUE if they made a positive selection, FALSE otherwise
  c(TRUE, FALSE, TRUE)[response]
}

prevent_doc_update_warning <- function() {
  add_to_renviron("GOOGDOWN_UPDATE_WARN=FALSE")  
}

# Should the user be warned about the document update behaviour?
check_doc_update_warning <- function() {
  warn <- Sys.getenv("GOOGDOWN_UPDATE_WARN")
  
  if (is.null(warn)) return(TRUE)
  if (warn)          return(TRUE)
  if (!warn)         return(FALSE)
}


#' Add a variable to a user's ~/.Renviron file
#'
#' @param x The text that you'd like to add, e.g. \code{VARIABLE=value}
#'
#' @return
#' @export
add_to_renviron <- function(x) {
  if (length(x) != 1L | is.character(x))
    stop("Must be a character vector of length 1")
  
  env_path <- normalizePath(
    paste0(Sys.getenv("HOME"), "/.Renviron"), mustWork = FALSE
  )
  
  if (file.exists(env_path)) {
    renv <- readLines(env_path)
  } else {
    renv <- NULL
  }
  
  # The name of the variable which you're setting
  var_name <- strsplit(x, "=")[[1L]][1L]
  
  # Remove references where previously set, write new details to the end of the
  # file
  writeLines(
    c(renv[!grepl(paste0(var_name, "="), renv)], paste0(x)),
    con = env_path
  )
  
  return(invisible(TRUE))
}


# Convert a file to commonmark, using system pandoc
convert_to_commonmark <- function(file) {
  # Extact the yaml header, in order to reattach it later (not preserved)
  partitioned_doc <- partition_yaml_front_matter(readLines(file))
  old_body_file   <- tempfile(fileext = ".Rmd")
  new_body_file   <- tempfile(fileext = ".Rmd")
  
  # Write the body out to a tempfile for simplicity
  writeLines(partitioned_doc$body, old_body_file)
  
  command <- paste0(
    "pandoc ", old_body_file,
    " --from markdown --to commonmark --wrap=", defaultWrapBehavior(),
    " --output=", new_body_file
  )
  
  system(command)
  
  # Add the new, standardised body to the (unchanged YAML front matter)
  partitioned_doc$body <- c("", readLines(new_body_file))
  
  # Write the whole thing back to the original file
  writeLines(unlist(partitioned_doc), file)
  
  # Log something for the user
  catif(file, " converted to the commonmark standard (with --wrap=",
        defaultWrapBehavior(), ")")
}

catif <- function(...) {
  if (TRUE) cat(paste0(...))
}
