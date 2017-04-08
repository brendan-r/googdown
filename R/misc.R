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
  Sys.setenv(GOOGDOWN_UPDATE_WARN = FALSE)
  add_to_renviron("GOOGDOWN_UPDATE_WARN=FALSE")
}

# Should the user be warned about the document update behaviour?
check_doc_update_warning <- function() {
  warn <- as.logical(Sys.getenv("GOOGDOWN_UPDATE_WARN"))

  if (is.na(warn))   return(TRUE)
  if (warn)          return(TRUE)
  if (!warn)         return(FALSE)
}


#' Add a variable to a user's ~/.Renviron file
#'
#' @param x The text that you'd like to add, e.g. \code{VARIABLE=value}
#'
#' @return \code{TRUE} (in)
#' @export
add_to_renviron <- function(x) {
  add_line(file_path(Sys.getenv("HOME"), ".Renviron"), x)
}

catif <- function(...) {
  if (TRUE) cat(paste0(...))
}
