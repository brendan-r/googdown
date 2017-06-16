##' Remove 'Bookmark References' from a .docx file
##'
##' 'Bookmark References' are MS Word's internal version of header I.D.s, for
##' linking to or navigating documents. Pandoc adds Bookmark References to all
##' .docx output to try and be helpful. Unfortunately, Google Docs renders these
##' with big blue ugly ribbons by the side of each header. This function strips
##' them out of a given input file.
##'
##' MS Word files are zipped XML. The file is unzipped, and a regular expression
##' is used to parse and remove the offending bookmarks from the XML. Then
##' everything is zipped back together again.
##'
##' @param input_file The .docx file you'd like the bookmark references stripped
##'   from
##' @return TRUE (invisibly) if it worked
remove_docx_bookmarks <- function(input_file) {
  # Do everything in a temp dir, perform operations on a tempfile
  working_dir <- file.path(tempdir(), digest::digest(stats::runif(1)))

  dir.create(working_dir)

  # Unzip it
  raw_doc_dir <- utils::unzip(input_file, exdir = working_dir)

  # The file we want to operate on
  document_xml <- file.path(working_dir, "word/document.xml")

  # A regex for bookmarks
  bookmark_regex <- '<w:bookmarkStart(.+?)<w:bookmarkEnd w:id="[0-9]+"[[:space:]]*/>'

  # Read in the document, regex remove all of the bookmarks
  readLines(document_xml) %>%
    stringr::str_replace_all(bookmark_regex, "") %>%
    writeLines(document_xml)

  # Zip it back up. Zip can only take relative paths (or it puts the whole path
  # in there) so temporarily change working directory, zip, then change back

  # Note: You could perform some validation on the file using pandoc here,
  # though it would slow things down
  wd <- getwd()
  setwd(working_dir)
 utils::zip(input_file, list.files(working_dir))
  setwd(wd)

  # Out
  return(invisible(TRUE))
}
