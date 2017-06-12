# For ODT / MS Word files ------------------------------------------------------

odt_to_md <- function(file, new_file = tempfile(fileext = ".md")) {

  system(paste0(
    "pandoc --atx-headers --wrap=", defaultWrapBehavior(), " ", file,
    " -f odt -t markdown -o ", new_file
  ))

  new_file

}


docx_to_md <- function(file, new_file = tempfile(fileext = ".md")) {

  system(paste0(
    "pandoc --atx-headers --wrap=", defaultWrapBehavior(), " ", file,
    " -f docx -t markdown -o ", new_file
  ))

  new_file

}


odt_to_ast <- function(file, new_file = tempfile(fileext = ".ast")) {

  # Convert to Markdown to to drop unusable metadata
  temp <- odt_to_md(file)

  # Convert markdown to AST
  md_to_ast(temp, new_file)

  new_file

}


docx_to_ast <- function(file, new_file = tempfile(fileext = ".ast")) {

  # Convert to Markdown to to drop unusable metadata
  temp <- docx_to_md(file)

  # Convert markdown to AST
  md_to_ast(temp, new_file)

  new_file
}



# For markdown -----------------------------------------------------------------

ast_to_md <- function(file, new_file = tempfile(fileext = ".md")) {
  system(paste0(
    "pandoc --atx-headers --wrap=", defaultWrapBehavior(), " ", file,
    " -f json -t markdown -o ", new_file
  ))

  new_file
}

md_to_ast <- function(file, new_file = tempfile(fileext = ".ast")) {

  paste("pandoc", file, "-f markdown -t json") %>%
    system(intern = TRUE) %>%
    jsonlite::prettify() %>%
    writeLines(new_file)

  new_file
}

# Convert a file from markdown, to, er, markdown. Used in the tests to
# 'standardize' the markdown conventions used
md_to_md <- function(file, new_file = tempfile(fileext = ".md")) {
  system(paste0(
    "pandoc --atx-headers --wrap=", defaultWrapBehavior(), " ", file,
    " -f markdown -t markdown -o ", new_file
  ))

  new_file
}

# Convert an rmd file to ast, and then back again
rmd_to_rmd <- function(file, new_file = tempfile(fileext = ".md")) {
  ast_to_rmd(rmd_to_ast(file), new_file)
}



# For Rmarkdown ----------------------------------------------------------------

ast_to_rmd <- function(file, new_file = tempfile(fileext = ".Rmd")) {

  # Take the pandoc markdown output, and change the pandoc fenced code block
  # attributes back into knitr chunk-option headers
  ast_to_md(file) %>%
    readLines() %>%
    pandoc_fenced_to_knitr_block() %>%
    writeLines(new_file)

  new_file
}

# Should you add the 'no_yaml' stage here?
rmd_to_ast <- function(file, new_file = tempfile(fileext = ".ast")) {

  temp_file <- tempfile(fileext = ".Rmd")

  # Turn knitr code chunks/blocks into pandoc fenced blocks with attributes
  readLines(file) %>% knitr_block_to_pandoc_fenced() %>%
    writeLines(temp_file)

  # Convert the Rmarkdown file (now with code block headers pandoc can read)
  # into JSON, prettify it, and write to the output file
  md_to_ast(temp_file, new_file)
}

# This will take file, convert it to json, and then all the way back,
# standardizing the way things are done within it (e.g. headings, chunks, etc.)


# Convert a file to commonmark, using system pandoc
standardize_rmd <- function(file) {
  # Extact the yaml header, in order to reattach it later (not preserved)
  partitioned_doc <- partition_yaml_front_matter(readLines(file))
  old_body_file   <- tempfile(fileext = ".Rmd")
  new_body_file   <- tempfile(fileext = ".Rmd")

  # Write the body out to a tempfile for simplicity
  writeLines(partitioned_doc$body, old_body_file)

  # Run the body through the pandoc process, and back again
  new_body_file <- ast_to_rmd(rmd_to_ast(old_body_file), old_body_file)

  # Add the new, standardised body to the (unchanged YAML front matter)
  partitioned_doc$body <- c("", readLines(new_body_file))

  # Write the whole thing back to the original file
  writeLines(unlist(partitioned_doc), file)

  # Log something for the user
  catif(file, " converted to standard pandoc markdown (with --wrap=",
        defaultWrapBehavior(), ")")
}
