# For ODT / MS Word files ------------------------------------------------------

# Small function to return a commonly used pandoc option list
#' @keywords internal
pandoc_options <- function() {
  c("--atx-headers", paste0("--wrap=", getOption("gd.wrap")))
}


odt_to_md <- function(input_file, output_file = tempfile(fileext = ".md")) {

  rmarkdown::pandoc_convert(
    input   = input_file,
    output  = output_file,
    from    = "odt",
    to      = "markdown",
    options = pandoc_options()
  )

  output_file

}


docx_to_md <- function(input_file, output_file = tempfile(fileext = ".md")) {


  rmarkdown::pandoc_convert(
    input   = input_file,
    output  = output_file,
    from    = "docx",
    to      = "markdown",
    options = pandoc_options()
  )

  output_file

}


odt_to_ast <- function(input_file, output_file = tempfile(fileext = ".ast")) {

  # Convert to Markdown to to drop unusable metadata
  temp <- odt_to_md(input_file)

  # Convert markdown to AST
  md_to_ast(temp, output_file)

  output_file

}


docx_to_ast <- function(input_file, output_file = tempfile(fileext = ".ast")) {

  # Convert to Markdown to to drop unusable metadata
  temp <- docx_to_md(input_file)

  # Convert markdown to AST
  md_to_ast(temp, output_file)

  output_file

}



# For markdown -----------------------------------------------------------------

ast_to_md <- function(input_file, output_file = tempfile(fileext = ".md")) {

  rmarkdown::pandoc_convert(
    input   = input_file,
    output  = output_file,
    from    = "json",
    to      = "markdown",
    options = pandoc_options()
  )

  output_file

}


md_to_ast <- function(input_file, output_file = tempfile(fileext = ".ast")) {


  rmarkdown::pandoc_convert(
    input   = input_file,
    output  = output_file,
    from    = "markdown",
    to      = "json",
    options = pandoc_options()
  )

  output_file %>%
    read_txt() %>%
    jsonlite::prettify() %>%
    writeLines(output_file)

  output_file
}


# Convert a file from markdown, to, er, markdown. Used in the tests to
# 'standardize' the markdown conventions used
md_to_md <- function(input_file, output_file = tempfile(fileext = ".md")) {

  rmarkdown::pandoc_convert(
    input   = input_file,
    output  = output_file,
    from    = "markdown",
    to      = "markdown",
    options = pandoc_options()
  )

  output_file

}


# Convert an rmd file to ast, and then back again
rmd_to_rmd <- function(input_file, output_file = tempfile(fileext = ".md")) {
  ast_to_rmd(rmd_to_ast(input_file), output_file)
}



# For Rmarkdown ----------------------------------------------------------------

ast_to_rmd <- function(input_file, output_file = tempfile(fileext = ".Rmd")) {

  # Take the pandoc markdown output, and change the pandoc fenced code block
  # attributes back into knitr chunk-option headers
  ast_to_md(input_file) %>%
    readLines() %>%
    pandoc_fenced_to_knitr_block() %>%
    writeLines(output_file)

  output_file
}


# Should you add the 'no_yaml' stage here?
rmd_to_ast <- function(input_file, output_file = tempfile(fileext = ".ast")) {

  temp_file <- tempfile(fileext = ".Rmd")

  # Turn knitr code chunks/blocks into pandoc fenced blocks with attributes
  readLines(input_file) %>%
    knitr_block_to_pandoc_fenced() %>%
    writeLines(temp_file)

  # Convert the Rmarkdown file (now with code block headers pandoc can read)
  # into JSON, prettify it, and write to the output file
  md_to_ast(temp_file, output_file)
}



# Misc -------------------------------------------------------------------------

ast_to_ast <- function(input_file, output_file = tempfile(fileext = ".ast")) {

  rmarkdown::pandoc_convert(
    input   = input_file,
    output  = output_file,
    from    = "json",
    to      = "json",
    options = pandoc_options()
  )

  output_file

}

# Convert a file to commonmark, using system pandoc
standardize_rmd <- function(input_file) {
  # Extact the yaml header, in order to reattach it later (not preserved)
  partitioned_doc <- partition_yaml_front_matter(readLines(input_file))
  old_body_file   <- tempfile(fileext = ".Rmd")
  new_body_file   <- tempfile(fileext = ".Rmd")

  # Write the body out to a tempfile for simplicity
  writeLines(partitioned_doc$body, old_body_file)

  # Run the body through the pandoc process, and back again
  new_body_file <- ast_to_rmd(rmd_to_ast(old_body_file), old_body_file)

  # Add the new, standardised body to the (unchanged YAML front matter)
  partitioned_doc$body <- c("", readLines(new_body_file))

  # Write the whole thing back to the original file
  writeLines(unlist(partitioned_doc), input_file)

  # Log something for the user
  catif(input_file, " converted to standard pandoc markdown")
}
