# Conversion between types ------------------------------------------------

# For markdown -------------------------------------------
ast_to_md <- function(file, new_file = tempfile(fileext = ".md")) {
  system(paste0(
    "pandoc --wrap=", defaultWrapBehavior(), " ", file,
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
    "pandoc --wrap=", defaultWrapBehavior(), " ", file,
    " -f markdown -t markdown -o ", new_file
  ))

  new_file
}

# Convert an rmd file to ast, and then back again
rmd_to_rmd <- function(file, new_file = tempfile(fileext = ".md")) {
  ast_to_rmd(rmd_to_ast(file), new_file)
}

# For Rmarkdown ---------------------------------------------
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
