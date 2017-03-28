#' @export
knitr_block_to_pandoc_fenced <- function(lines) {
  # Fail out if the special characters which you're using to sub for quotes are
  # in the string
  if (any(grepl("¶|§", lines))) {
    stop("Can't used the ascii characters ¶ or § in knitr blocks: Currently ",
         "used internally by knitr_block_to_pandoc_fenced")
  }

  # Internal function to change a knitr code-block-header to a pandoc one, if
  # detected. Not intended for vecotrized inputs.
  detect_and_change_headers <- function(l) {

    # Is this a knitr code block/chunk header?
    is_block_header <- grepl("```[[:space:]]*\\{r.*\\}[[:space:]]*$", l)

    # If not, return the original input
    if (!is_block_header) return(l)

    # Extract the string containing params, and potentially the name
    params <- l %>% gsub("```[[:space:]]*\\{r,?[[:space:]]*|\\}", "", .) %>%
      strsplit(",") %>% unlist() %>% gsub("[[:space:]]+", "", .)

    # The name is going to be the param which does not feature an equals sign,
    # if there is one. If there's more than one name, throw an informative
    # error.
    name   <- params[!grepl("=", params)]
    params <- params[ grepl("=", params)]

    # All parameters in a pandoc AST code block are strings. Parameters in knitr
    # code blocks are often numbers (e.g. fig.height) or R varibles. Double
    # quoting doesn't work. Here using the ascii ¶ symbol to sub for single
    # quotes, § for double
    params <- gsub("'", "¶", params)
    params <- gsub('"', "§", params)


    if (length(name) > 1L) {
      stop("Bad knitr chunk in .Rmd file: Appears to have more than one name.")
    }

    # If there is a name, append # to it as per pandoc fenced attributes
    if (length(name) == 1L) {
      name <- paste0("#", name)
    }

    # Re-assemble the parts back into pandoc fenced-code-block attributes
    paste0("``` {", name, " .r ", paste(params, collapse = " "), "}")
  }

  lines %>% lapply(detect_and_change_headers) %>% unlist()
}

#' @export
pandoc_fenced_to_knitr_block <- function(lines) {
  detect_and_change_headers <- function(l) {
    # Is this a pandoc code block/chunk header?
    is_block_header <- grepl("```[[:space:]]*\\{.*\\.r.*}[[:space:]]*$", l)

    # If not, return the original input
    if (!is_block_header) return(l)

    # Look for a word starting with #
    name <- stringr::str_extract(l, "#[[:alnum:][:punct:]]*") %>%
      gsub("#", "", .) %>%
      na.omit()

    params <- l %>%
      # Remove everything that isn't a param
      gsub("```[[:space:]]*\\{|}|#[[:alnum:][:punct:]]+|\\.r", "", .) %>%
      # Remove spaces around equals signs
      gsub("[[:space:]]*=[[:space:]]*", "=", .) %>%
      # Remove unneccesary white space
      gsub("[[:space:]]+", " ", .) %>%
      # Remove space at the start / end of the line
      gsub("^[[:space:]]|[[:space:]]$", "", .) %>%
      # Split on spaces
      strsplit(" ") %>% unlist

    # Because pandoc doesn't accept strings as fenced code-block arguments, we
    # replace single/double quotes with the ascii symbols ¶/§ in
    # `knitr_block_to_pandoc_fenced`. Sub them back now. out now
    params       <- gsub('"', '',  params)
    params       <- gsub('¶', "'", params)
    params       <- gsub('§', '"', params)

    param_string <- paste0(c(name, params), collapse = ", ")

    # If the param string is non-empty, prefix it with a space. This is so that
    # you either get ```{r params} or ```{r} (no space before closing bracket)
    if (nchar(param_string) > 0) {
      param_string <- paste0(" ", param_string)
    }

    paste0("```{r", param_string, "}")
  }

  lines %>% lapply(detect_and_change_headers) %>% unlist()
}
