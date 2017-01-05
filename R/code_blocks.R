
knitr_block_to_pandoc_fenced <- function(lines) {
  # Internal function to change a knitr code-block-header to a pandoc one, if
  # detected. Not intended for vecotrized inputs.
  detect_and_change_headers <- function(l) {

    # Is this a knitr code block/chunk header?
    is_block_header <- grepl("```[[:space:]]*\\{r.*\\}[[:space:]]*$", l)

    # If not, return the original input
    if (!is_block_header) return(l)

    # Extract the string containing params, and potentially the name
    params <- l %>% gsub("```[[:space:]]*\\{r,?[[:space:]]+|\\}", "", .) %>%
      strsplit(",") %>% unlist() %>% gsub("[[:space:]]+", "", .)

    # The name is going to be the param which does not feature an equals sign,
    # if there is one. If there's more than one name, throw an informative
    # error.
    name   <- params[!grepl("=", params)]
    params <- params[grepl("=", params)]

    if (length(name) > 1L) {
      stop("Bad knitr chunk in .Rmd file: Appears to have more than one name.")
    }

    # If there is a name, append # to it as per pandoc fenced attributes
    if (length(name) == 1L) {
      name <- paste0("#", name)
    }

    # Re-assemble the parts back into pandoc fenced-code-block attributes
    paste0("```{", name, " .r ", paste(params, collapse = " "), "}")
  }

  lines %>% lapply(detect_and_change_headers) %>% unlist()
}

pandoc_fenced_to_knitr_block <- function(lines) {
  detect_and_change_headers <- function(l) {
    # Is this a pandoc code block/chunk header?
    is_block_header <- grepl("```\\{.*\\.r.*}[[:space:]]*$", l)

    # If not, return the original input
    if (!is_block_header) return(l)

    # Look for a word starting with #
    name <- str_extract(l, "#[[:alnum:]]*") %>% gsub("#", "", .) %>%
      na.omit()

    params <- l %>%
      # Remove everything that isn't a param
      gsub("```\\{|}|#[[:alnum:]]+|\\.r", "", .) %>%
      # Remove spaces around equals signs
      gsub("[[:space:]]*=[[:space:]]*", "=", .) %>%
      # Remove unneccesary white space
      gsub("[[:space:]]+", " ", .) %>%
      # Remove space at the start / end of the line
      gsub("^[[:space:]]|[[:space:]]$", "", .) %>%
      # Split on spaces
      strsplit(" ") %>% unlist

    paste0("```{r ", paste0(c(name, params), collapse = ", "), "}")
  }

  lines %>% lapply(detect_and_change_headers) %>% unlist()
}

