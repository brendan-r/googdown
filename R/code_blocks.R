#' @export
knitr_block_to_pandoc_fenced <- function(lines) {
  # Fail out if the special characters which you're using to sub for quotes are
  # in the string
  if (any(grepl("§", lines))) {
    stop("Can't used the ascii character § in knitr blocks: Currently ",
         "used internally by knitr_block_to_pandoc_fenced")
  }

  # Internal function to change a knitr code-block-header to a pandoc one, if
  # detected. Not intended for vecotrized inputs.
  detect_and_change_headers <- function(l) {

    # Is this a knitr code block/chunk header?
    is_block_header <- grepl("```[[:space:]]*\\{r.*\\}[[:space:]]*$", l)

    # If not, return the original input
    if (!is_block_header) return(l)

    param_list <- l %>%
      gsub("```[[:space:]]*\\{r,?[[:space:]]*|\\}", "", .) %>%
      knitr:::parse_params()

    # Extract & remove the chunk name, if there is one
    name <- param_list$label
    param_list$label <- NULL

    # For the remaining params, if they're strings, double quote them
    param_list <- param_list %>% lapply(
      function(x) if(!is.character(x)) x else paste0('"§', x, '§"')
    )

    # Put the param list back into a vector of arg=val strings (if there are
    # any)
    if (length(param_list) > 0L) {
      params <- paste0(names(param_list), "=", unlist(param_list))
    } else {
      params <- NULL
    }

    # If there is a name, append # to it as per pandoc fenced attributes
    if (length(name) == 1L & !grepl("unnamed-chunk-[0-9]+$", name)) {
      name <- paste0("#", name)
    } else {
      name <- NULL
    }

    # Re-assemble the parts back into pandoc fenced-code-block attributes
    paste0("``` {", name, " .r ", paste(params, collapse = " "), "}")
  }

  lines %>% lapply(detect_and_change_headers) %>% unlist()
}




#' @export
pandoc_fenced_to_knitr_block <- function(lines) {
  # Pandoc folds down any line breaks in the code to the lolstring
  # "\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\n". This function replaces that with \r\n,
  # which should for some intents and purposes evaluate to \n
  fix_line_breaks <- function(l) {
    if (l == "") {
      return("")
    } else {
     unlist(strsplit(l, "(\\\\)+n"))
    }
  }

  # This function converts pandoc fenced code-block style headers, to knitr
  # style ones
  detect_and_change_headers <- function(l) {
    # Is this a pandoc code block/chunk header?
    is_block_header <- grepl("```[[:space:]]*\\{.*\\.r.*}[[:space:]]*$", l)

    # If not, return the original input
    if (!is_block_header) return(l)

    # Look for a word starting with #
    name <- stringr::str_extract(l, "#[[:alnum:][:punct:]]*") %>%
      gsub("#", "", .) %>%
    na.omit()

    # A regular expression to replace all spaces in a string which are not
    # between quotes (http://stackoverflow.com/a/9584469)
    space_reg <- "\\s+(?=((\\\\[\\\\\"]|[^\\\\\"])*\"(\\\\[\\\\\"]|[^\\\\\"])*\")*(\\\\[\\\\\"]|[^\\\\\"])*$)"

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
      stringr::str_split(space_reg) %>% unlist

    # Because to pandoc all fenced code-block attributes are strings (and
    # double/escaped freak it out) double quotes are replaced with the ascii
    # symbols § in `knitr_block_to_pandoc_fenced`. Here, remove all existing
    # double quotes, and replace § with new double quotes
    params       <- gsub('"', '',  params)
    params       <- gsub('§', '"', params)


    param_string <- paste0(c(name, params), collapse = ", ")

    # If the param string is non-empty, prefix it with a space. This is so that
    # you either get ```{r params} or ```{r} (no space before closing bracket)
    if (nchar(param_string) > 0) {
      param_string <- paste0(" ", param_string)
    }

    paste0("```{r", param_string, "}")
  }

  lines %>%
    lapply(function(x) fix_line_breaks(detect_and_change_headers(x))) %>%
    unlist()
}
