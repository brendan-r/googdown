
# From rmarkdown ---------------------------------------------------------------

# Yaml funs, swiped from the internals of package `rmarkdown`:
# https://github.com/rstudio/rmarkdown/blob/395d2a39b0cd3b3220ba5ab53bcb47302f9c7cae/R/output_format.R

partition_yaml_front_matter <- function(input_lines) {

  validate_front_matter <- function(delimiters) {
    if (length(delimiters) >= 2 &&
        (delimiters[2] - delimiters[1] > 1) &&
        grepl("^---\\s*$", input_lines[delimiters[1]])) {
      # verify that it's truly front matter (not preceded by other content)
      if (delimiters[1] == 1)
        TRUE
      else
        is_blank(input_lines[1:delimiters[1]-1])
    } else {
      FALSE
    }
  }

  # is there yaml front matter?
  delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input_lines)
  if (validate_front_matter(delimiters)) {

    front_matter <- input_lines[(delimiters[1]):(delimiters[2])]

    input_body <- c()

    if (delimiters[1] > 1)
      input_body <- c(input_body,
                      input_lines[1:delimiters[1]-1])

    if (delimiters[2] < length(input_lines))
      input_body <- c(input_body,
                      input_lines[-(1:delimiters[2])])

    list(front_matter = front_matter,
         body = input_body)
  }
  else {
    list(front_matter = NULL,
         body = input_lines)
  }
}

# https://github.com/rstudio/rmarkdown/blob/ea515efadeb2c4e878f865fb9fb99a9a0091f99c/R/util.R#L230
is_blank <- function (x)
{
  if (length(x))
    all(grepl("^\\s*$", x))
  else TRUE
}

# From knitr -------------------------------------------------------------------
# https://github.com/yihui/knitr/blob/0d648a67275cda5f554a0d724dc2bb4ab0c7dd6f


knit_counter = function(init = 0L) {
  n = init
  function(reset = FALSE) {
    if (reset) return(n <<- init)
    n <<- n + 1L
    n - 1L
  }
}

# quote the chunk label if necessary
quote_label = function(x) {
  x = gsub('^\\s*,?', '', x)
  if (grepl('^\\s*[^\'"](,|\\s*$)', x)) {
    # <<a,b=1>>= ---> <<'a',b=1>>=
    x = gsub('^\\s*([^\'"])(,|\\s*$)', "'\\1'\\2", x)
  } else if (grepl('^\\s*[^\'"](,|[^=]*(,|\\s*$))', x)) {
    # <<abc,b=1>>= ---> <<'abc',b=1>>=
    x = gsub('^\\s*([^\'"][^=]*)(,|\\s*$)', "'\\1'\\2", x)
  }
  x
}

chunk_counter = knit_counter(1L)

# parse params from chunk header
parse_params = function(params) {

  if (params == '') return(list(label = unnamed_chunk()))

  res = withCallingHandlers(
    eval(parse_only(paste('alist(', quote_label(params), ')'))),
    error = function(e) {
      message('(*) NOTE: I saw chunk options "', params,
              '"\n please go to https://yihui.name/knitr/options',
              '\n (it is likely that you forgot to quote "character" options)')
    })

  # good, now you seem to be using valid R code
  idx = which(names(res) == '')  # which option is not named?
  # remove empty options
  for (i in idx) if (identical(res[[i]], alist(,)[[1]])) res[[i]] = NULL
  idx = if (is.null(names(res)) && length(res) == 1L) 1L else which(names(res) == '')
  if ((n <- length(idx)) > 1L || (length(res) > 1L && is.null(names(res))))
    stop('invalid chunk options: ', params,
         "\n(all options must be of the form 'tag=value' except the chunk label)")
  if (is.null(res$label)) {
    if (n == 0L) res$label = unnamed_chunk() else names(res)[idx] = 'label'
  }
  if (!is.character(res$label))
    res$label = gsub(' ', '', as.character(as.expression(res$label)))
  if (identical(res$label, '')) res$label = unnamed_chunk()
  res
}

# autoname for unnamed chunk
unnamed_chunk = function(prefix = NULL, i = chunk_counter()) {
  if (is.null(prefix)) prefix = knitr::opts_knit$get('unnamed.chunk.label')
  paste(prefix, i, sep = '-')
}

# parse but do not keep source
parse_only = function(code) {
  if (length(code) == 0) return(expression())
  parse(text = code, keep.source = FALSE)
}


# From httr --------------------------------------------------------------------

# Taken from https://github.com/hadley/httr/blob/1fc659856602f60ff75eb01903513244e3491ec2/R/oauth-cache.R#L52
add_line <- function(path, line) {
  if (file.exists(path)) {
    lines <- readLines(path, warn = FALSE)
    lines <- lines[lines != ""]
  } else {
    lines <- character()
  }

  if (line %in% lines) return(TRUE)
  catif("Adding ", line, " to ", path)

  lines <- c(lines, line)
  writeLines(lines, path)
  TRUE
}
