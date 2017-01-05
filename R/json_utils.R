# A tricky function which 'prettifies' JSON in a very specific ways, to make it
# more amenable to line-by-line diffing.
#
# 'para' breaks each paragraph into one line, where as 'word' finds the lowest
# level of content/type pairs in the pandoc ast
fold_ast_json <- function(file_in, file_out, level = c("para", "word")) {
  # Read in the original pandoc json AST into R as a list
  json <- readLines(file_in) %>% from_json

  if (!level %in% c("para", "word")) {
    stop("invalid value of level")
  }

  # Iterate through the list, and turn parts of it *back into* JSON
  # 'para' will do this by just going two layers deep
  # 'word' will do this by finding the outermost 'childless' c/t pairs
  if (level == "para") {
    json <- lapply(json, function(x) lapply(x, to_json))
  } else if (level == "word") {
    json <- wrap_ct(json)
  }

  # Write the file out: A JSON text file, containg long strings, which just
  # happen to also be JSON

  temp_file1 <- tempfile(fileext = ".json")
  temp_file2 <- tempfile(fileext = ".json")

  json %>% to_json(pretty = TRUE) %>%
    writeLines(temp_file1)

  # Pulling out the table cloth...
  lines <- readLines(temp_file1)

  # cat(paste(lines, collapse = "\n"))

  # Different approach: Any line where the first non-whitespace character is a
  # quotation mark AND it isnt' "unMeta" "c": or "t":, remove that quotation
  # mark

  # Remove any " that begins a line
  lines <- gsub('^[[:space:]]*\\"', '', lines)

  # Remove any that sits between } (or },) and the end of the line
  lines <- gsub('\\}\\",$', '},', lines)
  lines <- gsub('\\}\\"$', '}', lines)

  # Replace the "s for "c": etc.
  lines <- gsub('^[[:space:]]*c\\":', '"c":', lines)
  lines <- gsub('^[[:space:]]*t\\":', '"t":', lines)
  lines <- gsub('^[[:space:]]*unMeta\\":', '"unMeta":', lines)

  # Remove double quotes either side of square brackets
  lines <- gsub('\\"\\[', '[', lines)
  lines <- gsub('\\]\\"', ']', lines)

  # And, empty objects are having trouble "{} -> {}
  lines <- gsub('\\"\\{\\}', '{}', lines)

  # And lose all the confusing escapted quotes
  lines <- gsub('\\\\"', '"', lines)

  # cat(paste(lines, collapse = "\n"))
  jsonlite::validate(lines)

  # Check that it's valid JSON
  stopifnot(jsonlite::validate(lines))

  # Write out the results
  writeLines(lines, file_out)

  return(invisible(TRUE))
}



# Taken from https://stackoverflow.com/a/13433689
# This needs to be re-written, it throws warnings all over the shop
depth <- function(this, thisdepth = 0) {
  if (!is.list(this)) {
    return(thisdepth)
  } else {
    return(max(unlist(lapply(this, depth, thisdepth = thisdepth + 1))))
  }
}

# returns bool
one_ct_pair <- function(x) {
  if ("unMeta" %in% names(x))
    return(TRUE)

  if (depth(x) < 2)
    return(TRUE)

  # If you unlist, child nodes get the parent node's name appended with a dot
  # inbetween. This is basically "if this node contains a child 't' node"
  (!any(grepl("\\.t$", names(unlist(x))))) &
    (sum(names(unlist(x)) == "t") == 1L)
}

# A function to wrap certain list depths to one line of JSON text. n = 2
# provides the right level of dpth to get c: t: pairings for words.
wrap <- function(x, n = 2) {
  if (depth(x) < n) {
    if (length(x) < 2) return(x) else return(to_json(x))
  } else {
    lapply(x, wrap, n)
  }
}

# This goes to the loest level of ct pairs
wrap_ct <- function(x) {
  if (one_ct_pair(x)) {
    if (depth(x) < 1) return(x) else return(to_json(x))
  } else {
    lapply(x, wrap_ct)
  }
}


from_json <- function(x, ...) {
  jsonlite::fromJSON(
    x, simplifyDataFrame = FALSE, simplifyVector = FALSE, flatten = TRUE, ...
  )
}

to_json <- function(x, ...) {
  jsonlite::toJSON(x, auto_unbox = TRUE, ...)
}

