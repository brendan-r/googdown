# A function to *try* and fix some common problems in json files caused by your
# messing with them and the whole ignoring commas business
fix_json <- function(string) {
  # A general rule (at least in the JSON that you're generating) might be
  # something like:
  #
  # - Where you see a closed paren followed by an open paren, whack a comma in
  # - between
  #
  # - Where you see a closed paren followed by another closed paren, remove any
  # - commas

  original_string <- string

  # If you have any curly or square braces back to back without a comma, fix it
  string <- gsub("\\}[[:space:]]*\\{", "},{", string)
  string <- gsub("\\][[:space:]]*\\[", "],[", string)
  string <- gsub("\\][[:space:]]*\\{", "],{", string)
  string <- gsub("\\}[[:space:]]*\\[", "},[", string)

  # If you have a closed (square, curly) paren followed by another, with a comma
  # in-between, remove it
  string <- gsub("\\}[[:space:]]*,[[:space:]]*\\}", "}}", string)
  string <- gsub("\\][[:space:]]*,[[:space:]]*\\]", "]]", string)
  string <- gsub("\\][[:space:]]*,[[:space:]]*\\}", "]}", string)
  string <- gsub("\\}[[:space:]]*,[[:space:]]*\\]", "}]", string)

  if (any(original_string != string)) {
    catif("Un-breaking the JSON caused during diffing\n")
  }

  string
}


# This function detects if a JSON node is an inline math block. For some reason
# these seem to break everything, so should be treated as a single ct pair for
# the purposes of code folding
is_inline_math <- function(x) {
  types <- unlist(x)[grepl("t", names(unlist(x)))]
  all(types %in% c("Math", "InlineMath"))
}

one_ct_pair <- function(x) {
  if ("unMeta" %in% names(x))
    return(TRUE)

  # Inline math contains more than one ct pair, but we want to fold it anyway
  if (is_inline_math(x))
    return(TRUE)

  if (depth(x) < 2)
    return(TRUE)

  if (is.null(names(x)))
    return(FALSE)

  # If you unlist, child nodes get the parent node's name appended with a dot
  # inbetween. This is basically "if this node contains a child 't' node"
  entry_names <- names(unlist(x))

  all(!grepl("\\.", entry_names)) &
    (sum(grepl("t", entry_names)) == 1L) &
    length(x) == 2L &
    names(x)[1] == "t" &
    names(x)[2] == "c"
}

# This goes to the lowest level of ct pairs
wrap_ct <- function(x) {
  if (one_ct_pair(x)) {
    if (depth(x) < 1) return(x) else return(to_json(x))
  } else {
    lapply(x, wrap_ct)
  }
}

# A function to determine if a JSON entry is a pandoc para containing text of
# some sort (e.g., we want to split it into lines)
selectively_split <- function(json) {

  is_text_para <- function(x) {
    if (!"t" %in% names(x)) {
      return(FALSE)
    }

    if (x$t == "Para") {
      # Extract the types contained within the node
      child_types <- unique(unlist(x)[grepl("t$", names(unlist(x)))])
      if ("Str" %in% child_types) return(TRUE) else return(FALSE)
    }

    return(FALSE)
  }


  split_or_wrap <- function(x) {
    if (is_text_para(x)) {
      list(t = x$t, c = wrap_ct(x$c))
    } else {
      to_json(x)
    }
  }

  lapply(json, split_or_wrap)
}

# A tricky function which 'prettifies' JSON in a very specific ways, to make it
# more amenable to line-by-line diffing.
#
# 'para' breaks each paragraph into one line, where as 'word' finds the lowest
# level of content/type pairs in the Pandoc ast
#' @keywords internal
fold_ast_json <- function(file_in, file_out) {
  # Read in the original pandoc json AST into R as a list
  json <- readLines(file_in) %>% from_json

  # Get the metadata into one line
  json[[1]] <- to_json(json[[1]])

  json[[2]] <- selectively_split(json[[2]])

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
  # If
  lines <- gsub('\\\\\\"', '"', lines)

  if (!jsonlite::validate(lines)) {
    stop("fold_json_ast: JSON validation lost")
  }

  # Put spaces following words on the same line -------------------------------

  lines <- paste(lines, collapse = "\n")

  # lines <- gsub('\n[[:space:]]*\\{\"t\":\"Space"', '{"t":"Space"', lines)

  lines <- gsub(
    '\\{\"t\":\"Space",\"c\":\\[\\]\\}\n', '{"t":"Space","c":[]}',
    lines
  )

  lines <- gsub(
    '\\{\"t\":\"Space",\"c\":\\[\\]\\},\n', '{"t":"Space","c":[]},',
    lines
  )

  if (!jsonlite::validate(lines)) {
    stop("JSON validation lost after putting spaces on the same line as words")
  }

  # Write out the results
  writeLines(lines, file_out)

  return(invisible(TRUE))
}



# ------------------------------------------------------------------------------

# Taken from https://stackoverflow.com/a/13433689
# This needs to be re-written, it throws warnings all over the shop
depth <- function(this, thisdepth = 0) {
  if (length(this) == 0L) {
    return(-Inf)
  } else if (!is.list(this)) {
    return(thisdepth)
  } else {
    return(max(unlist(lapply(this, depth, thisdepth = thisdepth + 1))))
  }
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

from_json <- function(x, ...) {
  jsonlite::fromJSON(
    x, simplifyDataFrame = FALSE, simplifyVector = FALSE, flatten = TRUE, ...
  )
}

to_json <- function(x, ...) {
  jsonlite::toJSON(x, auto_unbox = TRUE, ...)
}
