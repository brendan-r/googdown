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

