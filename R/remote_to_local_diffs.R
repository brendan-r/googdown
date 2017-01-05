

remote_diff_to_local <- function(remote1, local1, remote2, output_file) {

  map <- map_lines(remote1, local1)

  # Diff the two remote files, extract the lines which have changed
  remote_diff <- diff_list(remote1, remote2)

  # Determine if any of the diffs concern areas of the AST which can't
  # *meaningfully* be propagated back to the local markdown source's AST. All
  # additions *should* (?) be fine
  #
  # For changes / subtractions -- if any positive digit isn't in `map`, remove it

  filtered_diff <- remote_diff %>%
    lapply(function(x) {
      # If there's nothing to remove, then you're fine
      if (any(is.na(x$file1_remove))) return(x)
      # The lines which are flagged as changeable from the mapping between
      # remote1 and local1
      changeable_lines <- map$file1[!is.na(map$file2)]
      # The lines in remote1 which we hope to change
      lines_to_be_changed <- x$file1_remove

      if (all(lines_to_be_changed %in% changeable_lines)) x else NULL
    }) %>%
    Filter(Negate(is.null), .)


  # Alter the diff object, so that the lines from remote1 are changed to their
  # equivalents in local1
  map_ind <- function(x) {
    if (any(is.na(x))) return(NA)
    map$file2[x]
  }

  # The line numbers for the remote1 - remote2 diff, but with remote1's line
  # numbers replaced with the equivalents (where available) from local1
  offset_diff <- filtered_diff %>%
    lapply(function(x){
      x$file1_at     <- map_ind(x$file1_at)
      x$file1_remove <- map_ind(x$file1_remove)
      x
    })

  # Apply the offset diff object (containing the diffs between the two remote
  # files), and use it to apply a patch to the original markdown source
  patch(local1, remote2, offset_diff, output_file)
}
