#' @export
patch <- function(file1, file2, difflist, patched_file) {

  patch_strings(file1, file2, difflist) %>%
    # Mush back into a single string to fix (this is so that you can detect
    # paren-clashes that may span multiple lines)
    paste(collapse = "\n") %>%
    # If you've broken the JSON at some point (commas!), see if you can fix it
    # with some very basic rules of thumb
    fix_json() %>%
    writeLines(patched_file)

  # Run it through pandoc once more, just to tidy up the AST
  system(paste(
    "pandoc", patched_file, "-f json -t json -o", patched_file
  ))
}

#' @export
patch_strings <- function(file1, file2, difflist) {
  l1 <- readLines(file1)
  l2 <- readLines(file2)

  # Helper functions ---------------------------------
  delete_lines <- function(x, indicies) {
    x[-indicies]
  }

  add_lines <- function(x, lines, at) {
    line_list <- split(x, 1:length(x) >= at)
    c(line_list[[1]], lines, line_list[[2]])
  }

  change_lines <- function(x, indicies, lines) {
    x %>% delete_lines(indicies) %>%
      add_lines(lines, at = min(indicies))
  }

  ast_chunk_start <- c(
    1, difflist %>% lapply(function(x) x$file1_at) %>% unlist
  )

  ast_chunk_end   <- c(
    difflist %>% lapply(function(x) x$file1_at) %>% unlist - 1, length(l1)
  )

  # Iterate ------------------------------------------

  # Init the list for the parts to join up
  parts <- list()
  # Add the first chunk (the source before any diffs happen)
  parts[[1]] <- l1[ast_chunk_start[1]:ast_chunk_end[1]]

  for (l in 1:length(difflist)) {
    d <- difflist[[l]]

    if (d$type == "c") {
      part <- change_lines(l1, d$file1_remove, l2[d$file2_add])
    } else if (d$type == "d") {
      part <- delete_lines(l1, d$file1_remove)
    } else if (d$type == "a") {
      part <- add_lines(l1, l2[d$file2_add], d$file1_at)
    }

    # Figure out the overall number of lines that you've changed the length by
    n_lines_diff <- length(part) - length(l1)

    # Chop the part down, to just the lines affected by the diff
    index_start <- ast_chunk_start[l + 1]
    index_end   <- max(ast_chunk_end[l + 1] + n_lines_diff)

    # If you are trying to merge negative lines, then just return nothing
    if (index_end < index_start) {
      parts[[l + 1]] <- as.character()
    } else {
      parts[[l + 1]] <- part[index_start:index_end]
    }
  }

  # Back to strings
  unlist(parts)
}
