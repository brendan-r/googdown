diff_list <- function(file1, file2) {
  # The number after the letter -- you begin the chnage on the line below that
  # Note: This is vectorized, but it doesn't really need to be
  normal_diff_to_lines <- function(diff_text) {

    # A function to strip letters from numbers, and also to turn e.g. 1,3 into
    # c(1,2,3)
    to_int_vecs <- function(x) {
      gsub("[[:alpha:]]", "", x) %>%
        strsplit(",") %>% lapply(as.numeric) %>%
        lapply(function(x) if (length(x) == 2) seq(x[1], x[2]) else x)
    }

    extract_ints <- function(expr) {
      to_int_vecs(stringr::str_extract(diff_text, expr))
    }

    # Assemble the output object with regex
    out <- c(
      file1_at = min(unlist(extract_ints("^[0-9,]+[ac]|d[0-9,]+$"))),
      file1_remove = extract_ints("^[0-9,]+c|^.*d"),
      file2_add = extract_ints("c[0-9,]+$|a[0-9,]+$"),
      type = stringr::str_extract(diff_text, "[[:alpha:]]")
    )

    # For some reason, the at number is the line number affected, except for
    # additions, where it's the line *above* where the line should be added.
    # This messes with how you'd like to write the patch function, so you're
    # adding one to it here.
    if (out$type == "a") {
      out$file1_at <- out$file1_at + 1
    }

    out
  }

  # Run a diff on the files. You tried this without specifying useDiff. It ended
  # unexpectedly in misery! You may as well write your own system call to prevent
  # the messages being written out with cat.
  diff_object <- tools::Rdiff(file1, file2, useDiff = TRUE, Log = TRUE)$out

  diff_object %>%
    # Sometimes the diff function can bork itself and return the text of the
    # diffs on the same line as the diff lines affected. Re-structure it
    paste0(collapse = "\n") %>% strsplit("\n") %>% unlist() %>%
    # Just extract the neccessary information (returns a vector as long as the
    # number of diffs detected)
    subset(grepl("^[[:digit:]]", .)) %>%
    # From all those diff descriptions (e.g. "255a231,263"), turn them into
    # which lines to remove, and which lines to add
    lapply(normal_diff_to_lines)
}



