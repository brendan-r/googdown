#' @export
diff_list <- function(file1, file2, ignore_trailing_commas = FALSE) {

  # For magirttr / R CMD CHECK
  . <- NULL

  # The number after the letter -- you begin the change on the line below that
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

  if (ignore_trailing_commas) {
    # Diff tempfiles, less trailing commas. Trailing commas can cause you some
    # pain; e.g. if B is added below A, then A acquires a trailing comma, and
    # looks like it's been changed.
    t1 <- tempfile()
    t2 <- tempfile()

    readLines(file1) %>% gsub(",[[:space:]]*$", "", .) %>% writeLines(t1)
    readLines(file2) %>% gsub(",[[:space:]]*$", "", .) %>% writeLines(t2)
  } else {
    t1 <- file1
    t2 <- file2
  }

  # Run a diff on the files. You tried this without specifying useDiff. It ended
  # unexpectedly in misery! You may as well write your own system call to prevent
  # the messages being written out with cat.
  diff_object <- tools::Rdiff(t1, t2, useDiff = TRUE, Log = TRUE)$out

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


# This function allows you to take local and remote markdown files, and create a
# mapping between them so that changes to the remote file can be brought back to
# the markdown source (without also bringing in changes which are peculiar to
# the remote format, such as metadata ending up in the body (e.g. the document
# title, author, etc.), image locations changing, and so on).
#' @export
map_lines <- function(file1, file2, ...) {

  # For magirttr / R CMD CHECK
  . <- NULL

  n_1_lines <- length(readLines(file1))
  n_2_lines <- length(readLines(file2))

  # Init a data.frame with one row per line in file1
  out <- data.frame(file1 = 1:n_1_lines, file2 = NA)

  # Get the line-numbers for the things to find and replace
  line_numbers <- diff_list(file1, file2, ...)

  # These are lines in remote_file which do not appear in local_file
  new_lines <- line_numbers %>%
    lapply(function(x) x$file1_remove) %>% unlist %>% stats::na.omit()

  # These are lines in file2, which have been changed in file1
  changed_lines <- line_numbers %>%
    lapply(function(x) x$file2_add) %>% unlist %>% stats::na.omit()

  # The lines from file2 which we want to map to file1 -- all the lines, less
  # the changed ones
  file2_lines_to_map <- 1:n_2_lines %>% .[!. %in% changed_lines]

  out$file2[!out$file1 %in% new_lines] <- file2_lines_to_map

  out
}
