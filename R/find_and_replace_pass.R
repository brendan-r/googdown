
extract_knitr_generated_output <- function(source_rmd, local_md) {

  # Read in the text of the source file
  source_lines <- readLines(source_rmd)
  output_lines <- readLines(local_md)

  # Diff the two files
  diffs <- diff_list(source_rmd, local_md)

  # Filter the diffs just down to those which appear to have R code chunks
  chunks_with_output <- diffs %>%
    lapply(
      function(x) {
        # Do any of the lines begin with something that looks like an R chunk?
        any(grepl(
          "```[[:space:]]*\\{r.*\\}[[:space:]]*$",
          source_lines[x$file1_remove]
        )) &
          # And, are they also changes, e.g. something from the source code has
          # been removed, and replaced with something new in the output?
          # (i.e. Has the R chunk generated code? Not all do.)
          x$type == "c"
      }
    ) %>%
    unlist()


  # Extract the input and output lines for each of the chunks. Squash them down
  # into strings.
  diffs[chunks_with_output] %>%
    lapply(
      function(x) {
        list(
          source_lines = lines_to_string(source_lines[x$file1_remove]),
          output_lines = lines_to_string(output_lines[x$file2_add])
        )
      }
    ) %>%
    # Remove any diffs where the result is ""
    Filter(function(x) x$output_lines != "", .)

}


# Should you write out find and replace changes to a file somewhere? Or at least
# log that something happened?
final_find_and_replace_pass <- function(
  merged_rmd_file,
  local_md,
  source_rmd,
  output_file
  ) {

  # Extract a list of the differences between rmarkdown and markdown
  # (e.g. differences created during knitting) from the files used to compile
  # the document last pushed to google
  knit_diffs <- extract_knitr_generated_output(source_rmd, local_md)

  # Read in the lines of the merged rmarkdown file; best efforts to merge via
  # diffing should have already taken place
  merged_rmd_lines <- read_txt(merged_rmd_file)

  # Set this variable to FALSE here. It will get turned to TRUE if any changes
  # are actually made
  any_changes_made <- FALSE

  for (i in 1:length(knit_diffs)) {

    # How many times is the pattern in the diff matched?
    n_matches <- stringr::str_count(
      merged_rmd_lines, stringr::fixed(knit_diffs[[i]]$output_lines)
    )

    # If it isn't matched (this should happen most of the time), move to the
    # next iteration of the loop
    if (n_matches == 0L) next

    # If it's matched more than once, this is unusual. Issue a warning but
    # proceed
    if(n_matches > 1L)
      warning("Final find and replace pass: More than one line of output matches
               an R code chunk. Review merged Rmd file carefully.")

    # Search for the string and replace it
    merged_rmd_lines <- stringr::str_replace(
      merged_rmd_lines,
      stringr::fixed(knit_diffs[[i]]$output_lines),
      knit_diffs[[i]]$source_lines
    )

    any_changes_made <- TRUE
  }

  # Write out the file where the find and replace has happened
  writeLines(merged_rmd_lines, output_file)

  # Return a bool to indicate whether the find-and-replace pass actually caught
  # anything
  return(any_changes_made)
}

# Convenience function to squash strings read in as a vector one-per-line, to a
# single string with line breaks separated as \n
lines_to_string <- function(lines) {
  paste0(lines, collapse = "\n")
}
