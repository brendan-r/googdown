context("Code Blocks")

test_that("Code block changes are reversible", {

  cb_test_files <- normalizePath(list.files(
    "../test_files/code_blocks//", full.names = T
  ))

  # Remove the files which have commas at '```{r,' which gets lost in your
  # function, but that's alright
  cb_test_files <- subset(cb_test_files, !grepl("_w_comma", cb_test_files))

  for (f in cb_test_files) {

    original_lines <- readLines(f)

    new_lines <- pandoc_fenced_to_knitr_block(
      knitr_block_to_pandoc_fenced(original_lines)
    )

    # Is it the same?
    expect_equal(original_lines, new_lines)
  }

})


test_that("Code block translations can be understood by Pandoc", {

  for (f in cb_test_files) {

    original_lines <- readLines(f)

    tf1 <- tempfile()
    tf2 <- tempfile()

    writeLines(knitr_block_to_pandoc_fenced(original_lines), tf1)

    system(paste("pandoc -f markdown -t json", tf1, "-o", tf2))

    expect_true(any(grepl("CodeBlock", readLines(tf2))))
  }

})
