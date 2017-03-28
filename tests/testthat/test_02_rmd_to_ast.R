context("RMarkdown <--> AST")

test_that("Rmarkdown (no code) <--> AST", {
  md_test_files <- normalizePath(list.files(
    "../test_files/markdown_files/", full.names = T
  ))

  for (f in md_test_files) {
    # Make sure the file is in 'standard markdown' (however that's being
    # defined)
    md_to_md(f, f)

    # Convert the file to AST
    ast_file <- md_to_ast(f)

    # Convet it back to markdown
    md_file <- ast_to_md(ast_file)

    # Is it the same?
    expect_equal(brocks::read_txt(f), brocks::read_txt(md_file))
  }
})


test_that("Rmarkdown (with code) <--> AST", {
  rmd_test_files <- normalizePath(list.files(
    "../test_files/rmarkdown_files/", full.names = T
  ))

  for (f in rmd_test_files) {
    # Convert the file to AST
    ast_file <- rmd_to_ast(f)

    # Convet it back to markdown
    rmd_file <- ast_to_rmd(ast_file)

    # Is it the same?
    expect_equal(brocks::read_txt(f), brocks::read_txt(rmd_file))
  }
})
