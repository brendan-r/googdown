context("Markdown  <--> AST")

test_that("You can convert some markdown files to AST, and back again", {
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
