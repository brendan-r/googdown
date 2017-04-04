context("Remote -> local sim: Simple change")

setwd("../test_files/round_trips/")

test_that("Very simple file works", {

  setwd("1_no_code_sentence_add")

  # Get a list of our markdown test files
  md_files <- list.files(full.names = TRUE) %>%
    normalizePath() %>% subset(grepl(".*md$", .))

  # Convert them all to ast_files
  ast_files <- md_to_ast %>%
    mapply(md_files, gsub("\\.md$|\\.Rmd", ".ast", md_files)) %>%
    unname()

  # Fold those AST files to make them easier to diff
  ast_files %>% mapply(fold_ast_json, ., .)

  remote_diff_to_local(
    remote1 = ast_files %>% .[grepl("remote1\\.ast", .)],
    local1  = ast_files %>% .[grepl("local1\\.ast", .)],
    remote2 = ast_files %>% .[grepl("remote2\\.ast", .)],
    output_file = "output.ast"
  )

  # Did the merged thing end up the way you expect? Note: It's probably best to
  # make a file which has what you expect in it --- it won't always be the same
  # as the remote (re: source code, etc. etc.)
  expect_equal(
    brocks::read_txt(ast_to_md("output.ast")),
    brocks::read_txt(ast_to_md("remote2.ast"))
  )

  # Pop back out to the right dir
  setwd("..")

})


test_that("A more realistic file works", {

  setwd("2_real_round_trip/")

  ast_files <- c("_remote1.ast", "_remote2.ast", "_local.ast")

  # Fold those AST files to make them easier to diff
  ast_files %>% mapply(fold_ast_json, ., .)

  remote_diff_to_local(
    remote1 = "_remote1.ast",
    local1  = "_local.ast",
    remote2 = "_remote2.ast",
    output_file = "output.ast"
  )

  fold_ast_json("output.ast", "output.ast")

  # Note: You want to convert the ast to markdown, and create a hand-edited
  # version which does what you want it to (in other words, this remote thing
  # won't work)
  expect_equal(
    brocks::read_txt(ast_to_md("output.ast")),
    brocks::read_txt(ast_to_md("_remote2.ast"))
  )

  # Pop back out to the right dir
  setwd("..")
})


setwd("../testthat/")
