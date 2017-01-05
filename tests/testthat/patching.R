# Some tests
t1 <- tempfile()
t2 <- tempfile()
t3 <- tempfile()
out <- tempfile()


# Can do a basic deletion
writeLines(as.character(1:5), t1)
writeLines(as.character(1:4), t2)

stopifnot(all(patch_strings(t1, t2, diff_list(t1, t2)) == readLines(t2)))

writeLines(as.character(c("b", 1, 2, 3, 4, 5)), t1)
writeLines(as.character(c("b", 0, 1, "a", 3, 4)), t2)

stopifnot(all(patch_strings(t1, t2, diff_list(t1, t2)) == readLines(t2)))

writeLines(as.character(c(1:5, 1, 2, 3, 4, 5)), t1)
writeLines(as.character(c(1:5, 0, 1, "a", 3, 4)), t2)

stopifnot(all(patch_strings(t1, t2, diff_list(t1, t2)) == readLines(t2)))

# Warning, if there's a diff on the very first line of file1, then it breaks
writeLines(as.character(c(1, 2, 3, 4, 5)), t1)
writeLines(as.character(c(0, 1, "a", 3, 4)), t2)

stopifnot(all(patch_strings(t1, t2, diff_list(t1, t2)) == readLines(t2)))
