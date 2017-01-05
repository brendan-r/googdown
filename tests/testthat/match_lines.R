# Some tests
t1 <- tempfile()
t2 <- tempfile()

# file 1 is longer
writeLines(as.character(c(1, 2, 3)), t1)
writeLines(as.character(c(1, 2)), t2)
map_lines(t1, t2)

# file 2 is longer
writeLines(as.character(c(1, 2)), t1)
writeLines(as.character(c(1, 2, 3)), t2)
map_lines(t1, t2)

# A line is different
writeLines(as.character(c(1, 2, 3)), t1)
writeLines(as.character(c(1, 4, 3)), t2)
map_lines(t1, t2)
