t1 <- tempfile()
t2 <- tempfile()

# A deletion
writeLines(as.character(c(1, 2, 3)), t1)
writeLines(as.character(c(1, 2)), t2)
diff_list(t1, t2)

# No diff
writeLines(as.character(c(1, 2, 3)), t1)
writeLines(as.character(c(1, 2, 3)), t2)
diff_list(t1, t2)

# An addition
writeLines(as.character(c(1, 2, 3)), t1)
writeLines(as.character(c(1, 2, 3, 4)), t2)
diff_list(t1, t2)


# A change
writeLines(as.character(c(1, 2, 3)), t1)
writeLines(as.character(c(1, 2, 4)), t2)
diff_list(t1, t2)