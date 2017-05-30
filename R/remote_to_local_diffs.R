# Note: Currently the is.na statements in the below are throwing warnings, but
# the behavior is as desired. Possibly worth wrapping in a suppressWarnings call
#' @export
remote_diff_to_local <- function(remote1, local1, remote2, output_file) {

  map <- map_lines(remote1, local1)

  # Diff the two remote files, extract the lines which have changed
  # Before you do the diff, remove all commas at the end of lines, as these
  # mean that adding an item to an object changes the object above

  remote_diff <- diff_list(remote1, remote2, ignore_trailing_commas = TRUE)

  # Determine if any of the diffs concern areas of the AST which can't
  # *meaningfully* be propagated back to the local markdown source's AST. All
  # additions *should* (?) be fine
  #
  # For changes / subtractions -- if any positive digit isn't in `map`, remove it

  ## filtered_diff <- remote_diff %>%
  ##   lapply(function(x) {
  ##     # If there's nothing to remove, then you're fine
  ##     if (any(is.na(x$file1_remove))) return(x)
  ##     # The lines which are flagged as changeable from the mapping between
  ##     # remote1 and local1
  ##     changeable_lines <- map$file1[!is.na(map$file2)]
  ##     # The lines in remote1 which we hope to change
  ##     lines_to_be_changed <- x$file1_remove

  ##     if (all(lines_to_be_changed %in% changeable_lines)) x else NULL }) %>%
  ## Filter(Negate(is.null), .)

  filtered_diff <- remote_diff

  # Alter the diff object, so that the lines from remote1 are changed to their
  # equivalents in local1
  map_ind <- function(x) {
    if (any(is.na(x))) return(NA)
    map$file2[x]
  }


  map_ind2 <- function(lines_to_change) {

    if (any(is.na(lines_to_change))) return(NA)

    mapped_lines_to_change <- vector()

    # Go through each line in file 1
    for (i in 1:length(lines_to_change)) {
      # The line-number of the line in question, in the first / markdown file
      # (also the row number of map)
      l <- lines_to_change[i]

      if (!is.na(map$file2[l])) {
        # If there's no NA, then it should be a straight mapping
        mapped_lines_to_change <- c(mapped_lines_to_change, map$file2[l])
      } else {

        # Otherwise, interpolate between known editable lines
        # The previous non-NA line
        last_non_na <- max(na.omit(map$file2[1:(l-1)]))     + 1
        # The next non-NA line
        next_non_na <- min(na.omit(map$file2[l:nrow(map)])) - 1

        # Interpolate the lines of source code that would be affected, and add
        # them to the lines to edit
        mapped_lines_to_change <- c(
          mapped_lines_to_change, last_non_na:next_non_na
        )
      }
    }

    mapped_lines_to_change
  }


  # The line numbers for the remote1 - remote2 diff, but with remote1's line
  # numbers replaced with the equivalents (where available) from local1
  offset_diff <- filtered_diff %>%
    lapply(function(x){
      x$file1_at     <- max(map_ind2(x$file1_at))
      x$file1_remove <- map_ind2(x$file1_remove)
      x
    })

  # Apply the offset diff object (containing the diffs between the two remote
  # files), and use it to apply a patch to the original markdown source
  patch(local1, remote2, offset_diff, output_file)
}


# There are two approaches to this. One is that you diff the local and source
# (to get diffs which relate to R code), and then diff these back on to the new
# file. This seems like it could work, but it would, for example, not handle
# changes in code that don't have any visual result in the remote file.
#
# The other approach is to map the source and local files to see what can be
# changed. For the parts of the document that can be changed, propagate the
# changes over. If there are changes which are outside this, don't move them
# over (it would be helpful if you threw a warning).


#' @export
unknit_new_md <- function(original_rmd_ast, original_md_ast, new_md_ast,
                          output_file = "_unknit.ast") {

  # For each line in the original markdown file, which lines have a
  # corresponding line in the Rmarkdown file?
  map <- map_lines(original_md_ast, original_rmd_ast)

  # What are the differences between the original md and rmd?
  md_changes_diff <- diff_list(
    original_md_ast, new_md_ast,  ignore_trailing_commas = TRUE
  )

  # Determine if any of the diffs concern areas of the AST which can't
  # *meaningfully* be propagated back to the local markdown source's AST. All
  # additions *should* (?) be fine
  #
  # For changes / subtractions -- if any positive digit isn't in `map`, remove
  # it

  # You should probably do away with all of this: Deletions will appear as both
  # changes and deletions, and additions aren't problematic.
  ## filtered_diff <- md_changes_diff %>%
  ##   lapply(function(x) {
  ##     # If there's nothing to remove, then you're fine
  ##     if (any(is.na(x$file1_remove))) return(x)
  ##     # The lines which are flagged as changeable from the mapping between
  ##     # remote1 and local1
  ##     changeable_lines <- map$file1[!is.na(map$file2)]
  ##     # The lines in remote1 which we hope to change
  ##     lines_to_be_changed <- x$file1_remove

  ##     if (all(lines_to_be_changed %in% changeable_lines)) x else NULL
  ##   }) %>%
  ##   Filter(Negate(is.null), .)


  ## if (length(filtered_diff) < 1L) {
  ##   stop("After filtering to lines which can be changed, no diffs to make!")
  ## }

  filtered_diff <- md_changes_diff

  ## # Alter the diff object, so that the lines from new_md_ast are changed to their
  ## # equivalents in original_rmd_ast
  ## map_ind <- function(x) {
  ##   if (any(is.na(x))) return(NA)
  ##   map$file2[x]
  ## }

  # A version of the above, which should substitute NAs for equivalent lines in
  # the source code
  map_ind2 <- function(lines_to_change) {

    if (any(is.na(lines_to_change))) return(NA)

    mapped_lines_to_change <- vector()

    # Go through each line in file 1
    for (i in 1:length(lines_to_change)) {
      # The line-number of the line in question, in the first / markdown file
      # (also the row number of map)
      l <- lines_to_change[i]

      if (!is.na(map$file2[l])) {
        # If there's no NA, then it should be a straight mapping
        mapped_lines_to_change <- c(mapped_lines_to_change, map$file2[l])
      } else {

        # Otherwise, interpolate between known editable lines
        # The previous non-NA line
        last_non_na <- max(na.omit(map$file2[1:(l-1)]))     + 1
        # The next non-NA line
        next_non_na <- min(na.omit(map$file2[l:nrow(map)])) - 1

        # Interpolate the lines of source code that would be affected, and add
        # them to the lines to edit
        mapped_lines_to_change <- c(
          mapped_lines_to_change, last_non_na:next_non_na
        )
      }
    }

    mapped_lines_to_change
  }


  # The line numbers for the remote1 - remote2 diff, but with remote1's line
  # numbers replaced with the equivalents (where available) from local1
  offset_diff <- filtered_diff %>%
    lapply(function(x){
      x$file1_at     <- max(map_ind2(x$file1_at))
      x$file1_remove <- map_ind2(x$file1_remove)
      x
    })

  # Apply the offset diff object (containing the diffs between the old and new
  # markdown asts, and apply it to the source rmarkdown ast. Don't convert to
  # markdown at this stage
  patch(original_rmd_ast, new_md_ast, offset_diff, output_file)
}


## unknit_new_md <- function(
##   original_rmd_ast, original_md_ast, new_md_ast, output_file = "_unknit.ast"
## ) {
##   # The only paragraph-level difference between Rmd and md source files should
##   # be R code, so we can narrow our search to just things which look like code

##   # fold the JSON so that each terminaltype/content pair witin the padoc AST
##   # json is on it's own line (makes diffing easier). There's really no use-case
##   # for *not* doing it this way --- you may as well move it in to the main
##   # diffing function
##   rendered_fold <- "_folded_rendered.ast"
##   source_fold   <- "_folded_source.ast"
##   new_fold      <- "_folded_new.ast"

##   fold_ast_json(original_rmd_ast, source_fold)
##   fold_ast_json(original_md_ast, rendered_fold)
##   fold_ast_json(new_md_ast, new_fold)

##   # For each line in the original local file, where are the corresponding lines
##   # in the new file?
##   map <- map_lines(rendered_fold, new_fold)

##   # What are the differences between the original md and rmd?
##   knit_diff <- diff_list(rendered_fold, source_fold)

##   # Filter the diff, to remove anything which overlaps with an NA in the
##   # rendered file --- these diffs represent dynamic code which is no longer
##   # represented in the AST (e.g. it has been removed.)
##   filtered_diff <- knit_diff %>%
##     lapply(function(x) {
##       changeable_lines <- map$remote_file[is.na(map$file1)]
##       if (!all(x$file1_remove %in% changeable_lines)) x else NULL
##     }) %>%
##     Filter(Negate(is.null), .)



##   # Alter the diff object, so that changes intended for the local document can
##   # be mapped to the new remote one
##   map_ind <- function(x) {
##     if (any(is.na(x))) return(NA)
##     map$file2[x]
##   }

##   offset_diff <- filtered_diff %>%
##     lapply(function(x) {
##       x$file1_at     <- map_ind(x$file1_at)
##       x$file1_remove <- map_ind(x$file1_remove)
##       x
##     })

##   # Write the ast to a tempfile, then convert that to (R) markdown
##   temp_file <- tempfile(fileext = ".json")
##   patch(new_fold, source_fold, offset_diff, temp_file)

##   system(paste(
##     "pandoc", temp_file, "-f json -t markdown -o", output_file
##   ))
## }
