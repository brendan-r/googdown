

remote_diff_to_local <- function(remote1, local1, remote2, output_file) {

  map <- map_lines(remote1, local1)

  # Diff the two remote files, extract the lines which have changed
  remote_diff <- diff_list(remote1, remote2)

  # Determine if any of the diffs concern areas of the AST which can't
  # *meaningfully* be propagated back to the local markdown source's AST. All
  # additions *should* (?) be fine
  #
  # For changes / subtractions -- if any positive digit isn't in `map`, remove it

  filtered_diff <- remote_diff %>%
    lapply(function(x) {
      # If there's nothing to remove, then you're fine
      if (any(is.na(x$file1_remove))) return(x)
      # The lines which are flagged as changeable from the mapping between
      # remote1 and local1
      changeable_lines <- map$file1[!is.na(map$file2)]
      # The lines in remote1 which we hope to change
      lines_to_be_changed <- x$file1_remove

      if (all(lines_to_be_changed %in% changeable_lines)) x else NULL
    }) %>%
    Filter(Negate(is.null), .)


  # Alter the diff object, so that the lines from remote1 are changed to their
  # equivalents in local1
  map_ind <- function(x) {
    if (any(is.na(x))) return(NA)
    map$file2[x]
  }

  # The line numbers for the remote1 - remote2 diff, but with remote1's line
  # numbers replaced with the equivalents (where available) from local1
  offset_diff <- filtered_diff %>%
    lapply(function(x){
      x$file1_at     <- map_ind(x$file1_at)
      x$file1_remove <- map_ind(x$file1_remove)
      x
    })

  # Apply the offset diff object (containing the diffs between the two remote
  # files), and use it to apply a patch to the original markdown source
  patch(local1, remote2, offset_diff, output_file)

}

unknit_new_md <- function(
  original_rmd_ast, original_md_ast, new_md_ast, output_file = "_unknit.ast"
) {
  # The only paragraph-level difference between Rmd and md source files should
  # be R code, so we can narrow our search to just things which look like code

  # fold the JSON so that each terminaltype/content pair witin the padoc AST
  # json is on it's own line (makes diffing easier). There's really no use-case
  # for *not* doing it this way --- you may as well move it in to the main
  # diffing function
  rendered_fold <- "_folded_rendered.ast"
  source_fold   <- "_folded_source.ast"
  new_fold      <- "_folded_new.ast"

  fold_ast_json(original_rmd_ast, source_fold,  "word")
  fold_ast_json(original_md_ast, rendered_fold, "word")
  fold_ast_json(new_md_ast, new_fold, "word")

  # For each line in the original local file, where are the corresponding lines
  # in the new file?
  map <- map_lines(rendered_fold, new_fold)


  knit_diff <- diff_list(rendered_fold, source_fold)

  # Filter the diff, to remove anything which overlaps with an NA in the
  # rendered file --- these diffs represent dynamic code which is no longer
  # represented in the AST (e.g. it has been removed.)
  filtered_diff <- knit_diff %>%
    lapply(function(x) {
      changeable_lines <- map$remote_file[is.na(map$file1)]
      if (!all(x$file1_remove %in% changeable_lines)) x else NULL
    }) %>%
    Filter(Negate(is.null), .)



  # Alter the diff object, so that changes intended for the local document can
  # be mapped to the new remote one
  map_ind <- function(x) {
    if (any(is.na(x))) return(NA)
    map$file2[x]
  }

  offset_diff <- filtered_diff %>%
    lapply(function(x) {
      x$file1_at     <- map_ind(x$file1_at)
      x$file1_remove <- map_ind(x$file1_remove)
      x
    })

  # Write the ast to a tempfile, then convert that to (R) markdown
  temp_file <- tempfile(fileext = ".json")
  patch(new_fold, source_fold, offset_diff, temp_file)

  system(paste(
    "pandoc", temp_file, "-f json -t markdown -o", output_file
  ))
}
