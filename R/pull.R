#' Pull remote changes to a Google Doc to the source Rmarkdown file
#'
#' @param file The path to the Rmarkdown file which you'd like to update
#'
#' @return \code{TRUE} (invisibly) if successfull, otherwise, an error.
#' @export
gd_pull <- function(file_name, format = getOption("gd.upload_format")) {

  # For magirttr / R CMD CHECK
  . <- NULL

  gd_auth()

  # Convert the file to commonmark standard
  standardize_rmd(file_name)

  # Extact the doc's body and YAML front matter
  yaml_vars <- rmarkdown::yaml_front_matter(file_name)
  body      <- partition_yaml_front_matter(readLines(file_name))$body
  doc_id    <- yaml_vars$googdown$doc_id
  doc_title <- yaml_vars$title


  # Have there been any changes? If not, then go home early
  # You might want to come up with a more specific function here; e.g. one that
  # validates it was the last revision which came back from a push operation
  local_rev  <- latest_revision_from_local_metadata(doc_id, update = FALSE)
  remote_rev <- latest_revision_from_local_metadata(doc_id, update = TRUE)

  if (local_rev == remote_rev) {
    catif("No changes pulled: Local and remote documents already in sync")
    return(invisible(TRUE))
  }

  # Get cache files in order ---------------------------------------------------

  # And the previous local and remote versions, for comparison
  remote1_ast_path <- file_path(
    getOption("gd.cache"), doc_id, paste0(local_rev, "-remote.ast")
  )

  local1_ast_path <- file_path(
    getOption("gd.cache"), doc_id, paste0(local_rev, "-local.ast")
  )

  source1_ast_path <- file_path(
    getOption("gd.cache"), doc_id, paste0(local_rev, "-source.ast")
  )

  # Download the new file ------------------------------------------------------

  # If there are differences, pull the remote AST into the cache
  remote2_ast_path <- tempfile(fileext = ".ast")

  gd_download(doc_id, remote2_ast_path, output_format = "json")
  catif("Downloading remote changes")

  # Fold the JSON of the ast files
  c(remote1_ast_path, remote2_ast_path, local1_ast_path, source1_ast_path) %>%
    mapply(fold_ast_json, ., .)

  # Merging changes ------------------------------------------------------------

  md_merged_ast     <- tempfile(fileext = ".ast")
  rmd_merged_ast    <- tempfile(fileext = ".ast")
  rmd_merged_body   <- tempfile(fileext = ".Rmd")
  final_merged_file <- tempfile(fileext = ".Rmd")

  catif("Attempting to merge remote and local markdown files")

  # Attempt merging remote and local markdown files
  markdown_merge_attempt <- try(
    silent = TRUE,
    remote_diff_to_local(
      remote1     = remote1_ast_path,
      local1      = local1_ast_path,
      remote2     = remote2_ast_path,
      output_file = md_merged_ast
    )
  )

  if ("try-error" %in% class(markdown_merge_attempt)) {
    message("DID NOT WORK: LOCAL-REMOTE MARKDOWN MERGE FAILED")
    return(list(
      remote1     = remote1_ast_path,
      local1      = local1_ast_path,
      remote2     = remote2_ast_path,
      output_file = md_merged_ast
    ))
  }

  catif("Remote and local markdown files successfully merged")

  catif("Attempting to fold the AST of the newly merged markdown file")


  fold_markdown_ast_attempt <- try(
    silent = TRUE,
    fold_ast_json(md_merged_ast, md_merged_ast)
  )

  if ("try-error" %in% class(fold_markdown_ast_attempt)) {
    message("DID NOT WORK: MARKDOWN AST FOLDING FAILED")
    return(list(
      remote1     = remote1_ast_path,
      local1      = local1_ast_path,
      remote2     = remote2_ast_path,
      output_file = md_merged_ast
    ))
  }


  catif("Markdown AST successfully folded")

  catif("Attempting to 'unknit' merged local markdown ast")

  # Attempt the unknitting part
  unknit_attempt <- try(
    silent = TRUE,
    unknit_new_md(
      original_rmd_ast = source1_ast_path,
      original_md_ast = local1_ast_path,
      new_md_ast = md_merged_ast,
      output_file = rmd_merged_ast
    )
  )

  if ("try-error" %in% class(unknit_attempt)) {
    message("DID NOT WORK: UNKNITTING FAILED")
    return(list(
      original_rmd_ast = source1_ast_path,
      original_md_ast = local1_ast_path,
      new_md_ast = md_merged_ast,
      output_file = rmd_merged_ast
    ))
  }

  catif("Markdown AST successfully unknit to Rmd AST")
  catif("Attempting to convert the Rmd AST to Rmd")

  # Convert the AST back to Rmarkdown
  ast_to_rmd(rmd_merged_ast, rmd_merged_body)

  catif("Success!")

  # Add the YAML back on
  writeLines(
    c("---", yaml::as.yaml(yaml_vars), "---", readLines(rmd_merged_body)),
    final_merged_file
  )


  # Write out, end -------------------------------------------------------------

  # Copy the new file to the original file path
  file.copy(final_merged_file, file_name, overwrite = TRUE)

  # Versioning  / Caching
  cache_version_files(
    doc_id = doc_id, doc_revision = remote_rev, source = file_name,
    rendered_md = ast_to_md(md_merged_ast), remote_ast = remote2_ast_path
  )

  catif("Remote changes merged in to ", file_name)
}

#' A function for caching information about document state
#'
#' @param doc_id The Google ID of the document
#' @param source The Rmd source file at the time of the run
#' @param rendered_md The rendered md of the Rmd source file at the time of the
#'   run
#' @param cache_dir The dir used for caching
#' @param doc_revision Optional. Google's version number for the remote
#'   doc. Will be determined with a call to
#'   \code{latest_revision_from_local_metadata} if NULL (the default)
#' @param remote_ast Optional. The filepath to the Pandoc AST of the remote
#'   Google doc at doc_version. Will be retrieved from the remote document if
#'   NULL (the default)
#' @return Nothing
#' @keywords internal
cache_version_files <- function(doc_id, source, rendered_md,
                                cache_dir = getOption("gd.cache"),
                                doc_revision = NULL, remote_ast = NULL) {

  cache_file <- function(file, new_name = NULL, version = TRUE) {

    # Create the directory (won't wipe anything if it already exists)
    doc_dir <- file_path(cache_dir, doc_id)
    dir.create(doc_dir, recursive = TRUE, showWarnings = FALSE)

    # gitignore it (if it isn't already gitignored)
    add_line(".gitignore", cache_dir)

    # Get a name for the file, either new_name, or the file's original name
    use_name <- if (is.null(new_name)) basename(file) else basename(new_name)

    # Some files you want versioned, some files you don't
    out_file <- if (version) {
      file_path(doc_dir, paste0(doc_revision, "-", use_name))
    } else {
      file_path(doc_dir, use_name)
    }

    # Prefix the filename with the version number, and add to the cache
    # directory
    file.copy(file, out_file, overwrite = TRUE)
  }


  cache_run_status <- function() {
    run_status_file <- file_path(cache_dir, doc_id, "runs.csv")

    # Read in existing json (or init an empty list if it doesn't)
    if (!file.exists(run_status_file)) {
      csv <- data.frame()
    } else {
      csv <- utils::read.csv(run_status_file, stringsAsFactors = FALSE)
    }

    # openssl has it's own classes, which are tricky to coerce
    source_hash <- openssl::md5(read_txt(source))
    class(source_hash) <- "character"


    run_info <- data.frame(
      operation    = "push",
      doc_id       = doc_id,
      time         = Sys.time(),
      doc_revision = doc_revision,
      source_md5   = source_hash,
      stringsAsFactors = FALSE
    )

    # Append the latest run information, and write out the file
    utils::write.csv(row.names = FALSE, rbind(csv, run_info), run_status_file)

    return(invisible(TRUE))
  }

  # Do the caching -------------------------------------------------------------

  # You probably won't need all of these once the versioning / unknitting
  # process settles down

  # Extract the doc version
  if (is.null(doc_revision)) {
    doc_revision <- latest_revision_from_local_metadata(doc_id, update = TRUE)
  }

  # Save the AST of the remote file
  if (is.null(remote_ast)) {
    remote_ast <- gd_download(doc_id, output_format = "json")
  }

  # Save the Rmd of the original file
  cache_file(source, "source.Rmd")

  # Save the AST of the original file
  cache_file(rmd_to_ast(source), "source.ast")

  # Save the MD of the knitted file
  cache_file(rendered_md, "local.md")

  # Save the AST of the knitted file
  cache_file(md_to_ast(rendered_md), "local.ast")

  # Save the MD of the remote file
  cache_file(ast_to_md(remote_ast), "remote.md")

  # Save the AST of the remote file
  cache_file(remote_ast, "remote.ast")

  # Write out some general information about the run
  cache_run_status()
}


#' @keywords internal
revision_list_from_local_metadata <- function(
  doc_id, cache_dir = getOption("gd.cache"), update = FALSE
  ) {

  # If the dir already exists, nothing will happen
  dir.create(
    file_path(cache_dir, doc_id), recursive = TRUE, showWarnings = FALSE
  )

  revisions_file <- file_path(cache_dir, doc_id, "revisions.json")

  # Download if asked to (or if the file doesn't exist yet)
  if (!file.exists(revisions_file) | update) {
    writeLines(
      jsonlite::toJSON(gd_revisions_ls(doc_id)),
      revisions_file
    )
  }

  jsonlite::fromJSON(read_txt(revisions_file))
}

#' @keywords internal
latest_revision_from_local_metadata <- function(...) {
  max(as.numeric(unlist(revision_list_from_local_metadata(...)$items$id)))
}
