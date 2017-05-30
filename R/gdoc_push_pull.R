#' Pull remote changes to a Google Doc to the source Rmarkdown file
#'
#' @param file The path to the Rmarkdown file which you'd like to update
#'
#' @return \code{TRUE} (invisibly) if successfull, otherwise, an error.
#' @export
gd_pull <- function(file_name, format = defaultUploadFormat()) {
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

  fold_ast_json(md_merged_ast, md_merged_ast)

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

# Push / Render ----------------------------------------------------------------
# Notes: This *should probably* end up as an rmarkdown output_format (which is
# what jenny/noam did in gdoc). However, it's easiest to be lazy for the time
# being

#' Push an Rmarkdown document to Google Docs
#'
#' @param file_name The filepath of the Rmarkdown source code
#'
#' @return \code{TRUE} (invisibly) if successfull, otherwise, an error.
#' @export
gdoc_push <- function(file_name, format = defaultUploadFormat()) {
  rmarkdown::render(file_name, output_format = google_doc())
}

##' googdown pre-knit hook
##'
##' It simply checks google auths, and then 'standardizes' the Rmarkdown file,
##' using \code{\link{standardize_rmd}}
##'
##' @param input
##' @return Used for its side effects
##' @keywords internal
pre_knit <- function(input) {

  # Check auth
  gd_auth()

  # Convert the file to 'standard' markdown
  standardize_rmd(input)

}

##' googdown's post processor (run after pandoc)
##'
##' Uploads the file to google docs, adds the doc_id to the YAML front matter,
##' and caches some local files.
##'
##' @param metadata Supplied by \code{rmarkdown::\link{output_format}}
##' @param input_file Supplied by \code{rmarkdown::\link{output_format}}
##' @param output_file Supplied by \code{rmarkdown::\link{output_format}}
##' @param clean Supplied by \code{rmarkdown::\link{output_format}}
##' @param verbose Supplied by \code{rmarkdown::\link{output_format}}
##' @return Used for its side effects
##' @keywords internal
post_processor <- function(metadata, input_file, output_file, clean, verbose) {

  # Munge around to get the original file name
  #
  # This is actually really bad -- because you can't access the original
  # filename from within this function, you're having to assemble it making some
  # assumptions about how knitr handles it. The biggest / worst assumption here
  # is that this is running in the same directory as the original file.
  rendered_md <- input_file
  source_rmd  <- gsub("\\.utf8\\.md$", ".Rmd", input_file)

  # Extact the doc's body and YAML front matter -- much of this can probably be
  # replaced with the metadata object
  yaml_vars <- rmarkdown::yaml_front_matter(source_rmd)
  body      <- partition_yaml_front_matter(readLines(source_rmd))$body
  doc_id    <- yaml_vars$googdown$doc_id
  doc_title <- yaml_vars$title


  # Init -----------------------------------------------------------------------

  # If it looks like the doc hasn't been uploaded before, init a new one and
  # return it's id
  if (is.null(doc_id)) {

    # Init an empty doc
    init_respb <- init_empty_doc(doc_title)

    catif("No doc_id detected in YAML headers: New Google Doc created")

    # Append the ID to the original file's YAML data
    doc_id   <- init_respb$id
    yaml_vars$googdown$doc_id <- paste0(doc_id)
    yaml_vars <- paste0("---\n", yaml::as.yaml(yaml_vars), "---\n")

    writeLines(c(yaml_vars, body), source_rmd)
    catif("New doc_id added to source file's YAML headers")

  } else {
    # If you could blow comments out, warn the user
    if (!doc_update_warning()) return(NULL)
  }


  # Upload & cache -------------------------------------------------------------

  # Update / add content to the remote file
  up_respb <- gd_update(output_file, doc_id)

  catif("Google document content successfully updated")

  # Versioning  / Caching
  cache_version_files(doc_id, source = source_rmd, rendered_md)



  # Return something -----------------------------------------------------------

  # Return a file path to a '.url' file (I had no idea this was a thing;
  # shamelessly stolen from ropensiclabs' godc pakcage)

  url      <- paste0("https://docs.google.com/document/d/", doc_id, "/edit")
  url_file <- paste0(tools::file_path_sans_ext(source_rmd), ".url")

  writeLines(
    paste0("[InternetShortcut]\nURL=", url, "\n"), url_file
  )

  return(url_file)
}




##' Convert a Local Rmarkdown File to a remote Google Doc
##'
##' This function is intended to be used as an \code{rmarkdown} 'output format',
##' e.g. \code{rmarkdown::render("myfile.Rmd", output_format =
##' googdown::google_doc())}. You can also get the same result via
##' \code{\link{gd_push}}.
##'
##' @param reference_odt The reference file to be used for styling. The default
##'   (\code{NULL}) is to use a template with the same styling as a default
##'   Google doc, though with slightly nicer figure captions.
##' @param keep_md Should the intermediate markdown files be retained?
##' @param clean_supporting Should other intermediate files be retained?
##' @return If successful, the URL of a remote Google document
##' @export
google_doc <- function(reference_odt = NULL, keep_md = FALSE,
                       clean_supporting = FALSE) {

  # If no reference doc, use the package default
  if (is.null(reference_odt)) {
    reference_odt <- system.file("custom-reference.odt", package = "googdown")
  } else {
    if (!file_exists(reference_odt)) stop(reference_odt, " does not exist")
  }

  # Return an Rmarkdown output format
  rmarkdown::output_format(
    knitr            = rmarkdown::knitr_options(
      opts_chunk = getOption("gd.opts_chunk")
    ),
    pandoc           = rmarkdown::pandoc_options(to = "odt"),
    keep_md          = FALSE,
    clean_supporting = TRUE,
    post_processor   = post_processor,
    pre_knit         = pre_knit,
    base_format      = rmarkdown::odt_document(reference_odt = reference_odt)
  )

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
#'   \code{\link{latest_revision_from_local_metadata}} if NULL (the default)
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
      csv <- read.csv(run_status_file, stringsAsFactors = FALSE)
    }

    # openssl has it's own classes, which are tricky to coerce
    source_hash <- openssl::md5(brocks::read_txt(source))
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
    write.csv(row.names = FALSE, rbind(csv, run_info), run_status_file)

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
    remote_ast <- download_ast(doc_id)
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

  jsonlite::fromJSON(brocks::read_txt(revisions_file))
}

#' @keywords internal
latest_revision_from_local_metadata <- function(...) {
  max(as.numeric(unlist(revision_list_from_local_metadata(...)$items$id)))
}
