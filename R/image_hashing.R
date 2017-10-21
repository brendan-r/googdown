# Some functions to handle replacing 'image targets' (file references) in the
# pandoc AST, with hashes of the image file's content.
#
# Specifically, to allow the following actions:
#
# - Derive an AST of a downloaded remote docx, where all of the image targets
#   have been replaced by hashes of the files (remote_docx_to_imagehashed_ast)
#
# - Derive an AST of a remote markdown (e.g post-knit) document, where all of
#   the image targets have been replaced with the hash of the equivalent *remote
#   file* (imagehash_local_ast_with_equivalent_remote_ast)
#
# Some notes on hashing of the images:
#
# - Hashes of images are not the same in remote and local docs
#
# - Hashes of images *are* the same on different runs of the same document, or
#   after remote modifications, re-ordering, etc.


# A function to accept a downloaded remote docx file, and output its pandoc AST,
# with the image names replaced by the md5 hashes of their contents
remote_docx_to_imagehashed_ast <- function(
  input_file,
  output_file                  = tempfile(fileext = ".ast"),
  export_new_images            = FALSE,
  image_export_comparison_file = NULL,
  new_image_export_dir         = getOption("gd.new_image_path")
  ) {

  # Extract AST, and image files to a temp dir ---------------------------------

  # Create a temporary directory to extract images to
  temp_image_dir <- file_path(tempdir(), digest::digest(Sys.time()))

  dir_create(temp_image_dir)

  ast_file <- docx_to_ast(input_file, export_image_dir = temp_image_dir)

  # The dir containing to the images
  image_files <- list.files(
    file_path(temp_image_dir, "media"), full.names = TRUE
  )

  # Relative, not absolute filepaths are used in the ast
  old_targets <- image_files
  # Locate the files and find their hashes
  new_targets <- unlist(lapply(image_files, rename_file_with_hash))

  # Replace the image targets in the AST
  replace_ast_image_targets(
    input_ast   = ast_file,
    output_ast  = output_file,
    old_targets = old_targets,
    new_targets = new_targets
  )

  if (!export_new_images) {
    # If we don't need to export images, just return the path of the AST
    return(output_file)
  }


  # Extract new images (if required) -------------------------------------------


  # If we do, check that a comparison AST has also been provided
  if (is.null(image_export_comparison_file)) {
    stop(paste0(
      "image_export_comparison_file cannot be NULL if export_new_images is ",
      "TRUE. A comparison file is required to decide which images to export"
    ))
  }

  # Extract local image targets
  existing_local_targets <- extract_ast_image_targets(
    image_export_comparison_file
  )

  # Find remote targets which do not already exist in the local file (e.g. are
  # not generated locally, but have been added to the remote document)
  newly_added_remote_targets <-
    new_targets[!new_targets %in% existing_local_targets]

  newly_added_local_filenames <-
    old_targets[!new_targets %in% existing_local_targets]

  # Ensure that the directory that you want to copy new files into actually
  # exists
  dir_create(new_image_export_dir)

  # Copy the new targets to new_image_export_dir
  mapply(
    function(from, to) file.copy(from, to, overwrite = TRUE),
    newly_added_local_filenames,
    normalize_path(newly_added_remote_targets)
  )

  return(output_file)

}




# Simple function which relies on local and remote files having the same number
# of images, and in the same order. Both conditions should be true, for a single
# 'run' of a google document. It simply extracts the image targets from the
# remote AST, and replaces the targets in the local file with the equivalents,
# in order.
imagehash_local_ast_with_equivalent_remote_ast <- function(
  local_ast, remote_ast, output_ast = tempfile(fileext = ".ast")
  ) {

  # Parse image targets
  local_targets  <- extract_ast_image_targets(local_ast)
  remote_targets <- extract_ast_image_targets(remote_ast)

  replace_ast_image_targets(
    input_ast = local_ast,
    output_ast = output_ast,
    old_targets = local_targets,
    new_targets = remote_targets
  )

}



# Given an AST, a list of existing image targets, and a list of replacements for
# them, make the replacements, and write the new ast to output_ast.
#
# Note:  If a target is in the AST, but not in the lists, it will be unaffected.
replace_ast_image_targets <- function(input_ast,
                                      output_ast = tempfile(fileext = ".ast"),
                                      old_targets,
                                      new_targets) {

  if (length(new_targets) != length(old_targets))
    stop("old_targets and new_targets must have the same length")

  # A small 'pandoc filter', which should swap out the image references when the
  # AST is parsed
  image_swap <- function(key, value, ...) {
    if (key == "Image") {
      # The target of the image should always be in value[[3]][[1]]
      target     <- value[[3]][[1]]
      new_target <- new_targets[old_targets %in% target]

      return(
        pandocfilters::Image(new_target, text = NULL, caption = "")
      )
    }
    return(NULL)
  }

  pandocfilters_wrapper(image_swap, input_ast, output_ast)

  output_ast
}



# Given filepath to an AST, parse out the image targets, return as a character
# vector
extract_ast_image_targets <- function(ast_file) {

  # Init garget variable
  target <- vector()

  # A small 'pandoc filter', which writes out image targets to the 'target'
  # variable, stored one environment enclosure above. Used for its side-effects.
  image_extract <- function(key, value, ...) {
    # The target of the image should always be in value[[3]][[1]]
    if (key == "Image") target  <<- c(target, value[[3]][[1]])
  }

  # Run the filter function to populate the 'target variable'
  pandocfilters_wrapper(image_extract, ast_file, "blah")

  # Return it
  unlist(target)
}



# A small wrapper function for using the pandocfilters package, which seems to
# require formal text connections, which are a little verbose. Note: This could
# probably be simplified by using pandocfilters::astrapply directly
pandocfilters_wrapper <- function(fun, input_file, output_file) {

  input_file %>%
    from_json() %>%
    pandocfilters::astrapply(fun) %>%
    to_json() %>%
    writeLines(output_file)

}



rename_file_with_hash <- function(
  filepath,
  relative_image_dir = getOption("gd.new_image_path")
  ) {

  ext <- tools::file_ext(filepath)

  file_path(
    relative_image_dir,
    paste0(digest::digest(file = filepath, algo = "md5"), ".", ext)
  )

}
