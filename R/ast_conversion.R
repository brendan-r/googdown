# For ODT / MS Word files ------------------------------------------------------

# Small function to return a commonly used pandoc option list
#' @keywords internal
pandoc_options <- function(...) {
  c("--atx-headers", paste0("--wrap=", getOption("gd.wrap")), ...)
}


odt_to_md <- function(input_file, output_file = tempfile(fileext = ".md")) {

  pandoc_wrapper(
    input   = input_file,
    output  = output_file,
    from    = "odt",
    to      = "markdown",
    options = pandoc_options()
  )

}



docx_to_md <- function(input_file, output_file = tempfile(fileext = ".md"),
                       export_image_dir = NULL) {


  # If export_image_dir is specified, then create a string to pass to the pandoc
  # CLI, which will mean that images from the docx will be extracted to
  # export_image_dir
  #
  # Note: Pandoc will export the images to export_image_dir/media
  if (!is.null(export_image_dir)) {
    image_dir_pandoc_option <- paste0(
      "--extract-media=", normalizePath(export_image_dir)
    )
  } else {
    image_dir_pandoc_option <- NULL
  }

  pandoc_wrapper(
    input   = input_file,
    output  = output_file,
    from    = "docx",
    to      = "markdown",
    options = pandoc_options(image_dir_pandoc_option)
  )

}


odt_to_ast <- function(input_file, output_file = tempfile(fileext = ".ast")) {

  # Convert to Markdown to to drop unusable metadata
  temp <- odt_to_md(input_file)

  # Convert markdown to AST
  md_to_ast(temp, output_file)

}


docx_to_ast <- function(input_file, output_file = tempfile(fileext = ".ast"),
                        export_image_dir = NULL) {

  # Convert to Markdown to to drop unusable metadata
  temp <- docx_to_md(input_file, export_image_dir = export_image_dir)

  # Convert markdown to AST
  md_to_ast(temp, output_file)

}



# Take a doc_id, and returns an 'image-hashed' AST
doc_id_to_ast <- function(
  doc_id,
  output_file                  = tempfile(fileext = ".ast"),
  revision                     = NA, pull_in_new_images = FALSE,
  image_export_comparison_file = NULL
  ) {

  # A tempfile for downloading the docx
  temp_docx_file <- tempfile(fileext = ".docx")

  # Download the remote google document as a docx file
  gd_export(doc_id = doc_id, file_name = temp_docx_file, revision = revision)

  # Take the .docx file, and convert to an 'image-hashed' AST
  remote_docx_to_imagehashed_ast(
    input_file                   = temp_docx_file,
    output_file                  = output_file,
    export_new_images            = pull_in_new_images,
    image_export_comparison_file = image_export_comparison_file
  )

  # Check integrity by running it through pandoc again
  ast_to_ast(output_file, output_file)

  # If you've made it this far without an error, all should be well
  return(output_file)

}





# For markdown -----------------------------------------------------------------

ast_to_md <- function(input_file, output_file = tempfile(fileext = ".md")) {

  pandoc_wrapper(
    input   = input_file,
    output  = output_file,
    from    = "json",
    to      = "markdown",
    options = pandoc_options()
  )

}


md_to_ast <- function(input_file, output_file = tempfile(fileext = ".ast")) {


  pandoc_wrapper(
    input   = input_file,
    output  = output_file,
    from    = "markdown",
    to      = "json",
    options = pandoc_options()
  )

  output_file %>%
    read_txt() %>%
    jsonlite::prettify() %>%
    writeLines(output_file)

  output_file

}


# Convert a file from markdown, to, er, markdown. Used in the tests to
# 'standardize' the markdown conventions used
md_to_md <- function(input_file, output_file = tempfile(fileext = ".md")) {

  pandoc_wrapper(
    input   = input_file,
    output  = output_file,
    from    = "markdown",
    to      = "markdown",
    options = pandoc_options()
  )

}


# Convert an rmd file to ast, and then back again
rmd_to_rmd <- function(input_file, output_file = tempfile(fileext = ".md")) {
  ast_to_rmd(rmd_to_ast(input_file), output_file)
}


# For Rmarkdown ----------------------------------------------------------------

ast_to_rmd <- function(input_file, output_file = tempfile(fileext = ".Rmd"),
                       unescape = TRUE) {

  # Sometimes you want to escape strings (usually codeblacks after a diff),
  # sometimes you don't
  if (unescape) {
    unescape_fun <- unescape_ast_stuff
  } else {
    unescape_fun <- function(x) x
  }


  # Take the pandoc markdown output, and change the pandoc fenced code block
  # attributes back into knitr chunk-option headers
  input_file %>%
    unescape_fun() %>%
    ast_to_md() %>%
    readLines() %>%
    pandoc_fenced_to_knitr_block() %>%
    writeLines(output_file)

  output_file

}


# Should you add the 'no_yaml' stage here?
rmd_to_ast <- function(input_file, output_file = tempfile(fileext = ".ast")) {

  temp_file <- tempfile(fileext = ".Rmd")

  # Turn knitr code chunks/blocks into pandoc fenced blocks with attributes
  readLines(input_file) %>%
    knitr_block_to_pandoc_fenced() %>%
    writeLines(temp_file)

  # Convert the Rmarkdown file (now with code block headers pandoc can read)
  # into JSON, prettify it, and write to the output file
  md_to_ast(temp_file, output_file)

}



# Misc -------------------------------------------------------------------------

ast_to_ast <- function(input_file, output_file = tempfile(fileext = ".ast")) {

  pandoc_wrapper(
    input   = input_file,
    output  = output_file,
    from    = "json",
    to      = "json",
    options = pandoc_options()
  )

}


# A wrapper function around rmarkdown::pandoc_convert
#
# Makes fewer parameters optional, attempts (though fails) to suppress system
# writing to the console
pandoc_wrapper <- function(input, output, from, to, options = NULL, verbose = FALSE,
                           ...) {

rmarkdown::pandoc_convert(
    input   = normalizePath(input),
    output  = output,
    from    = from,
    to      = to,
    options = options,
    verbose = verbose,
    ...
  )

  return(output)
}



##' Convert a file to commonmark, using system pandoc
##'
##' @param input_file The file you'd like to conver
##' @return Used for its side effects
standardize_rmd <- function(input_file) {

  # Extact the yaml header, in order to reattach it later (not preserved)
  partitioned_doc <- partition_yaml_front_matter(readLines(input_file))
  old_body_file   <- tempfile(fileext = ".Rmd")
  new_body_file   <- tempfile(fileext = ".Rmd")

  # Write the body out to a tempfile for simplicity
  writeLines(partitioned_doc$body, old_body_file)

  # Run the body through the pandoc process, and back again. Usually called on
  # files 'in place' (e.g. which have not yet been through the diffing mange);
  # no need to unescape unicode chars
  new_body_file <- ast_to_rmd(
    rmd_to_ast(old_body_file), old_body_file, unescape = FALSE
  )

  # Add the new, standardised body to the (unchanged YAML front matter)
  partitioned_doc$body <- c("", readLines(new_body_file))

  # Write the whole thing back to the original file
  writeLines(unlist(partitioned_doc), input_file)

  # Log something for the user
  catif(input_file, " converted to standard pandoc markdown")

}

# fold_ast_json unfortunately introduces some problems: by treating JSON as a
# string within a 'real' json object, the contents many nodes get double
# escaped, meaning that strings with newlines end up as \\n, and anything with
# more than one backslash ends up with double the number it started out with.
#
# This function is a dirty hack: it parses the pandoc AST, and un-escapes the
# contents. The alternative is/was to do this over the entire AST file (but this
# breaks the JSON), or over the resultant markdown (which has some other
# undesirable effects). Of all the dirty hacks (aside from re-factoring
# fold_ast_json), this was the least dirty and the last hackish.
unescape_ast_stuff <- function(input_file,
                               output_file = tempfile(fileext = ".ast")) {

  if (!jsonlite::validate(read_txt(input_file))) {
    stop("Input file is not valid JSON")
  }

  # Internal convenience function imported from the pandocfilters package
  Type <- function(x) {
    structure(list(t = x, c = list()), class = c("Type", "list"))
  }

  # New constructor, for DisplayMath (seemingly omitted by the particulate
  # authors, though I might have just missed it)
  DisplayMath <- function(x) {
    structure(
      list(t = "Math", c = list(Type("DisplayMath"), x)),
      class = c("block", "list")
    )
  }

  # A pandoc filter function, to be applied recursively to the AST. Where a
  # "Math" node is encountered, replace it with a version with unescaped
  # contents
  unescape_stuff <- function(key, value, ...) {
    if (key == "Math") {

      if (value[[1]]$t == "InlineMath") {

        return(pandocfilters::Math(
          stringi::stri_unescape_unicode(
            stringi::stri_unescape_unicode(
              value[[2]]
            )
          )
        ))

      } else if (value[[1]]$t == "DisplayMath") {

        return(DisplayMath(
          stringi::stri_unescape_unicode(
            stringi::stri_unescape_unicode(
              value[[2]]
            )
          )
        ))

      }

    } else if (key == "CodeBlock") {

      return(pandocfilters::CodeBlock(
        value[[1]], stringi::stri_unescape_unicode(value[[2]])
      ))

    } else if (key == "Code") {

      return(pandocfilters::Code(
        stringi::stri_unescape_unicode(
          stringi::stri_unescape_unicode(
            value[[2]]
          )
        )
      ))

    } else if (key == "Str") {
      # This should cover everything else. Doing it twice seems unnecessary. To
      # be painfully honest you don't understand why.
      return(pandocfilters::Str(
        stringi::stri_unescape_unicode(
          stringi::stri_unescape_unicode(
            value
          )
        )
      ))

    }

    return(NULL)
  }

  # Read in the input file, unescape the stuff, write the output file
  from_json(input_file) %>%
    pandocfilters::astrapply(unescape_stuff) %>%
    to_json() %>%
    writeLines(output_file)

  # Check it worked
  if (!jsonlite::validate(read_txt(output_file))) {
    stop("JSON validation lost while unescaping AST contents.")
  }

  return(output_file)
}
