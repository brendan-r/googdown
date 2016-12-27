# Yaml funs, swiped from the internals of package `rmarkdown`:
# https://github.com/rstudio/rmarkdown/blob/395d2a39b0cd3b3220ba5ab53bcb47302f9c7cae/R/output_format.R



partition_yaml_front_matter <- function(input_lines) {
  
  validate_front_matter <- function(delimiters) {
    if (length(delimiters) >= 2 &&
        (delimiters[2] - delimiters[1] > 1) &&
        grepl("^---\\s*$", input_lines[delimiters[1]])) {
      # verify that it's truly front matter (not preceded by other content)
      if (delimiters[1] == 1)
        TRUE
      else
        is_blank(input_lines[1:delimiters[1]-1])
    } else {
      FALSE
    }
  }
  
  # is there yaml front matter?
  delimiters <- grep("^(---|\\.\\.\\.)\\s*$", input_lines)
  if (validate_front_matter(delimiters)) {
    
    front_matter <- input_lines[(delimiters[1]):(delimiters[2])]
    
    input_body <- c()
    
    if (delimiters[1] > 1)
      input_body <- c(input_body,
                      input_lines[1:delimiters[1]-1])
    
    if (delimiters[2] < length(input_lines))
      input_body <- c(input_body,
                      input_lines[-(1:delimiters[2])])
    
    list(front_matter = front_matter,
         body = input_body)
  }
  else {
    list(front_matter = NULL,
         body = input_lines)
  }
}
