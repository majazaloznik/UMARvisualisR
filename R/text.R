
#' Wrap a character string to a specified character width
#'
#' Uses strwrap to split a string into subsets that are shorter than the
#' specified width and inserts newlines in between them. Inspired by this
#' [SO answer](https://stackoverflow.com/questions/7367138/text-wrap-for-plot-titles/7367534#7367534).
#'
#' @param string a character string to be wrapped- vector of length one
#' @param width numeric - desired width
#'
#' @return a list with the first element a character string with new lines if required and the
#' second one a numeric value for how many rows are in the returned string.
#'
#' @export
#'
wrap_string <- function(string, width){
  split_text <- strwrap(string,
                        width = width)
  n <- length(split_text)
  wrapped_text <- paste(split_text,
        collapse="\n")
  return(list(wrapped_text, n))
}
