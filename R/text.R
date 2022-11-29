
#' Wrap a character string to a specified character width
#'
#' Uses strwrap to split a string into subsets that are shorter than the
#' specified width and inserts newlines in between them. Inspired by this
#' [SO answer](https://stackoverflow.com/questions/7367138/text-wrap-for-plot-titles/7367534#7367534).
#'
#' @param string a character string to be wrapped- vector of length one
#' @param width numeric - desired width
#'
#' @return a character sting with new lines if required.
#' @export
#'
wrap_string <- function(string, width){
  paste(strwrap(string,
                width = width),
        collapse="\n")
}
