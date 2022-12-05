#' Helper functions for testing if a value is in a range
#'
#' in_range checks if the value is within the range, including the limits,
#' in_range_strict excludes the limits.

#' @name ranges
#' @param x numeric vector of length one
#' @param r a range - numeric vector of lenght two, ascending
#'
#' @return logical
#'
NULL
#' @rdname ranges
#' @export
in_range <- function (x, r) {
  if(!(length(r) == 2 && r[1] <= r[2])) stop("not a real range")
  return(x >= r[1] & x <= r[2])
}

#' @rdname ranges
#' @export
in_range_strict <- function (x, r) {
  if(!(length(r) == 2 && r[1] <= r[2])) stop("not a real range")
  return(x > r[1] & x < r[2])
}


#' Helper function to capitalise firs letter of string
#'
#' Capitalise the first letter of a string, but keep capitalization as is
#' on other letters.
#'
#' @param x character string
#'
#' @return a character string with first letter capitalized.
#' @export
first_up <- function(x) {
  stringr::str_replace(x, "^\\w{1}", toupper)
}

#' Find pretty y-axis limits
#'
#' Finds ylimits that lead to a pretty y-axis. If the range of the values is
#' on the line, it artificially extends it by one more break.
#'
#' @param values vector of numeric values
#'
#' @return numeric vector of length 2 to be used as y-axis limit
#' @export
#'
find_pretty_ylim <- function(values){
  ylim <- range(pretty(c(values)), na.rm = TRUE)
  if(ylim[1] == min(values, na.rm = TRUE)) {
    values <- c(values, min(values, na.rm = TRUE)*0.99)}
  if(ylim[2] == max(values, na.rm = TRUE)) {
    values <- c(values, max(values, na.rm = TRUE)*1.01)}
  range(pretty(c(values)), na.rm = TRUE)
}
