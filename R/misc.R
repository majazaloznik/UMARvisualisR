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
