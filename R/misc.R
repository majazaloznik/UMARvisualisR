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
#' Finds y/limits that lead to a pretty y-axis. If the range of the values is
#' on the line, it artificially extends it by one more break.
#'
#' @param values vector of numeric values
#'
#' @return numeric vector of length 2 to be used as y-axis limit
#' @export
#'
find_pretty_ylim <- function(values){
  ylim <- range(pretty(c(values)), na.rm = TRUE)
  diff <- max(values, na.rm = TRUE) - min(values, na.rm = TRUE)
  if(ylim[1] == min(values, na.rm = TRUE)) {
    values <- c(values, min(values, na.rm = TRUE)-diff*0.05)}
  if(ylim[2] == max(values, na.rm = TRUE)) {
    values <- c(values, max(values, na.rm = TRUE)+diff*0.05)}
  range(pretty(c(values)), na.rm = TRUE)
}


#' Remove NA rows at start and end
#'
#' Removes rows containing NAs in the column `value`), but
#' only at the beginning and end of the table.
#'
#' @param df data frame with at least period and value columns
#'
#' @return dataframe with possibly fewer rows than going in.
#' @export
#'
remove_head_tail_NAs <- function(df){
  df %>% dplyr::arrange(period) -> df
  nisna <- !is.na(df$value)
  df[min(which(nisna)):max(which(nisna)),]
}


#' Apply limits to x-axis range of data
#'
#' Takes a dataframe with the datapoints and periods and removes the periods before xmin
#' (default is 1.1.2011) and after xmax (defaults to max in the data).
#'
#' @param df dataframe with period column in Date format
#' @param xmin Date, default "2011-01-01"
#' @param xmax Date, default maximum available in data.
#'
#' @return df as input with possibly fewer rows
#' @export
apply_xlims <- function(df, xmin = "2011-01-01", xmax =NULL){
  xmin <- as.Date(xmin)
 if(is.null(xmax)) xmax <- max(df$period)
 df <- remove_head_tail_NAs(df)
 df %>%
   dplyr::filter(period >= xmin & period <= xmax) -> df
 df
}


