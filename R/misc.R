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
  ylim <- range(pretty(c(values)), na.rm = TRUE)
  y_breaks <- pretty(c(values))
  mget(c("ylim", "y_breaks"))
}


#' Remove NA rows at start and end
#'
#' Removes rows containing NAs in the column `value`(or all the columns
#' except for `period` to be more precise), but
#' only at the beginning and end of the table.
#'
#' @param df data frame with at least period and value columns
#'
#' @return dataframe with possibly fewer rows than going in.
#' @export
#'
remove_head_tail_NAs <- function(df){
  df %>% dplyr::arrange(period) -> df
  nisna <- apply(!is.na(dplyr::select(df, -period, -period_id)), 1, any)
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

#' Helper function to guess encoding of a csv file and read slovenian csv
#'
#' guesses encoding and reads csv with slovenian delimiters
#'
#' @param file path to csv file
#'
#' @return content of the csv file
#' @export
#'
read_csv_guess_encoding <- function(file){
  enc <- readr::guess_encoding(file)[["encoding"]][1]
  readr::read_csv2(file,
                   locale = readr::locale(encoding = enc),
                   show_col_types = FALSE)
}



#' Test that all values in a vector are the same
#'
#' Testing that all values in a vector (or column) are the same.
#'
#' @param x vector
#'
#' @return logical
#' @export
#'
all_equal <- function(x) {
  length(unique(x)) == 1
}


#' Round like Excel
#'
#' Round so that .5 is always rounded up, unlike `base::round`
#' @param x numeric vector
#' @param digits number of digits
#'
#' @return vector of same lenght as x
#' @export
#'
rounding <- function (x, digits=0)
{
  posneg = sign(x)
  z = trunc(abs(x) * 10 ^ (digits + 1)) / 10
  z = floor(z * posneg + 0.5) / 10 ^ digits
  return(z)
}


#' Helper to regex the interval from the datapoints table
#'
#' @param string period string
#'
#' @return single character string
#' @export
#'
get_interval <- function(string) {
 ifelse(grepl(".+M.+", string), "M",
        ifelse(grepl(".+Q.+", string), "Q",
               ifelse(grepl("[0-9]{4}", string), "A", NA)))
}

#' Add period date column from period ID
#'
#' Converts the 2023M02 type and 2023Q2 into 1.2.2023 or 1.4.2023, while
#' keeping the year alone.
#'
#' @param df dataframe with a period_id column
#'
#' @return dataframe with an extra column called period with the dates
#' @export
#'
add_date_from_period_id <- function(df){
  df %>%
    dplyr::rowwise() |>
    dplyr::mutate(interval = get_interval(period_id)) |>
    dplyr::mutate(period = dplyr::if_else(interval ==  "M", lubridate::ym(period_id, quiet = TRUE),
                                          dplyr::if_else(interval == "Q", lubridate::yq(period_id, quiet = TRUE),
                                                         dplyr::if_else(interval == "A", lubridate::ymd(period_id, truncated = 2L, quiet = TRUE),
                                                                        as.Date(NA))))) %>%
    dplyr::select(-interval)
}



#' Get max period id from list of dataframes with with periods and their ids
#'
#' Helper function to get the period id for the most recent data point in a
#' list of dataframes which have period (date) and period_id columns (i.e. 2023M03)
#'
#' @param data_points list of dataframes with period and period_id columns
#'
#' @return value of the most recent period_id in list
#' @keywords internal
#'
get_max_period <- function(data_points){
  # Initialize variables for max_period and max_period_id
  max_info <- list(max_period = -Inf, max_period_id = NA)
  # Find max_period and corresponding max_period_id across data frames
  max_info <- purrr::reduce(data_points, function(curr_max_info, df) {
    idx_max_in_df <- which.max(df$period)
    max_in_df <- df$period[idx_max_in_df]
    if (max_in_df > curr_max_info$max_period) {
      curr_max_info$max_period <- max_in_df
      curr_max_info$max_period_id <- df$period_id[idx_max_in_df]
    }
    curr_max_info
  }, .init = max_info)
  max_info$max_period_id
}



#' Get unique values without NA
#'
#' @param vec vector
#'
#' @return vector of unique without NAs
#' @keywords internal
unique_without_na <- function(vec) {
  unique(vec[!is.na(vec)])
}

#' Check if a column has values and they are all the same
#'
#' Column check that returns true if all valid values are the same. This
#' means all NAs or some NAs are also OK, basically saying the column
#' has a single unique valid value, which can be used as a parameter downstream
#'
#'
#' @param column a dataframe column
#'
#' @return any(duplicated(
#' @keywords internal
check_consistency_or_na <- function(column) {
  # Extract the unique non-missing values
  unique_vals <- unique_without_na(column)
  # Check if there's more than one unique non-missing value
  if (length(unique_vals) > 1) {
    FALSE  } else {TRUE}
}

#' Check if a column has values and they are all the different or NA
#'
#' Column check that returns true if all valid values are the different This
#' means all NAs or some NAs are also OK, no duplicates excep for NAs if they exist
#'
#' @param column a dataframe column
#'
#' @return logical
#' @keywords internal
check_uniqueness_or_na <- function(column) {
  # Extract the unique non-missing values
  valid_vals <- column[!is.na(column)]
  # Check if there's more than one unique non-missing value
  if (any(duplicated((valid_vals)))) {
    FALSE  } else {TRUE}
}

#' Updates units in publication chart input table
#'
#' Updates the units based on the transformations - to index or% if necessary -
#' and then queries the database for the remaining units if there are still
#' any missing. And fixes the capitalisation ready for the y-label
#'
#' @param df with at a minimum the columns serija, enota, indeks_let and rast
#' @param con database connection
#'
#' @return df with updated
#' @export
update_units <- function(df, con){
 df |>
    dplyr::mutate(enota = ifelse(!is.na(indeks_obdobje), paste0("Indeks (",
                                                                indeks_obdobje, " = 100)"),
                                 ifelse(!is.na(rast), "%",
                                        enota))) |>
    dplyr::rowwise() |>
    dplyr::mutate(enota = ifelse(is.na(enota), UMARaccessR::get_unit_from_series(
      UMARaccessR::get_series_id_from_series_code(serija, con), con), enota)) |>
    dplyr::mutate(enota = dplyr::if_else(enota == "eur", "EUR",
                                  dplyr::if_else(enota == "mio eur", "Mio EUR", first_up(enota))))
}


#' New fun to get date from period with three types of alignment
#'
#' takes year, quarter or month format and returns first ("l"), middle ("c")
#' or last ("r") date, the default is "c"
#'
#' @param period one of three types
#' @param position l, c or r, defaults to middle
#'
#' @return date
#' @export
get_date_from_period <- function(period, position = c("c", "l", "r")) {
  position <- match.arg(position)
  if (grepl("^\\d{4}$", period)) {
    date <- lubridate::ymd(period, truncated = 2L)
    if (position == "l") date <- lubridate::ymd(period, truncated = 2L)
    else if (position == "c") date <- lubridate::ymd(paste0(period, "-07-01"))
    else if (position == "r") date <- lubridate::ymd(paste0(period, "-12-31"))
  } else if (grepl("^\\d{4}Q\\d{1}$", period)) {
    date <- lubridate::yq(period)
    if (position == "l") date <- lubridate::yq(period)
    else if (position == "c") {
      date <- date + months(1) # Start of 2nd month
      # Get to the middle of this month
      date <- date + lubridate::days(floor(lubridate::day(lubridate::ceiling_date(date, "month") - lubridate::days(1)) / 2))
    } else if (position == "r") date <- lubridate::ceiling_date(date, "quarter") - lubridate::days(1)
  } else if (grepl("^\\d{4}M\\d{2}$", period)) {
    date <- lubridate::ym(paste0(substr(period, 1, 4), "-", substr(period, 6, 7)))
    if (position == "l") date <- lubridate::ym(paste0(substr(period, 1, 4), "-", substr(period, 6, 7)))
    else if (position == "c") date <- date + lubridate::days(floor(lubridate::day(lubridate::ceiling_date(date, "month") - lubridate::days(1)) / 2))
    else if (position == "r") date <- lubridate::ceiling_date(date, "month") - lubridate::days(1)
  } else {
    stop("Invalid period format")
  }
date
}


#' Replace the period_id column with the correctly alligned date
#'
#' @param df dataframe with period_id column
#' @param position defaults to "c", but "l" and "r" are also valid
#'
#' @return df with same number of cols
#' @export
#'
replace_period_id_column <- function(df, position = "c"){
  df |>
    dplyr::rowwise() |>
    dplyr::mutate(date = get_date_from_period(period_id, position), .keep = "unused") |>
    dplyr::relocate(date) |>
    dplyr::ungroup()
}



#' Helper to get the time interval from a date column
#'
#' @param date_column what it says on the box
#'
#' @return integer value in months
#' @export
get_interval_in_months <- function(date_column) {
  # Calculate the difference between the first two dates
  interval <- as.numeric(difftime(date_column[2], date_column[1], units = "weeks")) / 4.34524
  round(interval)
}

#' Strip rows with NA values
#'
#' @param df dataframe with date and value column
#'
#' @return df with same number of cols
#' @export
#'
strip_na_rows <- function(df){
  df |>
    dplyr::mutate(date = get_date_from_period(period_id, position), .keep = "unused") |>
    dplyr::relocate(date) |>
    dplyr::ungroup()
}


#' Helper funciton to wrap title to appropriate width
#'
#' Had chatgpt write this for me pretty much. does what it says on the box.
#'
#' @param title character
#' @param max_width in user units - default is plot width from par("usr")
#' @param cex numeric
#' @param family of fonts.
#' @param font value for font style
#'
#' @return title but wiht line breaks where needed
#' @export
wrap_title <- function(title, max_width = NULL, cex = 1, family = "Myriad Pro", font = 1) {
  if (is.null(title) || identical(title, "") || is.na(title)) {
    return(list(title = NULL, num_lines = 0))
  }
  if (is.null(max_width)) max_width <- par("usr")[2] - par("usr")[1]
  words <- strsplit(title, " ")[[1]]
  current_line <- words[1]
  lines <- character(0)

  for (word in words[-1]) {
    temp_line <- paste(current_line, word)
    if (strwidth(temp_line, cex = cex, family = family, font = font) <= max_width) {
      current_line <- temp_line
    } else {
      lines <- c(lines, current_line)
      current_line <- word
    }
  }

  lines <- c(lines, current_line)
  wrapped_title <- paste(lines, collapse = "\n")
  return(list(title = wrapped_title, num_lines = length(lines)))
}


#' helper function to get number of lines required for the legend
#'
#' @param elements string of legend entries
#' @param columns numeric value of number of columns
#'
#' @return integer value
#' @export
#'
get_legend_lines <- function(elements, columns){
  if (length(elements) == 1) {lines <- 0} else {
    lines <- ceiling(length(elements) / columns)
  }
  lines
}
