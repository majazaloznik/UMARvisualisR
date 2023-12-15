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
  if(ylim[1] == min(values, na.rm = TRUE) & ylim[1] != 0) {
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

#' My legend function
#'
#' @param x	 the x and y co-ordinates to be used to position the legend. They can be specified by keyword or in any way which is accepted by xy.coords: See ‘Details’.
#' @param y the x and y co-ordinates to be used to position the legend. They can be specified by keyword or in any way which is accepted by xy.coords: See ‘Details’.
#' @param legend a character or expression vector of length ≥ 1 to appear in the legend. Other objects will be coerced by as.graphicsAnnot.
#' @param fill if specified, this argument will cause boxes filled with the specified colors (or shaded in the specified colors) to appear beside the legend text.
#' @param col color of points or lines appearing in the legend.
#' @param lty the line types and widths for lines appearing in the legend. One of these two must be specified for line drawing.
#' @param lwd the line types and widths for lines appearing in the legend. One of these two must be specified for line drawing.
#' @param xjust how the legend is to be justified relative to the legend x location. A value of 0 means left justified, 0.5 means centered and 1 means right justified.
#' @param yjust the same as xjust for the legend y location.
#' @param x.intersp character interspacing factor for horizontal (x) spacing.
#' @param y.intersp the same for vertical (y) line distances.
#' @param text.col the color used for the legend text.
#' @param text.font the font used for the legend text, see text.
#' @param ncol the number of columns in which to set the legend items (default is 1, a vertical legend).
#' @param family font family
#'
#' @return nothing, draws legend
#' @export
#'
legend_mz2 <- function(x = par("usr")[[1]],
                       y =  par("usr")[[4]], legend,
                       fill = NULL, col = NULL, lty = NULL, lwd = NULL,
                       xjust = 0, yjust = 1, x.intersp = 0.2, y.intersp = 0.7,
                       text.col = "black", text.font = NULL, ncol = 1,
                       family = "Myriad Pro") {

  # check what are we doing
  mfill <- !missing(fill) && !all(is.na(fill))
  do.lines <- !missing(lty) && !all(is.na(lty))

  # set par
  op <- par("xpd")
  on.exit(par(xpd = op))
  par(xpd = TRUE)
  par(family = family)

  # get legend length
  legend <- as.graphicsAnnot(legend)
  n.leg <- if (is.call(legend))     1   else length(legend)
  # get numer of legend lines per column
  n.legpercol <- ceiling(n.leg/ncol)

  # get legend position coordinates
  xy <- xy.coords(x, y, setLab = FALSE)
  x <- xy$x
  y <- xy$y

  # rewrite rect fun to use dx and dy a
  rect2 <- function(left, top, dx, dy, ...) {
    r <- left + dx
    b <- top - dy
    rect(left, top, r, b, ...)
  }
  # rewrite segments fun to use dx and dy
  segments2 <- function(x1, y1, dx, dy, ...) {
    x2 <- x1 + dx
    y2 <- y1 + dy
    segments(x1, y1, x2, y2, ...)
  }

  # get character size into user coordinates
  cex <- par("cex")
  text.width <- max(abs(strwidth(legend, units = "user",
                                 cex = cex, font = text.font)))
  xyc <- xyinch(par("cin"), warn.log = FALSE)
  xchar <- cex * xyc[1] # character width in user coordinates
  ychar <- cex * xyc[2] # character height in user coordinates

  # dimensions of box or line symbol
  if (!do.lines) xbox <- xchar * 0.54 else
    xbox <- xchar * 2
  ybox <- ychar * 0.4
  # Calculate column widths based on the longest string in each column
  column_widths <- sapply(1:ncol, function(i) {
    start_idx <- (i-1) * n.legpercol + 1
    end_idx <- min(i * n.legpercol, n.leg)
    max(abs(strwidth(legend[start_idx:end_idx], units = "user", cex = cex, font = text.font)))
  })

  # Add the width of the legend symbol (box or line) and spaces to the text width
  column_widths <- column_widths + xbox + (x.intersp + 1) * xchar


  # height of the whole legend box
  h <- (n.legpercol + 0.3) * ychar * y.intersp

  # width of single column
  w0 <- xbox + text.width + (x.intersp + 1) * xchar

  # width of whole box - kinda irrelevant if you don't use xjust
  w <- ncol * w0 + 0.5 * xchar

  # adjustments of starting position relative to xy / might throw this out.
  left <- x - xjust * w
  top <- y + (1 - yjust) * h

  # # x coordinates of legend elements symbols
  # xs <- left  + (w0 * rep.int(0:(ncol - 1),
  #                             rep.int(n.legpercol, ncol)))[1L:n.leg]
  # Compute the xs values for the legend symbols using the column widths
  xs <- left + c(0, cumsum(head(column_widths, -1)))[rep(1:ncol, each=n.legpercol)][1:n.leg]

  ys <- top   - y.intersp * ychar * (rep.int(1L:n.legpercol, ncol)[1L:n.leg]  )

  # plot rectangles
  if (mfill) {
    if (!is.null(fill))
      fill <- rep_len(fill, n.leg)
    rect2(left = xs, top = ys + ybox, dx = xbox,
          dy = ybox,  col = fill, border = NA)
  }

  # plot lines
  if (do.lines)
    col <- rep_len(col, n.leg)
  if (missing(lwd) || is.null(lwd))
    lwd <- par("lwd")
  if (do.lines) {
    if (missing(lty) || is.null(lty))
      lty <- 1
    lty <- rep_len(lty, n.leg)
    lwd <- rep_len(lwd, n.leg)
    ok.l <- !is.na(lty) & (is.character(lty) | lty > 0) &
      !is.na(lwd)
    segments2(xs[ok.l] , ys[ok.l] + ybox/2, dx = xbox, dy = 0, lty = lty[ok.l], lwd = lwd[ok.l],
              col = col[ok.l])
  }

  # x coordinates of legend text
  xt <- xs + xbox
  yt <- ys
  # add space between symbol and text
  xt <- xt + x.intersp * xchar

  # plot text
  text(xt, yt, labels = legend, adj = c(0, 0), cex = cex,
       col = text.col, font = text.font)

}


#' Prepare left axis labels and get width
#'
#' Changes euros to millions of euros if necessary and calculates how much
#' space the left margin needs to be. Returns axis title, axis labels, and
#' number of lines
#'
#' @param config config dictionary list from \link[UMARvisualisR]{prep_config}
#' @param y_axis output of \link[UMARvisualisR]{find_pretty_ylim}
#'
#' @return list of axis title, axis labels, and
#' number of lines
#' @export
#'
left_axis_label_width <- function(config, y_axis) {

  y_label_max <- max(y_axis$ylim)
  axis_labels <- y_axis$y_breaks
  axis_positions <- y_axis$y_breaks
  unit <- unique(unlist(purrr::map(config$series, ~ .x$unit)))
  mio_eur <- unique(unlist(purrr::map(config$series, ~ .x$mio_eur)))
  if(unit == "EUR" & mio_eur) {
    axis_labels <- axis_labels/1000000
    y_label_max <- max(abs(axis_labels))
    unit <- "Mio EUR"}

  y_lab_lines <- strwidth(format(y_label_max, big.mark = ".", decimal.mark = ",",
                                 scientific = FALSE),
                          units = "inches")/par("csi") + 0.5
  current_mar <- par("mar")
  current_mar[2] <- y_lab_lines + 1
  par(mar =  current_mar)
  mget(c("unit",  "axis_labels", "axis_positions", "y_lab_lines"))
}



#' Draw left axis labels
#'
#' @param unit unit usually from \link[UMARvisualisR]{left_axis_label_width}
#' @param axis_positions axis label positions usually from \link[UMARvisualisR]{left_axis_label_width}
#' @param axis_labels labels from \link[UMARvisualisR]{left_axis_label_width}
#' @param y_lab_lines number of margin lines from \link[UMARvisualisR]{left_axis_label_width}
#'
#' @return nothing, draws axis title and labels
#' @export
left_axis_labels <- function(unit, axis_positions, axis_labels, y_lab_lines){
  par(mgp=c(3,0.5,0), xpd = FALSE)
  axis(2, at = axis_positions,
       labels = format(axis_labels, big.mark = ".", decimal.mark = ",",
                       scientific = FALSE),
       col = umar_cols("gridlines"), lwd = 0,  tck = 0.0,
       las = 2,  family ="Myriad Pro")
  mtext(unit, side = 2,
        line = y_lab_lines + 0.1, family ="Myriad Pro")
}


#' Get data values as a vector
#'
#' Returns the data values as a vector to be used to get the ylims, but
#' also adds a 0 to the range if the chart is a barchart and also makes
#' sure the ylims are correct if it's a stacked barchart. Christ, i am
#' really overdoing this shit.
#'
#' @param datapoints list of dataframes from \link[UMARvisualisR]{prep_data}
#' @param config config dictionary list from \link[UMARvisualisR]{prep_config}
#'
#' @return vector of numeric values
#' @export
get_data_values <- function(datapoints, config){
  values <- unlist(purrr::map(datapoints, ~ .x$value))

  if(config$stacked) {
    series_types <- vapply(config$series, \(x) x$type, character(1))
    bar_datapoints <- datapoints[series_types == "bar"]
    bar_datapoints <- purrr::reduce(bar_datapoints, ~dplyr::full_join(.x, .y, by = "date"))
    bar_datapoints <-  bar_datapoints |>
      dplyr::rowwise() |>
      dplyr:: mutate(sum = sum(dplyr::c_across(where(is.numeric))))
    values <- c(values, bar_datapoints$sum)
  }

  if(any(vapply(config$series, \(x) x$type, character(1)) == "bar")) values <- c(values, 0)
  return(values)
}



#' Check if timeseries has a complete last year
#'
#' @param datapoints_df dataframe with date column
#' @param max_year year to be checking if it is complete
#'
#' @return logical
#' @export
#'
last_year_complete_series <- function(datapoints_df, max_year) {
  datapoints_df |>
    dplyr::mutate(year = lubridate::year(date)) |>
    dplyr::group_by(year) |>
    dplyr::filter(year == max_year) |>
    dplyr::mutate(count = dplyr::n()) |>
    dplyr::filter(count == max(count), date == max(date)) -> last_period

  # check if ends in Q4 or 12th month
  if(last_period$count == 4 & lubridate::month(last_period$date) >= 10) {
    complete <- TRUE
    } else if (last_period$count == 12 ) {
      complete <- TRUE
    }  else if (determine_interval(datapoints_df) == "A"){
      complete <- TRUE} else {complete <- FALSE}
  return(complete)
}


#' Check if at least one timeseries has a complete last year
#'
#' Takes a list of dataframes with a date column and checks
#' if at least one of the series has a complete final year.
#'
#' @param datapoints list od datapoints dataframes
#'
#' @return logical
#' @export
#'
last_year_complete <- function(datapoints) {
   max_date <- as.Date(max(unlist(purrr::map(datapoints, ~ .x$date))),
                       origin = "1970-01-01")
   max_year <- lubridate::year(max_date)
  any(sapply(datapoints, last_year_complete_series, max_year))
}




#' Medium year label squishing
#'
#' Squishes all but the first year to YY instead of YYYY
#'
#' @param sequence numeric sequence of YYYY values of lenght >2
#'
#' @return same length sequence of character labels
#' @export
#'
year_squisher_medium <- function(sequence){
  new_sequence <- as.character(sequence[1])
  c(new_sequence,substr(sequence[-1],3,4))
}

#' Extra  year label squishing
#'
#' Squishes all but the first year to YY instead of YYYY, but
#' only keeps every 5th year plust the first and last value.
#'
#' @param sequence numeric sequence of YYYY values of lenght >2
#'
#' @return same length sequence of character labels
#' @export
year_squisher_extra <- function(sequence){
  middle_seq <- sequence[2:(length(sequence)-1)]
  middle_seq <- ifelse(as.numeric(middle_seq) %% 5 == 0, as.character(middle_seq), NA)
  c(as.character(sequence[1]), middle_seq, as.character(sequence[length(sequence)]))

}

#' Year label squishing function
#'
#' Squishes year labels using medium squishing - all but the first label
#' go to YY - or extra squishing: keeping only every 5th one, plus the first
#' and last value.
#'
#' @param sequence numeric sequence of YYYY values of lenght >2
#' @param extra logical indicator of extra squishing
#'
#' @return same length sequence of character labels
#' @export
#'
year_squisher <- function(sequence, extra = FALSE){
  if (!extra) year_squisher_medium(sequence) else
    year_squisher_extra(sequence)
}


#' Check if all datapoints have annual data only
#'
#' Takes list of dataframes with datapoints and checks that all of them
#' have annual data only - regardless of the horizontal alignment i.e.
#' start, middle or end of they year.
#'
#' @param datapoints list od datapoints dataframes
#'
#' @return logical value indicating truth
#' @export
#'
only_annual_intervals <- function(datapoints) {
  all(sapply(datapoints, determine_interval) == "A")
}


#' Get first day of the year
#'
#' @param date date
#'
#' @return date
#' @export
#'
first_day_of_year <- function(date) {
  lubridate::ymd(paste0(lubridate::year(date), "-01-01"))
}

#' Get last day of the year
#'
#' @param date date
#'
#' @return date
#' @export
#'
last_day_of_year <- function(date) {
  lubridate::ymd(paste0(lubridate::year(date), "-12-31"))
}

#' Add 6 months to date vector
#'
#' @param dates vector of dates
#'
#' @return vector of same length shifted to the right by 6 months
#' @export
#'
shift_dates_by_six_months <- function(dates) {
  dates + months(6)
}

#' Stop par mgp warning
#'
#' convenience funciton to stop annoying warning when using differing
#' signs in the mgp parameters. For some reason cannot be suppressed using
#' suppressWarnings, but this works
#'
#' @param mgp numeric vector of length 3 to set mgp
#'
#' @return nothing, changes mgp parameter
#' @export
par_mgp <- function(mgp=c(3,-0.2,0)) {
  old_warn <- options("warn")
  options(warn = -1)
  # Set the par parameters
  par(mgp = mgp)
  # Restore previous warning options
  options(old_warn)
}

#' Determine interval from timeseries dataframe
#'
#' function to return A, Q or M based on differences
#' in the time periods in the dataframe.
#'
#' @param df dataframe with date column
#'
#' @return A, Q or M - or NA
#' @export
#'
determine_interval <- function(df) {
  df$date <- sort(as.Date(df$date))
  date_diffs <- diff(df$date)
  # Check for annual data
  if (all(abs(as.numeric(date_diffs, units = "days") - 365) <= 1)) {
    return("A")
  }
  # Check for quarterly data
  if (all(abs(as.numeric(date_diffs, units = "days") - 91) <= 3)) {
    return("Q")
  }
  # Check for monthly data
  if (all(abs(as.numeric(date_diffs, units = "days") - 30) <= 3)) {
    return("M")
  }
  return(NA)
}

#' Get dataframe with most recent datapoint
#'
#' Takes list of dataframes and returns the one with the most recent date value
#'
#' @param datapoints list of dataframes with date column
#'
#' @return single dataframe with date column
#' @export
get_most_recent_dataframe <- function(datapoints) {
  if (length(datapoints) == 0) {
    stop("The list is empty.")
  }

  max_dates <- sapply(datapoints, function(df) {
    if ("date" %in% names(df)) {
      max(as.Date(df$date))
    } else {
      stop("One or more dataframes do not have a 'date' column.")
    }
  })
  # Check if any NA values are present (indicating missing 'date' column)
  if (any(is.na(max_dates))) {
    stop("One or more dataframes do not have a 'date' column.")
  }
  most_recent_index <- which.max(max_dates)

  datapoints[[most_recent_index]]
}

#' Get the interval of the most recent dataframe datapoint
#'
#' Takes list of dataframes with date column and returns A, Q or M
#'
#' @param datapoints list of dataframes with date column
#'
#' @return "A", "Q" or "M"
#' @export
#'

get_most_recent_interval <- function(datapoints){
  df <- get_most_recent_dataframe(datapoints)
  determine_interval(df)
}

#' Format date into quarterly label
#'
#' Q1-23 format
#'
#' @param date date
#'
#' @return character vector of quarterly labels
#' @export
#'
quarterly_label <- function(date){
 paste0("Q",  lubridate::quarter(date), "-",  format(date, format = "%y"))

}


#' Calculate smallest gap between labels
#'
#' takes x positions and labels and calculates the smallest gap in the series.
#' the unit is the width of the letter "m"
#'
#' @param x_positions vector of x positions
#' @param x_labels  character vector of x labels
#'
#' @return size of smallest gap in "m" width
#' @export
#'
calculate_smallest_gap <- function(x_positions, x_labels) {
  if (length(x_positions) != length(x_labels)) {
    stop("Length of x_positions and x_labels must be the same")
  }

  # Calculate the width of the letter "m" in user coordinates
  m_width <- graphics::strwidth("m", units = "user")

  # Calculate label widths in user coordinates
  label_widths <- sapply(x_labels, function(label) {
    graphics::strwidth(label, units = "user")
  })

  # Calculate gaps between labels
  gaps <- numeric(length = length(x_labels) - 1)
  for (i in seq_along(gaps)) {
    right_edge_prev_label <- x_positions[i] + label_widths[i] / 2
    left_edge_next_label <- x_positions[i + 1] - label_widths[i + 1] / 2
    gaps[i] <- (left_edge_next_label - right_edge_prev_label) / m_width
  }

  # Return smallest gap in terms of "m" width
  min_gap <- min(gaps)
  return(min_gap)
}

#' Filter NA labels and positions
#'
#' Taking the equal lenght label and position vector, removes the indices in
#' both vectors  where the labels are NA.
#'
#' @param x_positions vector of label positions
#' @param x_labels vector of labels
#'
#' @return list with both filtered vectors
#' @export
filter_na_labels <- function(x_positions, x_labels) {
  valid_indices <- !is.na(x_labels)
  x_positions <- x_positions[valid_indices]
  x_labels <- x_labels[valid_indices]
  mget(c("x_positions", "x_labels"))
}
