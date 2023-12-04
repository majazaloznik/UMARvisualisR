#' Cut datapoints to x limits from config
#'
#' takes the maximum range of all the series in the datapoints tables
#' and then cuts it down by the values of xmin and xmax passed in the
#' config dictionary. If the intervals don't overlap, uses just the
#' datapoint range, ignoring the user input.
#'
#' Returns filtered datapoints.
#'
#' @param datapoints list of dataframes from \link[UMARvisualisR]{prep_data}
#' @param config config dictionary list from \link[UMARvisualisR]{prep_config}
#'
#' @return a list with the xmin and xmax values of the data in date format and the filtered datapoints
#' @export
cut_to_x_range <- function(datapoints, config){
  # get max limits from data
  limits <- unlist(purrr::map(datapoints, \(x) range(x$date, na.rm = TRUE)))
  xmin <- as.Date(min(limits), origin = "1970-01-01")
  xmax <- as.Date(max(limits), origin = "1970-01-01")
  data_range <- lubridate::interval(as.Date(min(limits), origin = "1970-01-01"),
                      as.Date(max(limits), origin = "1970-01-01"))
  # cut them down by config args
  user_range <- lubridate::interval(as.Date(config$xmin), as.Date(config$xmax))
  # Check if config$xmin and config$xmax are provided
  xmin_provided <- !is.null(config$xmin) && !identical(config$xmin, "")
  xmax_provided <- !is.null(config$xmax) && !identical(config$xmax, "")

  if (xmin_provided && xmax_provided) {
    user_range <- lubridate::interval(as.Date(config$xmin), as.Date(config$xmax))
    final_range <- lubridate::intersect(data_range, user_range)
  } else if (xmin_provided) {
    final_range <- lubridate::interval(as.Date(config$xmin), lubridate::int_end(data_range))
  } else if (xmax_provided) {
    final_range <- lubridate::interval(lubridate::int_start(data_range), as.Date(config$xmax))
  } else {
    final_range <- data_range
  }
  if(is.na(final_range)) {
    final_range <- data_range
    message("Tvoji xmin oz xmax datumi niso v podatkih, zato je prikazan celoten razpon.")
  }
  datapoints <- datapoints |>
    purrr::map(~dplyr::filter(.x, date >= lubridate::int_start(final_range) &
                         date <= lubridate::int_end(final_range)))

  return(datapoints)
}

#' Get x limits from the data
#'
#' Used after the data has been cut to user xlims, this returns the
#' actual first and last date in the datapoints
#'
#' @param datapoints list of dataframes from \link[UMARvisualisR]{prep_data}
#'
#' @return
#' @export
#'
get_x_lims <- function(datapoints){
  all_dates <- purrr::map(datapoints, ~ .x$date) |>  unlist()
  c(min_date = as.Date(min(all_dates), origin = "1970-01-01"),
    max_date = as.Date(max(all_dates), origin = "1970-01-01"))
}

#' Plots empty plot with gridlines
#'
#' Plots an empty plot with the gridlines, emphasised on 100 for indices a
#' and zero if in range.
#'
#' @param x_lims output of \link[UMARvisualisR]{get_x_lims}
#' @param y_axis output of \link[UMARvisualisR]{find_pretty_ylim}
#' @param y_axis_label whatever is in config$y_axis_label - required to
#' check if the unit is an index, so the 100 gridline can be emphasised.
#'
#' @return nothing, plots to open device
#' @export
empty_plot <- function(x_lims, y_axis, y_axis_label){
  plot(x_lims[1], y_axis$ylim[1],type = "n",
       xlab = "", ylab = "",
       bty = "n",
       axes = FALSE,
       main = "",
       cex.main = 1,
       family ="Myriad Pro",
       panel.first={grid(nx = NA, ny = length(y_axis$y_breaks) - 1,
                         col = umar_cols("gridlines"), lty = 1, lwd = 1.1)},
       yaxs="i",
       ylim = y_axis$ylim,
       xlim = x_lims)
  # emphasised gridline
  if(grepl("nde(ks|x)", y_axis_label)) {
    abline(h = 100, col = umar_cols("emph"), lwd = 1.1)}
  if(in_range_strict(0, y_axis$ylim))  {
    abline(h = 0, col = umar_cols("emph"), lwd = 1.1)}
  box(col = umar_cols("gridlines"), lwd = 1.1)
}



#' Base barplot
#'
#' Can be used as base for combo of bar and line plot as well.
#' All the info is in the datapoints and config.
#'
#' @param datapoints list of dataframes from \link[UMARvisualisR]{prep_data}
#' @param config config dictionary list from \link[UMARvisualisR]{prep_config}
#' @param y_axis output of \link[UMARvisualisR]{find_pretty_ylim}
#'
#' @return midpoints, of course also plots a barplot to open device
#' @export
#'
base_barplot <- function(datapoints, config, y_axis){
  series_types <- vapply(config$series, \(x) x$type, character(1))
  series_colours <- vapply(config$series, \(x) x$colour, character(1))
  bar_colours <- series_colours[series_types == "bar"]
  bar_datapoints <- datapoints[series_types == "bar"]
  bar_datapoints <- purrr::reduce(bar_datapoints, ~dplyr::full_join(.x, .y, by = "date")) |>
    dplyr::arrange(date)
  dates <- bar_datapoints$date
  if (config$stacked) beside <- FALSE else
    beside <- TRUE

  bar_datapoints <- t(as.matrix(bar_datapoints[,-1]))
  bar_datapoints[is.na(bar_datapoints)] <- 0
  if(beside) spacing <- c(0.1, 0.33) else
    spacing <- 0.33
  # plot emptty barplot
  midpoints <- barplot(bar_datapoints, beside = beside,
                       axes = FALSE, ylim = y_axis$ylim, border = NA, col = NA,
                       panel.first={grid(nx = NA, ny = length(y_axis$y_breaks) - 1,
                                         col = umar_cols("gridlines"), lty = 1, lwd = 1.1)},
                       space = spacing, xpd = FALSE)
  if(is.matrix(midpoints)) midpoints <- colMeans(midpoints)
  # emphasised gridline
  if(grepl("ndeks", config$y_axis_label)) {
    abline(h = 100, col = umar_cols("emph"), lwd = 1.1)}
  if(in_range_strict(0, y_axis$ylim))  {
    abline(h = 0, col = umar_cols("emph"), lwd = 1.1)}

  # plot over gridlines
  my_special_barplot(bar_datapoints, beside = beside,
          axes = FALSE, ylim = y_axis$ylim, col = bar_colours, border = NA,
          space = spacing, xpd = FALSE, add = TRUE)
  box(col = umar_cols("gridlines"), lwd = 1.1)

  mget(c("midpoints", "dates"))
}

#' Helper funciton to get the number of lines for the top margin and title position
#'
#' A bit of an eclectic function, but there's no way around it. This funciton
#' creates a blind plot to get the user dimensions
#' of the final chart, which lets it wrap the title, which lets it get the number
#' of lines for the title, which along with the number of lines in the legend
#' and a few graphical parameters lets us know what the top margin has to be.
#' Also returns the position of the title in lines and the wrapped title.
#'
#' @param config  dictionary list from \link[UMARvisualisR]{prep_config}
#' @param title_ps title font size in points defaults 10
#'
#' @return top margin in lines, vertical title position in lines and wrapped title
#' @export
get_top_margin_and_title <- function(config, title_ps){
  gap <- 0.15
  #blind plot
  plot.new()
  mar <- par("mar")
  mar[1] <- 1.6
  mar[4] <- 0.7
  par(mar = mar)
  plot.window(c(0,10), c(0,10))

    # get top margins
  legend_lines <- get_legend_lines(config$series, config$legend_columns)
  par("ps" = title_ps)
  title <- wrap_title(config$title, font = 2)
  title_lines <- title[[2]]
  wrapped_title <- title[[1]]

  lines <- (legend_lines + 0.3) * 0.7 - 0.3 + gap +
    title_lines * title_ps/12

  title_pos <- (legend_lines + 0.3) * 0.7 - 0.3 + gap
  current_mar <- par("mar")
  current_mar[3] <- lines
  par(mar =  current_mar)
  return(list(lines, title_pos, wrapped_title))
}


#' Create legend
#'
#' does what it says on the box based on the config data
#'
#' @param config  dictionary list from \link[UMARvisualisR]{prep_config}
#' @param legend_ps legend text size
#' @param language either "si" or "en"
#'
#' @return nothing, just draws the legend
#' @export
#'
create_legend <- function(config, legend_ps, language = "si") {
  par("ps" = legend_ps)
  par(family = "Myriad Pro")
  lwd <- lty <- series_types <- vapply(config$series, \(x) x$type, character(1))
  line_colours <- bar_colours <- series_colours <- vapply(config$series, \(x) x$colour, character(1))
  if(language == "si"){ # switch languages
  legend_labels <- vapply(config$series, \(x) x$legend_txt_si, character(1))} else {
    legend_labels <- vapply(config$series, \(x) x$legend_txt_en, character(1))}
  lty[series_types == "line"] <- 1
  lty[series_types != "line"] <- NA
  lwd[series_types == "line"] <- 2
  lwd[series_types != "line"] <- NA
  line_colours[series_types != "line"] <- NA
  bar_colours[series_types == "line"] <- NA
  legend_mz2(par("usr")[[1]] , par("usr")[[4]] ,
             legend_labels,
             lty = as.numeric(lty),
             lwd = lwd,
             col = line_colours,
             fill = bar_colours,
             ncol = config$legend_columns,
             xjust = 0,
             yjust = 0,
             x.intersp = 0.2,
             y.intersp = 0.7)
}

#' Get x_axis lims and tickmarks
#'
#' @param datapoints list of dataframes from \link[UMARvisualisR]{prep_data}
#' @param config  dictionary list from \link[UMARvisualisR]{prep_config}
#'
#' @return named list of tickmarks and x_lims
#' @export
#'
x_axis_lims_tickmarks <- function(datapoints, config) {
  # get x lims
  x_lims <- get_x_lims(datapoints)

  last_year_complete_log <- last_year_complete(datapoints)
  only_annual_log <- only_annual_intervals(datapoints)

  # annual labels
  if(!config$x_sub_annual){

    # axis tickmarks
    if(only_annual_log){
      tickmarks <- seq(x_lims[[1]], x_lims[[2]], by="1 year")
    } else if(last_year_complete_log) { # non annual data but complete last year
      tickmarks <- seq(first_day_of_year(x_lims[[1]]),
                       last_day_of_year(x_lims[[2]]) + 1 , by="1 year")
    } else { # non annual data and incomplete years
      tickmarks <- seq(first_day_of_year(x_lims[[1]]),
                       first_day_of_year(x_lims[[2]]), by="1 year")
      tickmarks <- c(tickmarks, x_lims[[2]])
    }

    # axis limits
    x_lims <- c(min(tickmarks), max(tickmarks))

    mget(c("tickmarks", "x_lims"))
  }
}
#' Get x axis parameters
#'
#' Takes the datapoints and config (for user xmin and xmax mainly) and
#' calculates the ticmkark positions, the label positions, the default
#' labels (not handling overlapping just yet) and the xlims for the x axis.
#'
#' Taking into account if the data is only annual - in which case the tickmarks
#' and lables align - or monthly/quarterly in which case the labels are between.
#' Taking into account if the last year is complete or not, which determines
#' the last tickmark and label etc.
#'
#' @param datapoints list of dataframes from \link[UMARvisualisR]{prep_data}
#' @param config  dictionary list from \link[UMARvisualisR]{prep_config}
#' @param tickmarks vector from from \link[UMARvisualisR]{x_axis_lims_tickmarks}
#' @param x_lims vector from from \link[UMARvisualisR]{x_axis_lims_tickmarks}
#' @param language "si" or "en"
#'
#' @return list of named x_positions, x_labels, tickmars and x_lims
#' @export
#'
x_axis_label_params <- function(datapoints, config, tickmarks, x_lims, language = "si") {

  last_year_complete_log <- last_year_complete(datapoints)
  only_annual_log <- only_annual_intervals(datapoints)
  old_locale <- Sys.getlocale("LC_TIME")
  if (language == "si")  Sys.setlocale("LC_TIME", "Slovenian_Slovenia.1250")
  if (language == "en")  Sys.setlocale("LC_TIME", "English_United Kingdom.1252")

  # annual labels
  if(!config$x_sub_annual){


    # x_positions
    if(only_annual_log){
      x_positions <- tickmarks
    } else if(last_year_complete_log) { # non annual data but complete last year
      x_positions <- shift_dates_by_six_months(tickmarks)[-length(tickmarks)]
    } else { # non annual data and incomplete years
      x_positions <- shift_dates_by_six_months(tickmarks)[1:(length(tickmarks) - 2)]
      x_positions <- c(x_positions, tickmarks[length(tickmarks)])
    }

    # labels
    if(only_annual_log){
      x_labels <- format(tickmarks, format = "%Y")
    } else if(last_year_complete_log) { # non annual data but complete last year
      x_labels <- format(tickmarks, format = "%Y")[-length(tickmarks)]
    } else { # non annual data and incomplete years
      x_labels <- format(tickmarks, format = "%Y")[1:(length(tickmarks) - 2)]
      int <- get_most_recent_interval(datapoints)
      if(int == "M"){
        x_labels <- c(x_labels, format(tickmarks[length(tickmarks)], format = "%b%y"))}
      if(int == "Q"){
        x_labels <- c(x_labels, quarterly_label(tickmarks[length(tickmarks)]))}
    }
    min_gap <- calculate_smallest_gap(x_positions, x_labels)

    if(only_annual_log | last_year_complete_log) { # annual labels only
      if (min_gap < 0.30) { # squish once
        x_labels <- year_squisher(x_labels)
        min_gap <- calculate_smallest_gap(x_positions, x_labels)
        if (min_gap < 0.30) { # squish only last
          x_labels[(length(x_labels)-1)] <- NA
          filtered <- filter_na_labels(x_positions, x_labels)
          x_positions <- filtered$x_positions
          x_labels <- filtered$x_labels
          min_gap <- calculate_smallest_gap(x_positions, x_labels)
          if (min_gap < 0.30) { # squish again
            x_labels <- year_squisher(x_labels, extra = TRUE)
            filtered <- filter_na_labels(x_positions, x_labels)
            x_positions <- filtered$x_positions
            x_labels <- filtered$x_labels
            min_gap <- calculate_smallest_gap(x_positions, x_labels)
            if (min_gap < 0.30) { # squish only last
              x_labels[(length(x_labels)-1)] <- NA
              filtered <- filter_na_labels(x_positions, x_labels)
              x_positions <- filtered$x_positions
              x_labels <- filtered$x_labels
            }
          }
        }
      }
    }
    else { # non annual data and incomplete years
      if (min_gap < 0.30) { # squish once
        x_labels <- c(year_squisher(x_labels[-length(x_labels)]),
                      x_labels[length(x_labels)])
        min_gap <- calculate_smallest_gap(x_positions, x_labels)
        if (min_gap < 0.30) { # squish only last
          x_labels[(length(x_labels)-1)] <- NA
          filtered <- filter_na_labels(x_positions, x_labels)
          x_positions <- filtered$x_positions
          x_labels <- filtered$x_labels
          min_gap <- calculate_smallest_gap(x_positions, x_labels)
          if (min_gap < 0.30) { # squish again
            x_labels <- year_squisher(x_labels, extra = TRUE)
            filtered <- filter_na_labels(x_positions, x_labels)
            x_positions <- filtered$x_positions
            x_labels <- filtered$x_labels
            min_gap <- calculate_smallest_gap(x_positions, x_labels)
            if (min_gap < 0.30) { # squish only last
              x_labels[(length(x_labels)-1)] <- NA
              filtered <- filter_na_labels(x_positions, x_labels)
              x_positions <- filtered$x_positions
              x_labels <- filtered$x_labels
            }
          }
        }
      }
    }



  } else { # sub annual labels - not implemented yet
    print("Grafi z oznakami na x-osi za mesece oz. kvartale \u0161e niso implementirani.")
  }
  Sys.setlocale("LC_TIME", old_locale)
  mget(c( "x_positions", "x_labels", "tickmarks", "x_lims"))
}


#' Umbrella function for publicaiton ready chart
#'
#'
#' @param datapoints list of dataframes from \link[UMARvisualisR]{prep_data}
#' @param config config dictionary list from \link[UMARvisualisR]{prep_config}
#' @param language "si" or "en"
#'
#'
#' @return
#' @export
#'
publication_ready_plot <- function(datapoints, config, language = "si"){

  # params
  title_ps = 9
  legend_ps = 8
  shapes <- vapply(config$series, \(x) x$type, character(1))
  if (any(shapes == "bar"))  bar <- TRUE else  bar <- FALSE
  if (any(shapes == "line")) line <- TRUE else  line <- FALSE

 # cut data to x limits
  datapoints <- cut_to_x_range(datapoints, config)

  x_axis <- x_axis_lims_tickmarks(datapoints, config)

  if(config$dual_y){
    # split into left and right y-axis
    list2env(split_by_unit(datapoints, config), envir = .GlobalEnv)
  }

  if(language == "en") config <- prep_config_en(config)

  values <- get_data_values(datapoints, config)
  y_axis <- find_pretty_ylim(values)

  # get left margins
  left <- left_axis_label_width(config, y_axis)
  if(language == "en" & is.null(config$y_axis_label)){
    config$y_axis_label <- unit_lookup |>
      dplyr::filter(name == tolower(left$unit)) |>
      dplyr::pull(name_en)}
  if (is.null(config$y_axis_label)) config$y_axis_label <- left$unit

  # get top margins
  top <- get_top_margin_and_title(config, title_ps = title_ps)

  if(!bar){
    # empty plot
    empty_plot(x_axis$x_lims, y_axis, left$unit)
    # draw lines
    draw_lines(datapoints, config)}
  if(bar)
    # barplot
    x_values <- base_barplot(datapoints, config, y_axis)
  if(bar & line)
    draw_lines(datapoints, config, x_values = x_values)

  # get x axis tickmark positions and label positions and limits
  x_axis <- x_axis_label_params(datapoints, config, x_axis$tickmarks, x_axis$x_lims, language)

  # legend
  create_legend(config, legend_ps = legend_ps, language = language)

  # position title
  par("ps" = title_ps)
  mtext(top[[3]], side = 3,  line = top[[2]], adj = 0, padj = 0, family = "Myriad Pro", font = 2)

  left_axis_labels(config$y_axis_label, left$axis_positions, left$axis_labels, left$y_lab_lines)

  # # axis tickmarks
  axis.Date(1,
            at = x_axis$tickmarks,
            col = umar_cols("gridlines"),
            lwd = 0, lwd.ticks =1, tck=-0.02, labels = FALSE)

  par_mgp(mgp=c(3,-0.2,0))
  axis(1, x_axis$x_labels,
       at = x_axis$x_positions,
       col = umar_cols("gridlines"),
       lwd = 0, tck = 0,  family ="Myriad Pro",
       padj = 0.5, gap.axis = 0.25)


}

#' Draw lines onto plot
#'
#' This works for either plotting only lines on a preexisting empty plot
#' from \link[UMARvisualisR]{empty_plot}, or plotting on top of existing
#' barplot from \link[UMARvisualisR]{base_barplot} in which case you
#' have to pass it the bar midpoints as x_values
#'
#' @param datapoints list of dataframes from \link[UMARvisualisR]{prep_data}
#' @param config config dictionary list from \link[UMARvisualisR]{prep_config}
#' @param x_values vector of midpoints from \link[UMARvisualisR]{base_barplot}
#'
#' @return nothing, just draws lines
#' @export
#'
draw_lines <- function(datapoints, config, x_values = NULL){
 if(!missing(x_values)){
   series_types <- vapply(config$series, \(x) x$type, character(1))
   series_colours <- vapply(config$series, \(x) x$colour, character(1))
   line_colours <- series_colours[series_types == "line"]
   line_datapoints <- datapoints[series_types == "line"]
   line_datapoints <- purrr::reduce(line_datapoints, ~dplyr::full_join(.x, .y, by = "date")) |>
     dplyr::arrange(date)
   numeric_cols <- line_datapoints[, sapply(line_datapoints, is.numeric)]

   x_values <- interpolate_x(x_values$dates, x_values$midpoints, line_datapoints$date)
   for (i in 1:ncol(numeric_cols)) {
     y_values <- dplyr::pull(numeric_cols[,i])
     lines(x_values[!is.na(y_values)], na.omit(y_values),
           col=line_colours[i], type="l", lwd = 2)
   }
 } else {# means there are only only lines
   line_colours <- vapply(config$series, \(x) x$colour, character(1))
   for (i in 1:length(datapoints)) {
     lines(datapoints[[i]]$date, datapoints[[i]]$value ,
           col=line_colours[i], type="l", lwd = 2)
   }
 }
}


interpolate_x <- function(original_dates, x_values, new_dates) {
  # Convert dates to numeric (number of days since the first date)
  original_days <- as.numeric(difftime(original_dates, original_dates[1], units = "days"))
  new_days <- as.numeric(difftime(new_dates, original_dates[1], units = "days"))

  # Using the linear interpolation formula:
  x1 <- original_days[1]
  x2 <- original_days[length(original_days)]
  y1 <- x_values[1]
  y2 <- x_values[length(x_values)]

  interpolated_x_values <- y1 + (new_days - x1) * (y2 - y1) / (x2 - x1)

  return(interpolated_x_values)
}

#
#
#
#   # plot main line (and raw background if exists - but only for univariate)
#
#   mapply(function(x, y) lines(x$period, x$value,
#                               col = y, lwd = 2), data_points, umar_cols()[1:length(data_points)])
#
#   # axis tickmarks
#   axis.Date(1,at=seq(min(xlim), max(xlim), by="1 year"),
#             col = umar_cols("gridlines"),
#             lwd = 0, lwd.ticks =1.1, tck=-0.02, labels = FALSE)
#
#   # shifting year labels by six months
#   ss <- as.POSIXlt(min(xlim))
#   ss$mon <- as.POSIXlt(min(xlim))$mon+6
#   ee <- as.POSIXlt(max(xlim))
#   ee$mon <- as.POSIXlt(max(xlim))$mon+6
#
#   # rotate labels if you can
#   x <- seq(ss, ee, by="1 year")
#   x_labels_size <- max(strwidth(format(x, format = "%Y"), units = "user"))
#   coord <- par("usr")
#   x_tick_dist <- (coord[2]-coord[1]) /length(x)
#   x_las <- ifelse(x_labels_size * 1.40 < x_tick_dist, 1, 2)
#   if(x_las == 1) {mgp_2 <- -0.2}
#   if(x_las == 2) {mgp_2 <- 0.3}
#   suppressWarnings(par(mgp=c(3,mgp_2,0)))
#   interval <- get_interval(data_points)
#   if(interval == "A"){
#     axis.Date(1,at=seq(min(xlim), max(xlim), by="1 year"),
#               col = umar_cols("gridlines"),
#               lwd = 0, tck = 0,  family ="Myriad Pro",
#               las = x_las, padj = 0.5, format = "%Y")
#   } else {
#     axis.Date(1,at=seq(ss, ee, by="1 year"),
#               col = umar_cols("gridlines"),
#               lwd = 0, tck = 0,  family ="Myriad Pro",
#               las = x_las, padj = 0.5, format = "%Y")
#   }
#
#
#
#
#   # titles and labels
#
#   par(ps=9)
#   mtext(unit, side = 2,
#         line = y_lab_lines+0.1, family ="Myriad Pro")
#
#   op <- par(family = "Myriad Pro")
#   if(length(data_points)> 1) {
#     par(ps=9)
#     legend_row_height <- xyinch(par("cin"))[[2]]
#     left_legend_space <- xyinch(par("cin"), warn.log = FALSE)[[1]]
#     net_legend_height <- half_legend * legend_row_height
#     lh <- diff(grconvertY(0:1, 'inches', 'user')) * par('cin')[2] * par('cex') * par('lheight')
#     short_legend_labels <- substr(legend_labels, 1, 45)
#     shortened <- nchar(legend_labels)!= nchar(short_legend_labels)
#     short_legend_labels <- paste(short_legend_labels,
#                                  ifelse(shortened, "...", ""))
#
#     legend(coord[[1]] + (coord[[2]]-coord[[1]]) *0 - left_legend_space, coord[[4]] ,
#            short_legend_labels,
#            lty = 1,
#            lwd = 1.5,
#            bg=NA,
#            col = umar_cols()[1:length(legend_labels)],
#            ncol = 2,
#            cex = 1,
#            bty = "n",
#            inset=c(0, 0), xpd = TRUE,
#            xjust = 0,
#            yjust = 0,
#            x.intersp = 0.5,
#            y.intersp = 0.8)
#     par(ps = 8)
#     mtext("Vir: SURS", side = 1, line = 2 ,
#           family ="Myriad Pro",  adj = 0)
#   }
# }
#


