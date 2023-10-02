#' Get x limits from data and config
#'
#' takes the maximum range of all the series in the datapoints tables
#' and then cuts it down by the values of xmin and xmax passed in the
#' config dictionary. If the intervals don't overlap, uses just the
#' datapoint range, ignoring the user input.
#'
#' @param datapoints list of dataframes from \link[UMARvisualisR]{prep_data}
#' @param config config dictionary list from \link[UMARvisualisR]{prep_config}
#'
#' @return a vector with the xmin and xmax values in date format
#' @export
get_x_lims <- function(datapoints, config){
  # get max limits from data
  limits <- unlist(purrr::map(datapoints, \(x) range(x$date, na.rm = TRUE)))
  xmin <- as.Date(min(limits), origin = "1970-01-01")
  xmax <- as.Date(max(limits), origin = "1970-01-01")
  data_range <- lubridate::interval(as.Date(min(limits), origin = "1970-01-01"),
                      as.Date(max(limits), origin = "1970-01-01"))
  # cut them down by config args
  user_range <- lubridate::interval(as.Date(config$xmin), as.Date(config$xmax))
  if(length(lubridate::int_length(user_range))==0) {final_range <- data_range } else {
  final_range <- lubridate::intersect(data_range, user_range)}
  if(is.na(final_range)) {
    final_range <- data_range
    message("Tvoji xmin oz xmax datumi niso v podatkih, zato je prikazan celoten razpon.")
  }
  return(c(lubridate::int_start(final_range), lubridate::int_end(final_range)))
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
  if(grepl("ndeks", y_axis_label)) {
    abline(h = 100, col = umar_cols("emph"), lwd = 1.1)}
  if(in_range_strict(0, y_axis$ylim))  {
    abline(h = 0, col = umar_cols("emph"), lwd = 1.1)}
}


#' Helper funciton to get the number of lines for the top margin and title position
#'
#' A bit of an eclectic funciton, but there's no way around it. This funciton
#' creates a blind plot to get the user dimensions
#' of the final chart, which lets it wrap the title, which lets it get the number
#' of lines for the title, which along with the number of lines in the legend
#' and a few graphical parameters lets us know what the top margin has to be.
#' Also returns the position of the title in lines and the wrapped title.
#'
#' @param config  dictionary list from \link[UMARvisualisR]{prep_config}
#' @param title_ps title font size in points defaults 10
#' @param legend_ps legend font size in points, defaults to 9
#' @param legend_pad padding between plot and legend in lines, defaults to 0.2
#' @param pad padding above legend and above title in lines, defaults to 0
#'
#' @return top margin in lines, vertical title position in lines and wrapped title
#' @export
get_top_margin_and_title <- function(config,
                           title_ps = 10,
                           legend_ps = 9,
                           legend_pad = 0.2,
                           pad = 0){
  #blind plot
  plot.new()
  par(mar = c(3.1, 4.1, 4.1 , 0.2))
  plot.window(c(0,10), c(0,10))
  original_ps <- par("ps")
  print(original_ps)
  # get top margins
  legend_lines <- get_legend_lines(config$series, config$legend_columns)
  par("ps" = title_ps)
  title <- wrap_title(config$title, font = 2)
  title_lines <- title[[2]]
  wrapped_title <- title[[1]]

  lines <- legend_lines * legend_ps/original_ps + ifelse(legend_lines > 0, pad, 0) +
    title[[2]] * title_ps/original_ps + ifelse(legend_lines > 0, pad, 0) + legend_pad

  title_pos <- legend_lines * legend_ps/original_ps + ifelse(legend_lines > 0, pad, 0) + legend_pad
  return(list(lines, title_pos, wrapped_title))
}


publication_ready_plot <- function(datapoints, config){

  # params
  title_ps = 10
  legend_ps = 9
  legend_pad = 0.2


 # get limits
  x_lims <- get_x_lims(datapoints, config)

  if(config$dual_y){
    # split into left and right y-axis
    list2env(split_by_unit(datapoints, config), envir = .GlobalEnv)
  } else {config$y_axis_label <- config$series[[1]]$unit}

  y_axis <- find_pretty_ylim(unlist(purrr::map(datapoints, ~ .x$value)))

  # get top margins
  top <- get_top_margin_and_title(config,
                                  legend_pad = legend_pad,
                                  title_ps = title_ps,
                                  legend_ps = legend_ps)

  # empty plot
  par(mar = c(3.1, 4.1, top[[1]] , 0.2))
  empty_plot(x_lims, y_axis, config$y_axis_label)
  box()

  # legend placeholder for now.
  par("ps" = legend_ps)
  mtext("legend", side = 3,  line = legend_pad, adj = 0, padj = 0, family = "Myriad Pro")

  # position title
  par("ps" = title_ps)
  mtext(top[[3]], side = 3,  line = top[[2]], adj = 0, padj = 0, family = "Myriad Pro", font = 2)





  # y_label_max <- max(ylim)
  # if(unit == "EUR" & y_label_max > 1000000) y_label_max <- y_label_max/1000000
  # y_lab_lines <- strwidth(format(y_label_max, big.mark = ".", decimal.mark = ",",
  #                                scientific = FALSE),
  #                         units = "inches")/par("csi") + 1

  # # dims for top margin

  # par(mar = c(3, y_lab_lines + 1, half_legend + 0.3, 0.2),
  #     mgp=c(3,0.5,0), xpd = FALSE)
  # plot main line (and raw background if exists - but only for univariate)
  mapply(function(x, y) lines(x$period, x$value,
                              col = y, lwd = 2), data_points, umar_cols()[1:length(data_points)])

  # axis tickmarks
  axis.Date(1,at=seq(min(xlim), max(xlim), by="1 year"),
            col = umar_cols("gridlines"),
            lwd = 0, lwd.ticks =1.1, tck=-0.02, labels = FALSE)

  # shifting year labels by six months
  ss <- as.POSIXlt(min(xlim))
  ss$mon <- as.POSIXlt(min(xlim))$mon+6
  ee <- as.POSIXlt(max(xlim))
  ee$mon <- as.POSIXlt(max(xlim))$mon+6

  # rotate labels if you can
  x <- seq(ss, ee, by="1 year")
  x_labels_size <- max(strwidth(format(x, format = "%Y"), units = "user"))
  coord <- par("usr")
  x_tick_dist <- (coord[2]-coord[1]) /length(x)
  x_las <- ifelse(x_labels_size * 1.40 < x_tick_dist, 1, 2)
  if(x_las == 1) {mgp_2 <- -0.2}
  if(x_las == 2) {mgp_2 <- 0.3}
  suppressWarnings(par(mgp=c(3,mgp_2,0)))
  interval <- get_interval(data_points)
  if(interval == "A"){
    axis.Date(1,at=seq(min(xlim), max(xlim), by="1 year"),
              col = umar_cols("gridlines"),
              lwd = 0, tck = 0,  family ="Myriad Pro",
              las = x_las, padj = 0.5, format = "%Y")
  } else {
    axis.Date(1,at=seq(ss, ee, by="1 year"),
              col = umar_cols("gridlines"),
              lwd = 0, tck = 0,  family ="Myriad Pro",
              las = x_las, padj = 0.5, format = "%Y")
  }


  par(mgp=c(3,0.5,0))
  axis_labels <- y_breaks
  if(unit == "EUR" & max(axis_labels) > 1000000) {
    unit <- "Mio EUR"
    axis_labels_new <- axis_labels/1000000
    axis(2, at = axis_labels,
         labels = format(axis_labels_new, big.mark = ".", decimal.mark = ",",
                         scientific = FALSE),
         col = umar_cols("gridlines"), lwd = 0,  tck=0.0,
         las = 2,  family ="Myriad Pro")
  } else {
    axis(2, at = axis_labels,
         labels = format(axis_labels, big.mark = ".", decimal.mark = ",",
                         scientific = FALSE),
         col = umar_cols("gridlines"), lwd = 0,  tck=0.0,
         las = 2,  family ="Myriad Pro")}
  box(col = umar_cols("gridlines"), lwd = 1.1)

  # titles and labels

  par(ps=9)
  mtext(unit, side = 2,
        line = y_lab_lines+0.1, family ="Myriad Pro")

  op <- par(family = "Myriad Pro")
  if(length(data_points)> 1) {
    par(ps=9)
    legend_row_height <- xyinch(par("cin"))[[2]]
    left_legend_space <- xyinch(par("cin"), warn.log = FALSE)[[1]]
    net_legend_height <- half_legend * legend_row_height
    lh <- diff(grconvertY(0:1, 'inches', 'user')) * par('cin')[2] * par('cex') * par('lheight')
    short_legend_labels <- substr(legend_labels, 1, 45)
    shortened <- nchar(legend_labels)!= nchar(short_legend_labels)
    short_legend_labels <- paste(short_legend_labels,
                                 ifelse(shortened, "...", ""))

    legend(coord[[1]] + (coord[[2]]-coord[[1]]) *0 - left_legend_space, coord[[4]] ,
           short_legend_labels,
           lty = 1,
           lwd = 1.5,
           bg=NA,
           col = umar_cols()[1:length(legend_labels)],
           ncol = 2,
           cex = 1,
           bty = "n",
           inset=c(0, 0), xpd = TRUE,
           xjust = 0,
           yjust = 0,
           x.intersp = 0.5,
           y.intersp = 0.8)
    par(ps = 8)
    mtext("Vir: SURS", side = 1, line = 2 ,
          family ="Myriad Pro",  adj = 0)
  }
}

