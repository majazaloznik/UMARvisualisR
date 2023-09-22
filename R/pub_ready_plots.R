#' Get x limits from data and config
#'
#' takes the maximum range of all the series in the datapoints tables
#' and then cuts it down by the values of xmin and xmax passed in the
#' config dictionary.
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
  # cut them down by config args
  xmax <- min(xmax, as.Date(config$xmax), na.rm = TRUE)
  xmin <- max(xmin, as.Date(config$xmin), na.rm = TRUE)
  return(c(xmin, xmax))
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

write_title <- function(title, size){
  text_width <- strwidth(title, cex = 1,  family ="Myriad Pro")
  user_range <- par("usr")[2] - par("usr")[1]
}


publication_ready_plot <- function(datapoints, config){

  x_lims <- get_x_lims(datapoints, config)

  if(config$dual_y){
    # split into left and right y-axis
    list2env(split_by_unit(datapoints, config), envir = .GlobalEnv)
  } else {config$y_axis_label <- config$series[[1]]$unit}

  y_axis <- find_pretty_ylim(unlist(purrr::map(datapoints, ~ .x$value)))

  # y_label_max <- max(ylim)
  # if(unit == "EUR" & y_label_max > 1000000) y_label_max <- y_label_max/1000000
  # y_lab_lines <- strwidth(format(y_label_max, big.mark = ".", decimal.mark = ",",
  #                                scientific = FALSE),
  #                         units = "inches")/par("csi") + 1

  # # dims for top margin
  # par(ps=9)
  # half_legend <- ifelse(length(legend_labels)==1, 0, rounding(length(legend_labels)/2))

  # par(mar = c(3, y_lab_lines + 1, half_legend + 0.3, 0.2),
  #     mgp=c(3,0.5,0), xpd = FALSE)

  #plot
  empty_plot(x_lims, y_axis, config$y_axis_label)
axis(1)
axis(2)
box()

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

