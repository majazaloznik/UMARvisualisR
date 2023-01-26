#' Plotting function for univariate line chart
#'
#' Plots univariate line chart in line with the UMAR corporate graphical identity.
#' Data input must be prepared with the \link[UMARvisualisR]{prep_single_line}. At the
#' moment only plots years as labels.
#'
#' Todo: add arguments such as x-value range, add flexibility of labels for different
#' time ranges..
#'
#' @param prep_l list of length four with data.frame with data, the unit used and the
#' main and sub titles. see \link[UMARvisualisR]{prep_single_line}.
#' @param ... xmin or xmax to be passed to \link[UMARvisualisR]{apply_xlims}
#'
#' @return nothing, plots to open device
#' @export
#'
univariate_line_chart <- function(prep_l, ...){
  # prepare inputs
  single <- if(is.data.frame(prep_l$data_points))  prep_l$data_points else prep_l$data_points[[1]]
  unit <- prep_l$unit
  main_title <- prep_l$main_title
  sub_title <- prep_l$sub_title
  update_time <- prep_l$updated
  last_period <- prep_l$last_period
  title_lines <-main_title[[2]] + sub_title[[2]]

  par(mar = c(3, 3.5, title_lines + 1, 4))

  if(all(is.na(single$value))) {
    na_chart(prep_l) } else {
      single <- apply_xlims(single, ...)

      if(any(names(prep_l[[1]]) %in% "raw")) {
        ylim <- find_pretty_ylim(single$raw)
      } else {
      ylim <- find_pretty_ylim(single$value)}

      #plot
      plot(single$period, single$value, type = "n",
           xlab = "", ylab = "",
           bty = "n",
           axes = FALSE,
           main = "",
           cex.main = 1,
           family ="Myriad Pro",
           panel.first={grid(nx = NA, ny = NULL, col = umar_cols("gridlines"), lty = 1)},
           yaxs="i",
           ylim = ylim)

      # emphasised gridline
      if(unit %in% c("Indeks", "Index", "indeks", "index", "%", "Medletna rast, v %")) {
        abline(h = 100, col = umar_cols("emph"), lwd = 1.5)}
      if(in_range_strict(0, ylim))  {
        abline(h = 0, col = umar_cols("emph"), lwd = 1.5)}

      # plot main line (and raw background if exists)
      if(any(names(prep_l[[1]]) %in% "raw")) {
        lines(single$period, single$raw,
              col = umar_cols("siva"), lwd = 2)}
      lines(single$period, single$value,
            col = umar_cols("rdeca"), lwd = 2)

      # axis tickmarks
      axis.Date(1,at=seq(min(single$period), max(single$period), by="1 year"),
                col = umar_cols("gridlines"),
                lwd = 0, lwd.ticks =1, tck=-0.01, labels = FALSE)

      # shifting year labels by six months
      ss <- as.POSIXlt(min(single$period))
      ss$mon <- as.POSIXlt(min(single$period))$mon+6
      ee <- as.POSIXlt(max(single$period))
      ee$mon <- as.POSIXlt(max(single$period))$mon+6

      axis.Date(1,at=seq(ss, ee, by="1 year"),
                col = umar_cols("gridlines"),
                lwd = 0, tck = 0,  family ="Myriad Pro",
                las = 2, padj = 0.5, format = "%Y")

      axis(2, col = umar_cols("gridlines"),lwd = 0,  tck=0.0,
           las = 2,  family ="Myriad Pro")
      box(col = umar_cols("gridlines"), lwd = 2)

      # titles and labels
      mtext(main_title[[1]], side = 3, line = 0.5 + sub_title[[2]],
            family ="Myriad Pro", font = 2)
      mtext(sub_title[[1]], side = 3,
            line = 0.5, family ="Myriad Pro")
      mtext(unit, side = 2,
            line = 2.5, family ="Myriad Pro")
      mtext(paste("Posodobljeno:",
                  strftime(update_time[1,1],
                           format ="%d.%m.%y %H:%M:%S", tz = "CET")),
            side = 4,
            line = 0.5, family ="Myriad Pro")
      mtext(paste("Zadnje odbobje:",last_period), side = 4,
            line = 1.5, family ="Myriad Pro")
      if("transf_txt" %in% names(prep_l)) {
        mtext(prep_l[["transf_txt"]], side = 4,
              line = 2.5, family ="Myriad Pro", font = 3)}

    }
}



#' Plotting function for chart with no data values
#'
#' Plots error chart with message when all the data values in the chart are
#' missing. To be used as a temporary placeholder in reports where the selection
#' accidentally doens't include any data. Input data must be prepared by
#' \link[UMARvisualisR]{prep_single_line}. Called from  \link[UMARvisualisR]{univariate_line_chart}
#' if all values are NA.
#'
#' @param prep_l list of length four with data.frame with data, the unit used and the
#' main and sub titles. see \link[UMARvisualisR]{prep_single_line}.
#'
#' @return nothing, plots to open device
#' @export
#'

na_chart <- function(prep_l){
  single <- if(is.data.frame(prep_l$data_points))  prep_l$data_points else prep_l$data_points[[1]]
  unit <- prep_l$unit
  main_title <- prep_l$main_title
  sub_title <- prep_l$sub_title
  title_lines <-main_title[[2]] + sub_title[[2]]

  ylim <- c(0,1)

  #plot
  plot(single$period, rep(0.5, length(single$value)), type = "l",
       col = umar_cols("rdeca"),
       lwd = 2,
       xlab = "", ylab = "",
       bty = "n",
       axes = FALSE,
       main = "",
       cex.main = 1,
       family ="Myriad Pro",
       panel.first={grid(nx = NA, ny = NULL, col = umar_cols("gridlines"), lty = 1)},
       yaxs="i",
       ylim = ylim)

  # axis tickmarks
  axis.Date(1,at=seq(min(single$period), max(single$period), by="1 year"),
            col = umar_cols("gridlines"),
            lwd = 0, lwd.ticks =1, tck=-0.01, labels = FALSE)

  # shifting year labels by six months
  ss <- as.POSIXlt(min(single$period))
  ss$mon <- as.POSIXlt(min(single$period))$mon+6
  ee <- as.POSIXlt(max(single$period))
  ee$mon <- as.POSIXlt(max(single$period))$mon+6

  axis.Date(1,at=seq(ss, ee, by="1 year"),
            col = umar_cols("gridlines"),
            lwd = 0, tck = 0,  family ="Myriad Pro",
            las = 2, padj = 0.5,  format = "%Y")

  box(col = umar_cols("gridlines"), lwd = 2)

  # titles and labels
  mtext(main_title[[1]], side = 3, line = 0.5 + sub_title[[2]],
        family ="Myriad Pro", font = 2)
  mtext(sub_title[[1]], side = 3,
        line = 0.5, family ="Myriad Pro")
  mtext("Podatki ne obstajajo", side = 3, cex=2, font = 3,
        line = -5.5, family ="Myriad Pro", col = umar_cols("rdeca"))
  mtext("(cela serija je za izbrano obdobje prazna)", side = 3, cex=1, font = 3,
        line = -6.5, family ="Myriad Pro", col = umar_cols("rdeca"))
}


#' Framework for plotting a multivariate line chart
#'
#' Plots univariate and multivariate line chart in line with the UMAR corporate
#' graphical identity.
#' Data input must be prepared with the \link[UMARvisualisR]{prep_multi_line} function.
#' At the moment only plots years as labels.
#'
#' Todo: add arguments such as x-value range, add flexibility of axis labels for different
#' time ranges..
#'
#' @param prep_l list of length 8+ with data.frame with data, the unit used and the
#' main and sub titles etc.. see \link[UMARvisualisR]{prep_multi_line}.
#' @param xmin to be passed to \link[UMARvisualisR]{apply_xlims}
#' @param xmax to be passed to \link[UMARvisualisR]{apply_xlims}
#'
#' @return nothing, plots to open device
#' @export
multivariate_line_chart <- function(prep_l, xmin = "2011-01-01", xmax =NULL){
  # prepare inputs
  data_points <- prep_l$data_points
  unit <- prep_l$unit
  main_title <- prep_l$main_title
  sub_title <- prep_l$sub_title
  update_time <- prep_l$updated
  last_period <- prep_l$last_period
  legend_labels <- prep_l$legend_labels

  # dims for top margin
  par(ps=10)
  half_legend <- ifelse(length(legend_labels)==1, 0, rounding(length(legend_labels)/2))
  title_lines <- main_title[[2]] + sub_title[[2]] + half_legend
  par(mar = c(3, 3.5, title_lines + 1, 4),
      mgp=c(3,0.5,0), xpd = FALSE)

  if(all(is.na(unlist(lapply(data_points, function(x) c(x$value)))))) {
    na_chart(prep_l)
    } else {
      data_points <- lapply(data_points, function(x) apply_xlims(x, xmin, xmax))
      xlim <- c(sapply(data_points, function(x) min(x$period, na.rm = TRUE), simplify = FALSE)[[1]],
                sapply(data_points, function(x) max(x$period, na.rm = TRUE), simplify = FALSE)[[1]])

      if(any(sapply(data_points, names) %in% "raw")) {
        ylim <-find_pretty_ylim(unlist(lapply(data_points, function(x) c(x$raw))))
      } else {
        ylim <-find_pretty_ylim(unlist(lapply(data_points, function(x) c(x$value))))}

      #plot
      plot(xlim[1], ylim[1],type = "n",
           xlab = "", ylab = "",
           bty = "n",
           axes = FALSE,
           main = "",
           cex.main = 1,
           family ="Myriad Pro",
           panel.first={grid(nx = NA, ny = NULL, col = umar_cols("gridlines"), lty = 1)},
           yaxs="i",
           ylim = ylim,
           xlim = xlim)
      # emphasised gridline
      if(unit %in% c("Indeks", "Index", "indeks", "index", "%", "Medletna rast, v %")) {
        abline(h = 100, col = umar_cols("emph"), lwd = 1)}
      if(in_range_strict(0, ylim))  {
        abline(h = 0, col = umar_cols("emph"), lwd = 1)}

      # plot main line (and raw background if exists - but only for univariate)
      if(any(sapply(data_points, names) %in% "raw") & length(data_points) ==1) {
        lines(data_points[[1]]$period, data_points[[1]]$raw,
              col = umar_cols("siva"), lwd = 2)}
      mapply(function(x, y) lines(x$period, x$value,
                                  col = y, lwd = 2), data_points, umar_cols()[1:length(data_points)])

      # axis tickmarks
      axis.Date(1,at=seq(min(xlim), max(xlim), by="1 year"),
                col = umar_cols("gridlines"),
                lwd = 0, lwd.ticks =1, tck=-0.02, labels = FALSE)

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
      axis.Date(1,at=seq(ss, ee, by="1 year"),
                col = umar_cols("gridlines"),
                lwd = 0, tck = 0,  family ="Myriad Pro",
                las = x_las, padj = 0.5, format = "%Y")
      par(mgp=c(3,0.5,0))

      axis(2, col = umar_cols("gridlines"),lwd = 0,  tck=0.0,
           las = 2,  family ="Myriad Pro")
      box(col = umar_cols("gridlines"), lwd = 1)

      # titles and labels

      par(ps=10)
      mtext(sub_title[[1]], side = 3,
            line = 0.5, family ="Myriad Pro", adj = 0)
      mtext(unit, side = 2,
            line = 2.5, family ="Myriad Pro")
      mtext(paste("Posodobljeno:",
                  strftime(update_time,
                           format ="%d.%m.%y %H:%M:%S", tz = "CET")),
            side = 4,
            line = 0.5, family ="Myriad Pro", cex = 1)
      mtext(paste("Zadnje odbobje:",last_period), side = 4,
            line = 1.5, family ="Myriad Pro", cex = 1)
      if("transf_txt" %in% names(prep_l)) {
        mtext(prep_l[["transf_txt"]], side = 4,
              line = 2.5, family ="Myriad Pro", font = 3, cex = 1)}
      op <- par(family = "Myriad Pro")
      if(length(data_points)> 1) {
        par(ps=10)
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
               lwd = 2,
               bg=NA,
               col = umar_cols()[1:length(legend_labels)],
               ncol = 2,
               cex = 1,
               bty = "n",
               inset=c(0, 0), xpd = TRUE,
               xjust = 0,
               yjust = 0,
               x.intersp = 0.5)
        mtext(main_title[[1]], side = 3, line = 1 + half_legend,
              family ="Myriad Pro", font = 2, adj = 0, cex = 1.05)
      } else {
        mtext(main_title[[1]], side = 3, line = 1 + sub_title[[2]],
              family ="Myriad Pro", font = 2, adj = 0, cex = 1.05)
      }
    }
}
