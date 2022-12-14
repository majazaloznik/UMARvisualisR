

#' Plotting function for univariate line chart
#'
#' Plots univariate line chart in line with the UMAR corporate graphical identity.
#' Data input must be prepared with the \link[UMARaccessR]{prep_single_line}. At the
#' moment only plots years as labels.
#'
#' Todo: add arguments such as x-value range, add flexibility of labels for different
#' time ranges..
#'
#' @param prep_l list of length four with data.frame with data, the unit used and the
#' main and sub titles. see \link[UMARaccessR]{prep_single_line}.
#' @param ... xmin or xmax to be passed to \link[UMARvisualisR]{apply_xlims}
#'
#' @return nothing, plots to open device
#' @export
#'
univariate_line_chart <- function(prep_l, ...){
  # prepare inputs
  single <- prep_l[[1]]
  single <- apply_xlims(single, ...)
  unit <- prep_l[[2]]
  main_title <- prep_l[[3]]
  sub_title <- prep_l[[4]]
  update_time <- prep_l[[5]]
  last_period <- prep_l[[6]]
  title_lines <-main_title[[2]] + sub_title[[2]]

  par(mar = c(3, 3.5, title_lines + 1, 4))

  if(all(is.na(single$value))) {
    na_chart(prep_l) } else {
      # prepare plot area and limits
      #par(mar = c(3, 3.5, title_lines + 1, 3))
      ylim <- find_pretty_ylim(single$value)

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

      if(any(names(prep_l[[1]]) %in% "rolling")) {
        lines(single$period, single$value,
              col = umar_cols("siva"), lwd = 2)
        lines(single$period, single$rolling,
              col = umar_cols("rdeca"), lwd = 2)} else {
      lines(single$period, single$value,
            col = umar_cols("rdeca"), lwd = 2)}


      # axis tickmarks
      axis.Date(1,at=seq(min(single$period), max(single$period), by="1 year"),
                col = umar_cols("gridlines"),
                lwd = 0, lwd.ticks =2, tck=-0.01, labels = FALSE)

      # shifting year labels by six months
      ss <- as.POSIXlt(min(single$period))
      ss$mon <- as.POSIXlt(min(single$period))$mon+6
      ee <- as.POSIXlt(max(single$period))
      ee$mon <- as.POSIXlt(max(single$period))$mon+6

      axis.Date(1,at=seq(ss, ee, by="1 year"),
                col = umar_cols("gridlines"),
                lwd = 0, tck = 0,  family ="Myriad Pro",
                las = 2, padj = 0.5)

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
                           format ="%m.%d.%y %H:%M:%S", tz = "CET")),
            side = 4,
            line = 0.5, family ="Myriad Pro")
      mtext(paste("Zadnje odbobje:",last_period), side = 4,
            line = 1.5, family ="Myriad Pro")
      if("transf_txt" %in% names(prep_l)) {
        mtext(prep_l[["transf_txt"]], side = 4,
              line = 2.5, family ="Myriad Pro")}
              # emphasised gridline
      if(unit %in% c("Indeks", "Index", "indeks", "index", "%")) {
        abline(h = 100, col = umar_cols("emph"), lwd = 1.5)}
      if(in_range_strict(0, ylim))  {
        abline(h = 0, col = umar_cols("emph"), lwd = 1.5)}
    }
}



#' Plotting function for chart with no data values
#'
#' Plots error chart with message when all the data values in the chart are
#' missing. To be used as a temporary placeholder in reports where the selection
#' accidentally doens't include any data. Input data must be prepared by
#' \link[UMARaccessR]{prep_single_line}. Called from  \link[UMARvisualisR]{univariate_line_chart}
#' if all values are NA.
#'
#' @param prep_l list of length four with data.frame with data, the unit used and the
#' main and sub titles. see \link[UMARaccessR]{prep_single_line}.
#'
#' @return nothing, plots to open device
#' @export
#'

na_chart <- function(prep_l){
  single <- prep_l[[1]]
  unit <- prep_l[[2]]
  main_title <- prep_l[[3]]
  sub_title <- prep_l[[4]]
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
            lwd = 0, lwd.ticks =2, tck=-0.01, labels = FALSE)

  # shifting year labels by six months
  ss <- as.POSIXlt(min(single$period))
  ss$mon <- as.POSIXlt(min(single$period))$mon+6
  ee <- as.POSIXlt(max(single$period))
  ee$mon <- as.POSIXlt(max(single$period))$mon+6

  axis.Date(1,at=seq(ss, ee, by="1 year"),
            col = umar_cols("gridlines"),
            lwd = 0, tck = 0,  family ="Myriad Pro",
            las = 2, padj = 0.5)

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


