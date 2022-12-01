#' Umar colour palette function
#'
#' Returns colour(s) from the Umar corporate graphical identity palette.
#' Colour names are rdeca, roza, siva, temno modra, turkizna, zelena, vijolicna,
#' sinja, gridlines and emph. The last two are grays for gridlines, axes and the like
#' and for emphasising the 100/0 gridline.
#'
#' @param ... optional colour names.
#'
#' @return hex value of relevant colour(s)
#' @export
umar_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (umar_colours)
  umar_colours[cols]
}
