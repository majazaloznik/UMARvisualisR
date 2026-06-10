.umar_env <- new.env(parent = emptyenv())
.umar_env$font_family <- "Aptos"

#' Get or set the font family used in UMAR charts
#'
#' @param family character font family name. If NULL, returns current value.
#' @return current font family (invisibly when setting)
#' @export
umar_font <- function(family = NULL) {
  if (is.null(family)) {
    .umar_env$font_family
  } else {
    .umar_env$font_family <- family
    invisible(family)
  }
}


.umar_env$palette <- "default"

#' Get or set the active UMAR colour palette
#'
#' The default is the UMAR-style palette used in all official publications.
#' The alternative is the Janez-style colour blindness palette. Set it once per
#' session!
#'
#' @param palette "default" or "Janez" (or "janez"). Omit to get current value.
#' @return active palette name (invisibly when setting)
#' @export
umar_palette <- function(palette) {
  if (missing(palette)) return(.umar_env$palette)
  if (!palette %in% c("default", "janez", "Janez"))
    stop("palette must be 'default' or 'Janez'.")
  .umar_env$palette <- palette
  invisible(palette)
}
