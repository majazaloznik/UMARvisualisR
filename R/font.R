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
