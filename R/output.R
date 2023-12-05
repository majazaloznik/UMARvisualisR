#' Output to pdf
#'
#' Opens device to output to pdf with filename and size. Afterwards you
#' need to call dev.off().
#'
#' @param filename name of file with path
#' @param size small, normal or large, defaults to normal
#'
#' @return nothing, produces pdf file
#' @export
#'
pdf_output <- function(filename, size = "normal") {
  cm <- 1/2.54
  if (size == "small"){
    width <-  7*cm
    height <- 9*cm} else if (size == "normal"){
      width <-  11*cm
      height <- 9*cm} else if (size == "large"){
        width <-  22*cm
        height <- 8*cm}

  grDevices::cairo_pdf(filename, width, height)

}

