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

  new_filename <- check_and_rename_pdf(filename)

  grDevices::cairo_pdf(new_filename, width, height)

}

#' Output to png
#'
#' Opens device to output to png with filename and size. Afterwards you
#' need to call dev.off().
#'
#' @param filename name of file with path
#' @param size small, normal or large, defaults to normal
#'
#' @return nothing, produces png file
#' @export
#'
png_output <- function(filename, size = "normal") {
  cm <- 1/2.54
  if (size == "small"){
    width <-  7 * cm
    height <- 9 * cm} else if (size == "normal"){
      width <-  11 * cm
      height <- 9 * cm} else if (size == "large"){
        width <-  22 * cm
        height <- 8 * cm}

  new_filename <- check_and_rename_pdf(filename)

  png(filename, width = width * 300, height = height * 300, res = 300, pointsize = 12)

}


#' Checks if pdf is open/locked and renames filename
#'
#' if the pdf you are trying to write to is open, it is also locked for
#' writing. so in this case we rename the filename by adding the timestamp
#' to the end of the filename. kludgy but works.
#'
#' @param filename file to check
#'
#' @return (updated) filename
#' @export
#'
check_and_rename_pdf <- function(filename) {
  if (file.exists(filename)) {
    try({
      # Attempt to open and close the file to check if it's locked
      con <- file(filename, open = "r+")
      close(con)
      return(filename)
    }, silent = TRUE)

    # If the file is locked, rename it
    timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    new_filename <- sub("\\.pdf$", paste0("_", timestamp, ".pdf"), filename)
    message("File is locked, saving to a new file: ", new_filename)
    return(new_filename)
  } else {
    return(filename)
  }
}
