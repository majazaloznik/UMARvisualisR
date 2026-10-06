#' Chart dimensions in inches
#'
#' The house sizes, in one place. All output devices use these, so a change
#' here applies to every format at once.
#'
#' @param size "small", "normal" or "large"
#'
#' @return numeric(2), width and height in inches
#' @export
#'
chart_size <- function(size = "normal") {
  cm <- 1/2.54
  switch(size,
         small  = c(7, 9) * cm,
         normal = c(11, 9) * cm,
         large  = c(22, 8) * cm,
         stop("size must be 'small', 'normal' or 'large', got '", size, "'",
              call. = FALSE))
}


#' Device point size
#'
#' Every output device must be opened at this point size. It is not a text
#' size - no text is drawn at 12 - but it is what fixes \code{par("csi")} at
#' 0.2 inches, which is the unit for every spacing constant in the package
#' (\code{legend_lead}, \code{note_lead}, \code{title_lead}, \code{title_gap},
#' \code{legend_offset}, \link{axis_label_gap} and all \code{mar} values). A
#' device opened at a different point size renders the same chart with
#' silently different spacing.
#'
#' @return numeric scalar
#' @keywords internal
device_pointsize <- function() 12


#' Formats \link{save_chart} can write
#'
#' @return character vector of extensions
#' @export
#'
chart_formats <- function() c("pdf", "png", "svg", "emf")


#' Open an output device for a chart
#'
#' Dispatches to the right device for \code{format} at the house size and
#' point size. Afterwards you need to call \code{dev.off()}.
#'
#' @param filename name of file with path
#' @param format one of \link{chart_formats}
#' @param size small, normal or large, defaults to normal
#'
#' @return the filename actually opened (may differ from \code{filename} if
#'   the original was locked), invisibly
#' @export
#'
chart_device <- function(filename, format = "pdf", size = "normal") {
  format <- tolower(format)
  if (!format %in% chart_formats()) {
    stop("format must be one of: ", paste(chart_formats(), collapse = ", "),
         call. = FALSE)
  }
  wh <- chart_size(size)
  width <- wh[1]
  height <- wh[2]
  ps <- device_pointsize()
  filename <- check_and_rename_file(filename)

  switch(format,
         pdf = grDevices::cairo_pdf(filename, width = width, height = height,
                                    pointsize = ps),
         png = grDevices::png(filename,
                              width = width * 600, height = height * 600,
                              res = 600, pointsize = ps, type = "cairo"),
         svg = {
           if (!requireNamespace("svglite", quietly = TRUE)) {
             stop("SVG output needs the 'svglite' package: install.packages('svglite')",
                  call. = FALSE)
           }
           svglite::svglite(filename, width = width, height = height,
                            pointsize = ps, bg = "white")
         },
         emf = {
           if (!requireNamespace("devEMF", quietly = TRUE)) {
             stop("EMF output needs the 'devEMF' package: install.packages('devEMF')",
                  call. = FALSE)
           }
           devEMF::emf(filename, width = width, height = height,
                       pointsize = ps, bg = "white",
                       emfPlus = TRUE, emfPlusFont = TRUE,
                       emfPlusFontToPath = FALSE) # pick true for larger file, nonsearchable text but no Aptos install requirement
         })

  invisible(filename)
}


#' Output to pdf
#'
#' Thin wrapper on \link{chart_device}. Opens device to output to pdf with
#' filename and size. Afterwards you need to call dev.off().
#'
#' @param filename name of file with path
#' @param size small, normal or large, defaults to normal
#'
#' @return nothing, produces pdf file
#' @export
#'
pdf_output <- function(filename, size = "normal") {
  chart_device(filename, "pdf", size)
}


#' Output to png
#'
#' Thin wrapper on \link{chart_device}. Opens device to output to png with
#' filename and size. Afterwards you need to call dev.off().
#'
#' @param filename name of file with path
#' @param size small, normal or large, defaults to normal
#'
#' @return nothing, produces png file
#' @export
#'
png_output <- function(filename, size = "normal") {
  chart_device(filename, "png", size)
}


#' Output to svg
#'
#' Thin wrapper on \link{chart_device}. Needs the \pkg{svglite} package.
#' Text is written as text and resolved against the fonts installed where the
#' file is opened - see \link{chart_device}.
#'
#' @param filename name of file with path
#' @param size small, normal or large, defaults to normal
#'
#' @return nothing, produces svg file
#' @export
#'
svg_output <- function(filename, size = "normal") {
  chart_device(filename, "svg", size)
}


#' Output to emf
#'
#' Thin wrapper on \link{chart_device}. Needs the \pkg{devEMF} package.
#' EMF is Word's native vector format on Windows.
#'
#' @param filename name of file with path
#' @param size small, normal or large, defaults to normal
#'
#' @return nothing, produces emf file
#' @export
#'
emf_output <- function(filename, size = "normal") {
  chart_device(filename, "emf", size)
}


#' Checks if a file is open/locked and renames it
#'
#' If the file you are trying to write to is open, it is also locked for
#' writing. So in this case we rename it by adding a timestamp before the
#' extension. Kludgy but works.
#'
#' @param filename file to check
#'
#' @return (updated) filename
#' @export
#'
check_and_rename_file <- function(filename) {
  if (!file.exists(filename)) return(filename)

  locked <- TRUE
  try({
    con <- file(filename, open = "r+")
    close(con)
    locked <- FALSE
  }, silent = TRUE)
  if (!locked) return(filename)

  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  ext <- tools::file_ext(filename)
  new_filename <- if (nzchar(ext)) {
    paste0(tools::file_path_sans_ext(filename), "_", timestamp, ".", ext)
  } else {
    paste0(filename, "_", timestamp)
  }
  message("File is locked, saving to a new file: ", new_filename)
  new_filename
}


#' Checks if pdf is open/locked and renames filename
#'
#' Deprecated, use \link{check_and_rename_file}, which handles any extension.
#'
#' @param filename file to check
#'
#' @return (updated) filename
#' @export
#'
check_and_rename_pdf <- function(filename) {
  check_and_rename_file(filename)
}
