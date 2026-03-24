

#' Full pipeline for publicaiton ready charts
#'
#' Input appropriate excel file with config info and produces an english and
#' slovenian version of the chart in pdf format, currently to the hardcoded
#' folder "O:\\Avtomatizacija\\umar-publication-ready-grafi\\grafi\\",
#' but this can of course be changed if needed.
#'
#' @param input_file excel with single sheet and all relevant info for config.
#' see vingette (non existent as of now) for full details.
#'
#' @return nothing, produces pdf charts
#' @export
publication_ready_plot_pipeline <- function(input_file){
  # setup
  output_path <-"O:\\Avtomatizacija\\umar-publication-ready-grafi\\grafi\\"
  filename <- basename(input_file)
  base_filename <- sub('\\.xlsx$', '', filename)
  # get and prep data
  x <- openxlsx::read.xlsx(input_file)
  results <- prep_data(x, con)
  out <- split_by_unit(results$datapoints, results$config)
  datapoints <- out$datapoints
  config <- out$config

  size <- ifelse(config$chart_size == 1, "small",
                 ifelse(config$chart_size == 2, "normal", "large"))
  # output file names
  output_si <- paste0(output_path, base_filename, "_si", ".pdf")
  output_en <- paste0(output_path, base_filename, "_en", ".pdf")
  output_si_png <- paste0(output_path, base_filename, "_si", ".png")
  output_en_png <- paste0(output_path, base_filename, "_en", ".png")

  # output slovenian chart
  pdf_output(output_si, size)
  publication_ready_plot(datapoints, config)
  dev.off()

  png_output(output_si_png, size)
  publication_ready_plot(datapoints, config)
  dev.off()

  #output english chart
  pdf_output(output_en, size)
  publication_ready_plot(datapoints, config, "en")
  dev.off()

  png_output(output_en_png, size)
  publication_ready_plot(datapoints, config, "en")
  dev.off()
}
