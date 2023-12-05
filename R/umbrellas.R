#' FUll pipeline function for plotting a univariate line chart
#'
#' From the series id and the connection, gets the appropriate vintage id,
#' then the data prepared for the chart and finally plots the chart.
#'
#' @param date_valid date when the vintage was valid if none is given most recent
#' i.e. currently valid vintage is returned.
#' @param xmin character date, default "2011-01-01"
#' @param xmax character date, default today
#' @param rolling logical / should a rolling average be calculated and displayed
#' @param roll_periods number of periods to roll over (defualt = 3)
#' @param roll_align alignment of rolling average (default = "center")
#' @param yoy logical - should the year on year change be calculated and displayed
#' @param interval character describing interval type
#' @param unit character describing unit to be written as y label
#' @param main_title character..
#' @param sub_title character...
#' @inheritParams common_parameters
#'
#' @return plots a univariate line chart
#' @export

univariate_line_pipeline <- function(series,
                             date_valid = NULL,
                             xmin = "2011-01-01", xmax =NULL,
                             rolling = FALSE, roll_periods = 3, roll_align = "center",
                             yoy = FALSE,
                             interval = NULL,
                             unit = NULL,
                             main_title = NULL,
                             sub_title = NULL,
                             con) {
  vintage_id <- UMARaccessR::get_vintage_from_series(series, con, date_valid = date_valid)
  prep_l <- prep_single_line(vintage_id, con, interval = interval, unit = unit,
                             main_title = main_title, sub_title = sub_title)
  names(prep_l)[names(prep_l)== "single"] <- "data_points"
  if(rolling & yoy){
    prep_l <- add_yoy_of_rolling(prep_l, periods = roll_periods, align = roll_align)} else {
      if(rolling) prep_l <- add_rolling_average(prep_l, periods = roll_periods, align = roll_align)
      if(yoy) prep_l <- add_yoy_change(prep_l)}
  univariate_line_chart(prep_l, xmin, xmax)
}



#' Multiline pipeline from input dataframe
#'
#' Pipeline that takes an appropriate dataframe with instrucitons for the charts and
#' prepares the data and outputs the the charts. SHould work on single and multiline
#' and on transformed data as well as long as all the series in a multiline have the
#' same transformation.
#'
#' @param df dataframe with the following fields: `tabel_name`, `series_name`, `series_code`,
#' `unit_name`,	`interval_id`,	`chart_no`,	`rolling_average_periods`,
#' `rolling_average_alignment`,	`year_on_year`.
#' @inheritParams common_parameters
#' @param xmin to be passed to \link[UMARvisualisR]{apply_xlims}
#' @param xmax to be passed to \link[UMARvisualisR]{apply_xlims}
#' @param pdf_charts logical
#' @param html_charts logical
#' @param export_excel logical
#' @param wb active binding to openxlsx workbook object
#' @param odd logical indicating alternating charts, used in the TOC of the Excel
#' spreadsheet for the backgrounds for greater readability.
#'
#'
#' @return plots chart and writes the data to an excel file - if you chose so.
#' @export
multiline_pipeline  <- function(df, con, xmin = "2011-01-01", xmax =NULL,
                                pdf_charts = TRUE,
                                html_charts = FALSE,
                                export_excel = FALSE, wb, odd = TRUE){
  # prepare the data
  prep_l <-  tryCatch(prep_multi_line(df, con),
                      error = function(e) {print(e)
                        print("Graf bo preskocen.")
                        return(NULL)})
  # draw chart
  if(pdf_charts & !is.null(prep_l)){
    multivariate_line_chart(prep_l, xmin = xmin, xmax = xmax)
  }
  # draw chart
  if(html_charts & !is.null(prep_l)){
    dygraph_plotter(prep_l )
  }

  if(export_excel & !is.null(prep_l)) {
    prep_l <- rename_columns(prep_l)
    write_to_sheet(prep_l, df, wb=wb, odd)
  }
}


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

  # output slovenian chart
  pdf_output(output_si, size)
  publication_ready_plot(datapoints, config)
  dev.off()

  #output english chart
  pdf_output(output_en, size)
  publication_ready_plot(datapoints, config, "en")
  dev.off()
}
