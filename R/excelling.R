#' Rename generic column names for export
#'
#' From the prepared list used to plot the charts, this funciton renames the columns
#' with the raw and transformed data with the series names, preparing them
#' for export into an excel spreadsheet with the function \link[UMARvisualisR]{write_to_sheet}.
#'
#' @param prep_l list of prepared data, which must include elements `data_points`, `sub_title`
#' or `legend_labels` and `transf_txt` if a transformation was made
#'
#' @return same list with updated column names in the dataframe(s) in `data_points`
#' @export
#'
rename_columns <- function(prep_l) {
  # name columns
  if(length(prep_l$data_points)==1) {
    if(!is.null(prep_l$transf_txt)){
      prep_l$data_points[[1]] <- dplyr::rename(prep_l$data_points[[1]],
                                              !! paste0(prep_l$sub_title[[1]],
                                                      " -- ", prep_l$transf_txt) := value)
      prep_l$data_points[[1]] <- dplyr::rename(prep_l$data_points[[1]],
                                               !! paste0(prep_l$sub_title[[1]],
                                                         " -- original") := raw)
    } else {
      prep_l$data_points[[1]] <- dplyr::rename(prep_l$data_points[[1]], !!prep_l$sub_title[[1]] := value)
    }
  } else {
    if(!is.null(prep_l$transf_txt)){
      prep_l$data_points <- purrr::map2(prep_l$data_points, prep_l$legend_labels,
                                        ~dplyr::rename(.x, !!paste0(.y, " -- ", prep_l$transf_txt) := value))
      prep_l$data_points <- purrr::map2(prep_l$data_points, prep_l$legend_labels,
                                        ~dplyr::rename(.x, !!paste0(.y, " -- original") := raw))

    } else {
      if(!any(is.na(prep_l$legend_labels))) {
        prep_l$data_points <- purrr::map2(prep_l$data_points, prep_l$legend_labels,
                                          ~dplyr::rename(.x, !!.y := value))
      }
    }
  }

  prep_l$data_points <- purrr::map(prep_l$data_points, ~ dplyr::relocate(., period_id, period))
  return(prep_l)
}


#' Write prepared data to an excel spreadsheet
#'
#' Taking the prepared data with the columns renamed by \link[UMARvisualisR]{rename_columns}
#' and joining into a single dataframe, writing it to a sheet and formatting a bit.
#'
#' @param prep_l list output from \link[UMARvisualisR]{rename_columns}
#' @param df dataframe input of series for each chart - needed to get the chart number
#' @param wb an active workbook binding to write to.
#' @param odd logical indicating alternating charts, used in the TOC of the Excel
#' spreadsheet for the backgrounds for greater readability.
#'
#' @return side effect is writing to the excel file.
#' @export
#'
write_to_sheet <- function(prep_l, df, wb, odd) {

  out <- purrr::reduce(prep_l$data_points, dplyr::left_join, by = c("period_id", "period")) %>%
    dplyr::relocate(period_id, period)

  sheet_name <- paste0("Graf_\u0161t_", unique(df$chart_no))
  try(openxlsx::addWorksheet(wb, unique(sheet_name)))
  openxlsx::writeData(wb, sheet_name, out, startRow = 1, startCol = 1)
  openxlsx::freezePane(wb, sheet = sheet_name, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet = sheet_name, cols = 3:ncol(out), widths = 15)
  openxlsx::addStyle(wb, sheet_name, style = openxlsx::createStyle(fontSize = 10),
                     cols = 1:ncol(out), rows = 1:(nrow(out)+1), gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet_name, style = openxlsx::createStyle(numFmt = "dd.mm.yyyy",
                                                                   fontSize = 10),
                     rows = 2:(nrow(out)+1), cols = 2, gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet = sheet_name,
                     openxlsx::createStyle(wrapText = TRUE, border = "Bottom", fontSize = 10,
                                           borderColour = "black", textDecoration = "bold"),
                     rows = 1, cols = 1:ncol(out))
  write_metadata(prep_l, sheet_name, df, wb, odd)
}


#' Write metadata to first sheet of workbook
#'
#' Prepares the metadata to be written to the first sheet in the workbook
#' and appended each time to the stuff already there.
#'
#' @param prep_l prepared data
#' @param sheet_name character string with chart number, also used as sheet name
#' @param wb workbook object
#'
#' @return nothing, side effect is writing to excel
#' @keywords internal

write_metadata <- function(prep_l, sheet_name, df, wb, odd) {
  if(nrow(df) == 1){
    meta <- data.frame(chart = sheet_name,
                       updated=prep_l$updated,
                       main = prep_l$main_title[[1]],
                       series = df$series_name,
                       transformation = ifelse(is.null(prep_l$transf_txt),
                                               NA_character_, prep_l$transf_txt))}
  else {
    meta <- data.frame(chart = sheet_name,
                       updated=prep_l$updated,
                       main = prep_l$main_title[[1]],
                       series = prep_l$legend_labels,
                       transformation = ifelse(is.null(prep_l$transf_txt),
                                               NA_character_, prep_l$transf_txt))
  }
  meta$chart[duplicated(meta$chart)] <- NA
  meta$updated[duplicated(meta$updated)] <- NA
  meta$main[duplicated(meta$main)] <- NA
  row_names <- openxlsx::readWorkbook(wb, sheet='Kazalo', colNames = F, cols = 4)
  start_row <- nrow(row_names) + 1
  openxlsx::writeData(wb, "Kazalo", meta, startRow = start_row, startCol = 1,
                      colNames = FALSE)
  if(odd) {
    openxlsx::addStyle(wb, sheet = "Kazalo", style = openxlsx::createStyle( fgFill  = umar_cols("sinja")),
                              rows = start_row: (start_row+nrow(meta)-1), cols = 1:5,
                              gridExpand = TRUE)
    openxlsx::addStyle(wb, sheet = "Kazalo", style = openxlsx::createStyle(numFmt = "dd.mm.yyyy hh:mm:ss",
                                                                           fgFill  = umar_cols("sinja")),
                       rows = start_row: (start_row+nrow(meta)-1), cols = 2 )}
  if(!odd) {
    openxlsx::addStyle(wb, sheet = "Kazalo", style = openxlsx::createStyle(numFmt = "dd.mm.yyyy hh:mm:ss"),
                     rows = start_row: (start_row+nrow(meta)-1), cols = 2 )}
}


#' Prepare Excel workbook for writing to
#'
#' @return workbook object
#' @export
prep_workbook <- function(){
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Kazalo")
  colnames <- data.frame(chart = character(),	updated = as.Date(character()),
                         main = character(),	series = character(),	transformation = character())
  openxlsx::writeData(wb, "Kazalo", colnames, startRow = 1, startCol = 1)
  openxlsx::setColWidths(wb, "Kazalo", cols = c(2,3,4,5), widths = c(20, 50, 100, 40))
  openxlsx::addStyle(wb, sheet = "Kazalo",
                     style = openxlsx::createStyle(wrapText = TRUE, border = "Bottom",
                                                   borderColour = "black", textDecoration = "bold"),
                     rows = 1, cols = 1:5)
  options(openxlsx.dateFormat = "dd.mm.yyyy")
  return(wb)
}
