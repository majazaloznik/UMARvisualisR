

#' Rename generic column names for export
#'
#' From the prepared list used to plot the charts, this funciton renames the columns
#' with the raw and transformed data with the series names, preparing them
#' for export into an excel spreadsheet with the yet unwritten funcito `export_to_excel`.
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
    prep_l$data_points <- purrr::map2(prep_l$data_points, prep_l$legend_labels,
                                      ~dplyr::rename(.x, !!.y := value))
    }
  }
  prep_l$data_points <- purrr::map(prep_l$data_points, ~ dplyr::relocate(., period_id, period))
  return(prep_l)
}

