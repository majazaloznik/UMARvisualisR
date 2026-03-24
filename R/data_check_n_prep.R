#' Check input data for multi-line chart
#'
#' New version for publication ready charts.
#' Helper function to check that the input data dataframe for multi line charts is
#' correct. And updates the units if they are missing or if the series have transformations.
#' This means that series used in a single chart have to:
#' - be unique
#' - have a single unit - possibly two for dual y-axis
#' - have a maximum of 8 series
#' ...
#'
#' @param df input dataframe with at least the following columns: serija, enota
#' @param con database connection
#' @param schema character schema name, default is "platform"
#'
#' @return input df, possibly with updated main titles.
#' @export
#'
check_plot_inputs <- function(df, con, schema = "platform") {
  errors  <- c()
  if(is.null(df)){
    stop("\nV tabeli ni nobene serije.")}

  if(nrow(df) > 8){
    errors <- c(errors,
                paste("\nMaksimalno \u0161tevilo serij na enem grafu je 8."))}

  dup_rows <- which(duplicated(df) | duplicated(df, fromLast = TRUE))
  if (length(dup_rows) > 0) {
    errors <- c(errors,
                paste("\nV tabeli ne sme\u0161 imeti podvojenih vrstic:\n", toString(dup_rows)))
  }

  xx <- FALSE
  if (!check_consistency_or_na(df$xmin)) {
    errors <- c(errors,
                paste("\nVse xmin vrednosti morajo biti enake."))
    xx <- TRUE}

  if (!check_consistency_or_na(df$xmax)) {
    errors <- c(errors,
                paste("\nVse xmax vrednosti morajo biti enake."))
    xx <- TRUE}

  if (!xx) {
    df <- df |>
      dplyr::mutate(xmin = ifelse(all(is.na(xmin)), NA, unique_without_na(xmin)),
                    xmax = ifelse(all(is.na(xmax)), NA, unique_without_na(xmax)))}

  if(!xx && !is.na(unique(df$xmin)) && !is.na(unique(df$xmax)) && unique(df$xmin) > unique(df$xmax)){
    errors <- c(errors,
                paste("\nVrendost xmax ne more biti manj\u0161a od vrednosti xmin."))}

  if (!check_consistency_or_na(df$naslov)) {
    errors <- c(errors,
                paste("\nVse vrednosti v polju naslov morajo biti enake."))}

  if (!check_consistency_or_na(df$velikost)) {
    errors <- c(errors,
                paste("\nVse vrednosti v polju velikost morajo biti enake."))}

  if (!all(df$velikost  %in% c(1, 2, 3, NA))) {
    errors <- c(errors,
                paste("\nV polju velikost so dovoljene samo vrednosti 1, 2 ali 3."))}

  if (!check_consistency_or_na(df$datum_podatkov)) {
    errors <- c(errors,
                paste("\nVse vrednosti v polju datum_podatkov morajo biti enake."))}

  if (!check_consistency_or_na(df$opomba)) {
    errors <- c(errors,
                paste("\nVse vrednosti v polju opomba morajo biti enake."))}

  if (!check_consistency_or_na(df$stolpci_legende)) {
    errors <- c(errors,
                paste("\nVse vrednosti v polju stolpci_legende morajo biti enake."))}

  if (!all(df$stolpci_legende  %in% c(1, 2, 3, 4, 5, NA))) {
    errors <- c(errors,
                paste("\nV polju stolpci_legende so dovoljene samo vrednosti od 1 do 5."))}

  if (!check_consistency_or_na(df$leva_y_os)) {
    errors <- c(errors,
                paste("\nVse vrednosti v polju leva_y_os morajo biti enake."))}

  if (!check_consistency_or_na(df$desna_y_os)) {
    errors <- c(errors,
                paste("\nVse vrednosti v polju desna_y_os morajo biti enake."))}

  if (!check_consistency_or_na(df$x_brez_let)) {
    errors <- c(errors,
                paste("\nVse vrednosti v polju x_brez_let morajo biti enake."))}

  if (!check_consistency_or_na(df$leva_os_en)) {
    errors <- c(errors,
                paste("\nVse vrednosti v polju leva_os_en morajo biti enake."))}

  if (!check_consistency_or_na(df$leva_os_si)) {
    errors <- c(errors,
                paste("\nVse vrednosti v polju leva_os_si morajo biti enake."))}

  if (!check_consistency_or_na(df$desna_os_si)) {
    errors <- c(errors,
                paste("\nVse vrednosti v polju desna_os_si morajo biti enake."))}

  if (!check_consistency_or_na(df$desna_os_en)) {
    errors <- c(errors,
                paste("\nVse vrednosti v polju desna_os_en morajo biti enake."))}

  if (any(duplicated(df$legenda[df$barva!=0 | is.na(df$barva)]))) {
    errors <- c(errors,
                paste("\nNe more\u0161 imeti enakih oznak legend."))}
  x <- df |>
    dplyr::select(intersect(names(df), c("serija", "drseca_obdobja", "drseca_poravnava",
                                         "rast", "indeks_obdobje", "glajenje_prvo"))) |>
    duplicated() |> any()
  if (x) {
    errors <- c(errors,
                paste("\nSerije se ne smejo podvajati, razen \u010de imajo razli\u010dne transformacije."))}

  # update units
  df <- update_units(df, con, schema)

  # check units
  if (length(unique_without_na(df$enota)) > 2) {
    stop("V grafu ima\u0161 (po transformacijah) ve\u010d kot dve enoti, kar je nemogo\u010de izrisati.")
  } else {
    if (length(unique_without_na(df$enota)) == 2) {
      if (all(is.na(df$leva_y_os))| all(is.na(df$leva_y_os))) {
        stop("V grafu ima\u0161 (po transformacijah) dve enoti, nima\u0161 pa vrednosti leva_y_os oz. desna_y_os.")
      }
    }
  }
  if (!all(df$tip  %in% c("line", "bar", "area", NA))) {
    errors <- c(errors,
                paste("\nV polju tip so dovoljene samo vrednosti 'line', 'bar' in 'area'."))}

    if (!check_uniqueness_or_na(df$barva[df$barva != 0])) {
    errors <- c(errors,
                paste("\nV polju barva ne sme\u0161 podvajati barv."))}

  if (!all(df$barva  %in% c(0:8, NA))) {
    errors <- c(errors,
                paste("\nV polju barva so dovoljene samo vrednosti od 1 do 8."))}

  if (!all(df$stacked  %in% c(TRUE, FALSE, NA))) {
    errors <- c(errors,
                paste("\nV polju stacked so dovoljene samo vrednosti TRUE ali FALSE (prazno polje je tudi dovoljeno in pomeni isto kot FALSE)."))}
  if (!all(df$drseca_najprej  %in% c(TRUE, FALSE, NA))) {
    errors <- c(errors,
                paste("\nV polju drseca_najprej so dovoljene samo vrednosti TRUE ali FALSE (prazno polje je tudi dovoljeno in pomeni isto kot FALSE)."))}

  if (!all(df$mio_eur  %in% c(TRUE, FALSE, NA))) {
    errors <- c(errors,
                paste("\nV polju mio_eur so dovoljene samo vrednosti TRUE ali FALSE (prazno polje je tudi dovoljeno in pomeni isto kot TRUE)."))}


  if (!check_uniqueness_or_na(df$stacked)) {
    errors <- c(errors,
                paste("\nV polju stacked morajo biti vse vrednosti enake (ali prazne)."))}

  if (!all(sapply(df$drseca_obdobja, \(x) is.numeric(x) || is.na(x)))) {
    errors <- c(errors,
                paste("\nV polju drseca_obdobja so dovoljene samo numeri\u010dne vrednosti."))}

  if (!all(df$drseca_poravnava  %in% c("R", "D", "C", "L", "r", "d", "c", "l", NA))) {
    errors <- c(errors,
                paste("\nV polju drseca_poravnava so dovoljene samo vrednosti 'D', 'C' in 'L'."))}

  if (!all(df$rast  %in% c("YOY", "QOQ", "MOM", "yoy", "qoq", "mom", NA))) {
    errors <- c(errors,
                paste("\nV polju rast so dovoljene samo vrednosti 'YOY', 'QOQ' in 'MOM'."))}
  # check growths
  growth_check <- df |>
    dplyr::mutate(interval = substr(serija, nchar(serija), nchar(serija)),
                  check = dplyr::if_else(toupper(rast) == "QOQ" & interval != "Q", FALSE,
                                         dplyr::if_else(toupper(rast) == "MOM" & interval != "M", FALSE, TRUE, missing = TRUE), missing =TRUE)) |>
    dplyr::pull(check) |>
    all()

  if (!growth_check) {
    errors <- c(errors,
                paste("\nVrednosti v polju rast se ne ujemajo z intervalom serije (npr. mese\u010dna rast za letne podatke)."))}

  # check indices
  check <- df |>
    dplyr::mutate(check = dplyr::if_else(
      is.na(indeks_obdobje), TRUE,
      dplyr::if_else(
        grepl("^\\d{4}M\\d{2}$", indeks_obdobje) &
          substr(serija, nchar(serija), nchar(serija)) == "M", TRUE,
        dplyr::if_else(
          grepl("^\\d{4}Q\\d{1}$", indeks_obdobje) &
            substr(serija, nchar(serija), nchar(serija)) == "Q", TRUE,
          dplyr::if_else(
            grepl("^\\d{4}$", indeks_obdobje), TRUE, FALSE))))) |>
    dplyr::filter(check == FALSE) |>
    dplyr::pull(indeks_obdobje)

  if (length(check) > 0) {
    errors <- c(errors,
                paste("\nVrednosti v polju indeks_obdobje niso v pravilni obliki ali pa se ne ujemajo z intervalom serije:",
                      paste(check, collapse = ", "), "."))}

  if (!check_consistency_or_na(df$indeks_obdobje)) {
    errors <- c(errors,
                paste("\nVse vrednosti v polju indeks_obdobje morajo biti enake."))}

  check <- df |>
    dplyr::mutate(check = dplyr::if_else(!is.na(indeks_obdobje) & !is.na(rast), TRUE,FALSE)) |>
    dplyr::filter(check) |>
    dplyr::pull(serija)

  if (length(check) > 0) {
    errors <- c(errors,
                paste("\nTransformacija na indeks s stalno osnovo in hkraten izra\u010dun rasti nima smisla in tako ni omogo\u010dena.",
                      paste(check, collapse = ", "), "."))}

  if (length(errors) == 0) df else {
    message(paste("Najdene so bile naslednje napake:",
                  paste(errors, collapse = "")))}
}


#' Prepare plot configuration from dataframe
#'
#' Takes the input dataframe which may or may not have config data for the plot
#' and the series and should have gone through the \link[UMARvisualisR]{check_plot_inputs}
#' check first!
#' Updates the default config with whatever is in the dataframe. Defaults are
#' hardcoded here and the prep will work even if only the series codes are
#' given in the dataframe, which is the bare minimum and will produce a basic
#' line plot configuration. The output config is the input to the plotting function.
#'
#' @param df with at least the columns series_code and unit
#'
#' @return a config list for plotting
#' @export
#'
prep_config <- function(df) {

  config <- list(
    xmin = as.Date("2010-01-01"),
    xmax = NULL,
    title = NULL,
    horizontal_alignment = "c",
    date_valid = NULL,
    footnote = NULL,
    legend_columns = 1,
    dual_y = FALSE,
    left_y = NULL,
    right_y = NULL,
    stacked = FALSE,
    chart_size = 2,
    x_sub_annual = FALSE,
    language = "si",
    y_axis_label_en = NULL,
    y_axis_label = NULL,
    y2_axis_label_en = NULL,
    y2_axis_label = NULL,
    series = list(
      list(series_code = NULL,
           unit = NULL,
           type = "line",
           colour = NA,
           legend_txt_si = NULL,
           legend_txt_en = NULL,
           rolling_period = NA,
           rolling_alignment = "c",
           growth = NA,
           index_period = NA,
           roll_first = TRUE,
           mio_eur = TRUE))
  )
  # plot parameters
  if (!all(is.na(df$xmin))) {
    config$xmin <- as.Date(UMARvisualisR:::unique_without_na(df$xmin),
                           origin = "1899-12-30")
  }
  if (!all(is.na(df$xmax))) {
    config$xmax <- as.Date(UMARvisualisR:::unique_without_na(df$xmax),
                           origin = "1899-12-30")
  }
  if (!all(is.na(df$naslov))) {
    config$title <- unique_without_na(df$naslov)
  }
  if (!all(is.na(df$velikost))) {
    config$chart_size <- unique_without_na(df$velikost)
  }
  if (!all(is.na(df$datum_podatkov))) {
    config$date_valid <- unique_without_na(df$datum_podatkov)
  }
  if (!all(is.na(df$opomba))) {
    config$footnote <- unique_without_na(df$opomba)
  }
  if (!all(is.na(df$stolpci_legende))) {
    config$legend_columns <- unique_without_na(df$stolpci_legende)
  }
  if (!all(is.na(df$leva_y_os))) {
    config$left_y <- unique_without_na(df$leva_y_os)
  }
  if (!all(is.na(df$desna_y_os))) {
    config$right_y <- unique_without_na(df$desna_y_os)
  }
  if (!all(is.na(df$stacked))) {
    config$stacked <- unique_without_na(df$stacked)
  }
  if (!all(is.na(df$x_brez_let))) {
    config$x_sub_annual <- unique_without_na(df$x_brez_let)
  }
  if (!all(is.na(df$leva_os_en))) {
    config$y_axis_label_en <- unique_without_na(df$leva_os_en)
  }
  if (!all(is.na(df$leva_os_si))) {
    config$y_axis_label <- unique_without_na(df$leva_os_si)
  }
  if (!all(is.na(df$desna_os_en))) {
    config$y2_axis_label_en <- unique_without_na(df$desna_os_en)
  }
  if (!all(is.na(df$desna_os_si))) {
    config$y2_axis_label <- unique_without_na(df$desna_os_si)
  }
  if(length(unique_without_na(df$enota)) == 2) {
    config$dual_y <- TRUE}

  # update colours
  colours <- df$barva
  missing_colours <- is.na(colours)
  existing_colours <- colours[!is.na(colours)]
  remaining_colours <- c(1:8)[!1:8 %in% existing_colours]
  colours[missing_colours] <- remaining_colours[1:sum(missing_colours)]

  # clean up rolling alignments
  alignments <- df$drseca_poravnava
  alignments <- tolower(alignments)
  alignments[alignments == "d"] <- "r"

  # clean up growths
  df$rast <- toupper(df$rast)

  # series parameters
  series_list <- list()
  for (i in 1:nrow(df)) {
    series_info <- list(
      series_code = df$serija[i],
      unit = df$enota[i],
      type = ifelse(is.na(df$tip[i]), config$series[[1]]$type, df$tip[i]),
      colour = ifelse(colours[i] == 0, "white", umar_cols()[colours[i]]),
      legend_txt_si = ifelse(is.na(df$legenda[i]),
                          ifelse(colours[i] == 0, "",df$serija[i]), df$legenda[i]),
      legend_txt_en = ifelse(is.na(df$legenda_en[i]),
                          ifelse(colours[i] == 0, "",df$serija[i]), df$legenda_en[i]),
      rolling_period = df$drseca_obdobja[i],
      rolling_alignment = alignments[i],
      growth = df$rast[i],
      index_period = df$indeks_obdobje[i],
      roll_first = ifelse(is.na(df$drseca_najprej[i]), config$series[[1]]$roll_first, df$drseca_najprej[i]),
      mio_eur = ifelse(is.na(df$mio_eur[i]), config$series[[1]]$mio_eur, df$mio_eur[i]))
    series_list[[i]] <- series_info
  }
  config$series <- series_list

  config
}

#' Change axis label for english version
#'
#' used inside \link[UMARvisualisR]{publication_ready_plot} to change
#' y axis label to english version.
#'
#' @param config configuration list
#'
#' @return updated config list
#' @export
#'
prep_config_en <- function(config){
  config$y_axis_label <- config$y_axis_label_en
  config$y2_axis_label <- config$y2_axis_label_en
  config
}

#' Get the datapoints from the database
#'
#' taking the configuration list output from \link[UMARvisualisR]{prep_config} returns
#' a list with a dataframe for each of the series of the correct vintage
#'
#' @param config configuration list
#' @param con connection to database
#' @param schema character schema name, default is "platform"
#'
#' @return list with a df for each series
#' @export
#'
get_data <- function(config, con, schema = "platform") {

  series_codes <- vapply(config$series, \(x) x$series_code, character(1))

  vintage_id <- purrr::map_int(series_codes, ~UMARaccessR::sql_get_vintage_from_series_code(con, .x, config$date_valid, schema))

  data_points <- purrr::map(vintage_id, ~UMARaccessR::sql_get_data_points_from_vintage(con, .x, schema))

  purrr::map(data_points, replace_period_id_column, config$horizontal_alignment)

}

#' Transform a series dataframe with the series confing
#'
#' Takes the dataframe with date and value columns and the series'
#' config list (not the chart config list, just the series') and
#' does the transformations. Currently the default is that rolling
#' averages are performed first, but i can change that later and
#' add the other way round. The \link[UMARvisualisR]{check_plot_inputs}
#' function has already made sure the combinations of transformations
#' are all legal.
#'
#' @param df dataframe with date and value columns
#' @param series_config list of series' config paramters from
#' \link[UMARvisualisR]{prep_config}.
#'
#' @return df and series' config
#' @export
#'
transform_data <- function(df, series_config) {

  if(series_config$roll_first){
    if(!is.na(series_config$rolling_period)){
      df <- transform_rolling(df, series_config$rolling_period,
                        series_config$rolling_alignment)
    }

    if(!is.na(series_config$growth)){
      df <- transform_growth(df, series_config$growth)
    }

    if(!is.na(series_config$index_period)){
      out <- transform_index(df, series_config$index_period)
      df <- out$df
      series_config$index_period <- out$base_period
    }
  }
  if(!series_config$roll_first){


    if(!is.na(series_config$growth)){
      df <- transform_growth(df, series_config$growth)
    }

    if(!is.na(series_config$index_period)){
      out <- transform_index(df, series_config$index_period)
      df <- out$df
      series_config$index_period <- out$base_period
    }
    if(!is.na(series_config$rolling_period)){
      df <- transform_rolling(df, series_config$rolling_period,
                              series_config$rolling_alignment)
    }
  }
  list(df = df, series_config = series_config)
}


#' Whole data prep pipeline for plotting publicaiton ready charts
#'
#' takes as its input a dataframe with one series per row and all the relevant info.
#' (see vignette that hasn't been written yet). Checks the data input is OK,
#' prepares the config, gets the data, transforms the data if necessary and
#' returns a list od dataframes with all the datapoints and the config list.
#'
#' @param df input dataframe with at least the following columns: serija, enota
#' @param con database connection
#' @param schema character schema name, default is "platform"
#'
#' @return list of dataframes and config list
#' @export
#'
prep_data <- function(df, con, schema = "platform") {

  df <- check_plot_inputs(df, con, schema)

  config <-  prep_config(df)

  datapoints <- get_data(config, con, schema)

  results <- purrr::map2(datapoints, config$series, transform_data)

  out <- purrr::transpose(results)

  datapoints <- out$df
  config$series <- out$series_config

  return(list(datapoints = datapoints, config = config))
}




#' Function to split the datapoints into left and right axis.
#'
#' @param datapoints list of dataframes from \link[UMARvisualisR]{prep_data}
#' @param config config dictionary list from \link[UMARvisualisR]{prep_config}
#'
#' @return list of left and right datapoints and configs (if right exists)
#' @export
split_by_unit <- function(datapoints, config){
  units <- purrr::map_chr(config$series, ~ .x$unit)
  unique_units <- unique(units)
  split_datapoints <- purrr::map(unique_units, function(x) {
    series_index <- which(units == x)
    datapoints[series_index]
  })
  datapoints_left <- split_datapoints[[1]]
  datapoints_right <- if (length(split_datapoints) > 1) split_datapoints[[2]] else NULL
  if(!is.null(datapoints_right)){
    split_config_series <- purrr::map(unique_units, function(x) {
      series_index <- which(units == x)
      config$series[series_index]
    })
    config$series <- split_config_series[[1]]
    config_left <- config
    config$series <- if (length(split_config_series) > 1) split_config_series[[2]] else NULL
    config_right <- config
    config_right$y_axis_label <- config_right$y2_axis_label
  } else {
    config_left <- config
    config_right <- NULL}

  return(list(datapoints = datapoints_left, config = config_left,
              datapoints_right = datapoints_right, config_right = config_right))
}

