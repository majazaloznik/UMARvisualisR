
#' Prepare data needed for a univariate line chart
#'
#' Given the vintage id and a connection to the database, this function
#' gets the datapoints, and the unit, prepares the titles. The returned list
#' is the input for the plotting function \link[UMARvisualisR]{univariate_line_chart}.
#'
#' @param con PostgreSQL connection object created by the RPostgres package.
#' @param vintage numeric id of vintage
#' @param interval character value of interval type, defaults to NULL in which case the
#' function will get it from the database.
#' @param unit character describing unit to be written as y label. defaults to NULL in which case the
#' function will get it from the database.
#' @param main_title character..defaults to NULL in which case the
#' function will get it from the database.
#' @param sub_title character...defaults to NULL in which case the
#' function will get it from the database.
#'
#' @return a list with the data frame with values, period_ids and periods as the
#' first element, a character unit name as second, and the row wrapped main and
#' subtitles (default 100 chars max 3 lines), the date and time of the last update,
#' the last period and the interval.
#' @export
#'
prep_single_line <- function(vintage, con, interval=NULL,
                             unit = NULL, main_title = NULL, sub_title = NULL){
  if(is.null(interval)) {interval <- UMARaccessR::get_interval_from_vintage(vintage, con)}
  single <- UMARaccessR::add_date_from_period_id(
    UMARaccessR::get_data_points_from_vintage(vintage, con), interval)
  if(is.null(unit)) { unit <-first_up(UMARaccessR::get_unit_from_vintage(vintage, con))}
  if(is.null(main_title)) { main_title <- wrap_string(
    UMARaccessR::get_table_name_from_vintage(vintage, con))} else {
      main_title <- wrap_string(main_title)}
  if(is.null(sub_title)) {sub_title <- wrap_string(
    UMARaccessR::get_series_name_from_vintage(vintage, con))} else {
      sub_title <- wrap_string(sub_title)}
  updated <- UMARaccessR::get_date_published_from_vintage(vintage, con)
  last_period <- UMARaccessR::get_last_period_from_vintage(vintage, con)
  mget(c("single", "unit", "main_title" , "sub_title", "updated", "last_period", "interval"))
}


#' Check input data for multi-line chart
#'
#' Helper function to check that the input data dataframe for multi line charts is
#' correct. This means that series used in a single chart have to:
#' - have the same unit
#' - have the same interval (this may be changed in the future)
#' - are either all rolling averages or not and if they are, they have the same
#' alignment and number of periods (might also change in the future)
#' - are either all y-o-y changes or not (might also change in the future).
#' If any of these is true, the function stops
#'
#' Also checks if
#' - main_titles are the same, otherwise it deletes them, but only issues a warning.
#'
#' @param df input dataframe with at least the following columns: series_code, unit
#' interval_id, rolling_average_alignment, rolling_average_periods, year-on-year
#' @param con PostgreSQL connection object created by the RPostgres package.
#'
#' @return input df, possibly with updated main titles.
#' @export
#'
multi_checks <- function(df, con){
  if(nrow(df) > 8)  warning(
    paste("Graf \u0161tevilka", unique(df$chart_no),
          "\n Maksimalno \u0161tevilo serij na enem grafu je 8."))
  if(!all_equal(df$unit_name))  stop(
    paste("Graf \u0161tevilka", unique(df$chart_no),
          "\n Vse izbrane serije morajo imeti enako enoto!"))
  if(!all_equal(df$interval_id))  stop(
    paste("Graf \u0161tevilka", unique(df$chart_no),
          "\n Trenutno ve\u010dlinijski grafi niso mo\u017eni za serije z razli\u010dnimi intervali."))
  if(!all_equal(df$rolling_average_alignment)) stop(
    paste("Graf \u0161tevilka", unique(df$chart_no),
          "\n Vse serije na ve\u010dlinisjkem grafu morajo uporabljati enako drse\u010do sredino."))
  if(!all_equal(df$rolling_average_periods)) stop(
    paste("Graf \u0161tevilka", unique(df$chart_no),
          "\n Vse serije na ve\u010dlinisjkem grafu morajo uporabljati enako drse\u010do sredino."))
  if(!all_equal(df$year_on_year)) stop(
    paste("Graf \u0161tevilka", unique(df$chart_no),
          "\n Medletno spremembo na ve\u010dlinijskem grafu je mogo\u010d uporabiti za vse serije ali za nobeno."))
  df <- multi_titles(df, con)
}


#' Check titles in input for multi-line chart
#'
#' Check if the main_title is the same for all series, and if not issue a warning and
#' remove all the titles
#'
#' @param df input dataframe with at least the following columns: series_code, unit_name
#' interval_id, rolling_average_alignment, rolling_average_periods, year_on_year
#' @param con PostgreSQL connection object created by the RPostgres package.

#' @return input df, possibly with updated main titles.
#' @export
#'
multi_titles <- function(df, con){
  if(!all_equal(df$table_name))  {
    warning(paste("Graf \u0161tevika", unique(df$chart_no),
                  "\n Vse izbrane serije morajo imeti enak naslov tabele, razen, \u010de imajo vse isto ime in razli\u010dna imena tabel."))
    df$table_name <-  paste("Graf \u0161tevika", unique(df$chart_no))
  }
  if(all_equal(df$series_code) & is.na(unique(df$table_name))) df$table_name <- UMARaccessR::get_table_name_from_series(df$id[1], con)
  if(is.na(unique(df$table_name))) df$table_name <-  paste("Graf \u0161tevika", unique(df$chart_no))
  df
}

#' Get legend labels from input dataframe
#'
#' One of the columns is the `series_name` one, which is from the `series` table and contains
#' the dimension values separated by `--`. If these names have the same number of dimension
#' values and differ by just one of them, then that one is returned as the legend labels.
#' Warnings are issued for different numbers of dimensions or differences in more than
#' one dim, or if the series names are the same and no table names are provided,
#' in which three cases NAs are returned.
#'
#' If the series are different by one dimension, that is returned, and if the series
#' are the same and the different table names are provided, then those are returned.
#'
#'
#' @param df input dataframe with at least the following columns: `series_name`, `chart_no`
#' @param original_table_names character vector with original table names, which is only relevant
#' if series names are identical.
#' @return character vector of length nrow(df) with legend labels and optionally a second one
#' with the new chart name.
#' @export
#'
get_legend_labels_from_df <- function(df, original_table_names = NULL) {
  splt <- sapply(list(df$series_name)[[1]], function(x) strsplit(x, " -- "))
  # check for different number of dimensions
  if(!UMARvisualisR::all_equal(sapply(splt, length))) {
    warning(paste("Graf \u0161t.", unique(df$chart_no),
                  ": Besedila legende ni mogo\u010de dolo\u010diti avtomati\u010dno, ker so serije iz razli\u010dnih tabel."))
    diff <- df$series_code} else {
      intersection <- Reduce(intersect, splt)
      if(UMARvisualisR::all_equal(splt)){
        if(is.null(original_table_names) | UMARvisualisR::all_equal(original_table_names)){
          warning(paste("Graf \u0161t.", unique(df$chart_no),
                        ": Oznak legende ni mogo\u010de dolo\u010diti avtomati\u010dno, ker so imena serij in tabel enaka."))
          diff <- df$series_code} else  {
            new_table_name <- wrap_string(paste(unique(splt)[[1]], collapse = " -- "))
            mget(c("original_table_names", "new_table_name"))
          }
      } else {
        if(!unique(sapply(splt, length)) == length(intersection)+1) {
          warning(paste("Graf \u0161t.", unique(df$chart_no),
                        ": Oznak legende ni mogo\u010e dolo\u010diti avtomati\u010dno, ker se razlikujejo po ve\u010d kot eni dimenziji"))
          diff <- df$series_code} else {
            diff <- sapply(splt, function(x) setdiff(x, intersection))
            unname(diff)}
      }
    }
}


#' Prepare data needed for multi (or single) line chart
#'
#' Uses an input table which must have the following columns: `interval_id`,
#' `unit_name`, `table_name`, `series_name` for the subtitle, `series_code` for the series id
#' (same as gets input into \link[UMARvisualisR]{multi_checks}, which is
#' run on the dataframe as the first step in this funciton, to check everything is cool with
#' the inputs.
#'
#' Prepares the list of data, structured to get plotted properly. Currently seven
#' elements are prepared: `data_points` a list of dataframes with the actual data,
#' `unit_name`, `table_name` , `sub_title`, `updated`, `last_period`, `interval`.
#'
#' Needs a valid connection to get the data as well as the last published date.
#'
#' @param df dataframe described above
#' @param con PostgreSQL connection object created by the RPostgres package.
#' @param date_valid validity of vintages, NULL gives most recent.
#'
#' @return A list of lentgh seven described above.
#' @export
#'

prep_multi_line <- function(df, con, date_valid = NULL){
  if ("chart_no" %in% names(df)) warning(paste0("Pripravljam podatke za graf \u0161t. ",
                                                unique(df$chart_no), "."))
  original_table_names <- df$table_name
  df <- multi_checks(df, con)
  interval <- unique(df$interval_id)
  unit <- first_up(unique(df$unit_name))
  unit <- ifelse(unit == "Eur", "EUR", unit)
  unit <- ifelse(unit == "Mio eur", "Mio EUR", unit)
  main_title <- wrap_string(unique(df$table_name))
  if (nrow(df) > 1) {
    sub_title <- list(NA, 0)
    legend_labels <- get_legend_labels_from_df(df, original_table_names)
    if(length(names(legend_labels)) == 2) {
      main_title <- legend_labels[[2]]
      legend_labels <- legend_labels[[1]]
    }} else {
      sub_title <- wrap_string(df$series_name)
      legend_labels <- NA}
  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(vintage_id = UMARaccessR::get_vintage_from_series_code(series_code, con, date_valid)$id,
                  updated = UMARaccessR::get_date_published_from_vintage(vintage_id, con)$published)
  data_points <- purrr::map(df$vintage_id, UMARaccessR::get_data_points_from_vintage, con)
  data_points <- purrr::map(data_points, add_date_from_period_id)
  # transformations:
  input_data <- list(data_points = data_points,
                     rolling_average_periods = df$rolling_average_periods,
                     rolling_average_alignment = df$rolling_average_alignment,
                     year_on_year = df$year_on_year,
                     interval = interval)
  transformed <- do_transformations(input_data)
  data_points <- transformed$data_points
  transf_txt <- transformed$transf_txt
  unit <- ifelse(is.na(transformed$unit), unit, transformed$unit)

  updated <- max(df$updated)

  last_period <- get_max_period(data_points)
  warning(paste0("Podatki za graf \u0161t. ", unique(df$chart_no), " so pripravljeni."))
  mget(c("data_points", "unit", "main_title" , "sub_title", "updated", "last_period",
         "interval", "legend_labels", "transf_txt"))
}


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
#'
#' @return input df, possibly with updated main titles.
#' @export
#'
check_plot_inputs <- function(df, con){
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
  df <- update_units(df, con)

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
#'
#' @return list with a df for each series
#' @export
#'
get_data <- function(config, con) {

  series_codes <- vapply(config$series, \(x) x$series_code, character(1))

  vintage_id <- purrr::map_int(series_codes, ~UMARaccessR::get_vintage_from_series_code(.x, con, config$date_valid)$id)

  data_points <- purrr::map(vintage_id, UMARaccessR::get_data_points_from_vintage, con)

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
#'
#' @return list of dataframes and config list
#' @export
#'
prep_data <- function(df, con) {

  df <- check_plot_inputs(df, con)

  config <-  prep_config(df)

  datapoints <- get_data(config, con)

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

