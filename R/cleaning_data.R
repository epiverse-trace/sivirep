#' Standardize geographic codes of the disease data
#'
#' Function that standardizes the geographic codes of the disease data
#' @param disease_data The disease data
#' @return The standardized geographic codes of the disease data
#' @examples
#' disease_data <- import_linelist_disease_year(2019, "DENGUE")
#' disease_data <- clean_header(disease_data)
#' standardize_geo_codes(disease_data)
#' @export
standardize_geo_codes <- function(disease_data) {
  disease_data2 <- clean_header(disease_data)
  geo_column_names <- config::get(file =
                                    system.file("extdata", "config.yml",
                                                package = "sivirep"),
                                  "geo_column_names")
  for (column in geo_column_names) {
    if (stringr::str_detect(column, "dpto") == TRUE) {
      disease_data2[[column]] <- formatC(disease_data2[[column]],
                                         width = 2,
                                         format = "d",
                                         flag = "0")
    }
    if (stringr::str_detect(column, "mun") == TRUE) {
      disease_data2[[column]] <- formatC(disease_data2[[column]],
                                         width = 3,
                                         format = "d",
                                         flag = "0")
    }
  }
  return(disease_data)
}

#' Clean department codes of the disease data
#'
#' Function that cleans the department codes of the disease data
#' @param depto_codes The department codes
#' @param disease_data The disease data
#' @param make_group Indicates if it is necessary to group by
#' department IDs and case numbers
#' @return Clean department codes of the disease data
#' @examples
#' geo_codes <- import_geo_codes()
#' depto_codes <- get_depto_codes(geo_codes)
#' disease_data <- import_linelist_disease_year(2019, "DENGUE")
#' disease_data <- clean_header(disease_data)
#' data_grouped <- group_columns_cases(disease_data,
#'                                     "cod_dpto_o",
#'                                     wt_percentage = TRUE)
#' clean_depto_disease_codes(depto_codes, data_grouped)
#' @export
clean_depto_disease_codes <- function(depto_codes,
                                      disease_data,
                                      make_group = TRUE) {
  depto_codes$id <- as.character(depto_codes$cod_dep)
  disease_data$id <- as.character(disease_data$cod_dpto_o)
  disease_data_clean <- disease_data
  if (make_group) {
    disease_data_clean <- disease_data %>%
      dplyr::group_by(.data$id) %>%
      dplyr::summarise(casos = sum(.data$casos))
  }
  return(disease_data_clean)
}

#' Clean department codes of the disease data
#'
#' Function that cleans the department codes of the disease data
#' @param disease_data The disease data
#' @param col_data_codes Column name in the disease data that contains
#' the department codes
#' @param geo_data Geographical data that includes the departments codes
#' @param col_geo_codes Column name in the geographical data than
#' contains the department codes
#' @return Clean department codes of the disease data
#' @examples
#' geo_codes <- import_geo_codes()
#' disease_data <- import_linelist_disease_year(2019, "DENGUE")
#' disease_data <- clean_header(disease_data)
#' clean_depto_codes(disease_data,
#'                   col_data_codes = "cod_dpto_o",
#'                   geo_codes,
#'                   col_geo_codes = "codigo_departamento")
#' @export
clean_depto_codes <- function(disease_data,
                              col_data_codes,
                              geo_data,
                              col_geo_codes) {
  col_detps_geo <- eval(parse(text = paste0("geo_data$", col_geo_codes)))
  col_detps_geo <- as.character(col_detps_geo)
  disease_data_clean <- disease_data
  col_detps_data <- eval(parse(text =
                                 paste0("disease_data_clean$", col_data_codes)))
  col_detps_data <- as.character(col_detps_data)
  col_detps_data[
    nchar(col_detps_data) < 2 & col_detps_data != "1" & col_detps_data != "0" &
      paste("0", col_detps_data, sep = "") %in% col_detps_geo
  ] <- paste("0", col_detps_data[
    nchar(col_detps_data) < 2 & col_detps_data != "1" & col_detps_data != "0" &
      paste("0", col_detps_data, sep = "") %in% col_detps_geo
  ], sep = "")
  col_detps_data[col_detps_data == "1"
                 & paste("1", col_detps_data, sep = "")
                 %in% col_detps_geo] <- "11"
  return(disease_data_clean)
}

#' Parse age to years
#'
#' Function that converts ages into years according to
#' SIVIGILA measurement units
#' @param disease_data The disease data
#' @param col_age Column name in the disease data that contains the ages
#' @param col_uni_met Column name in the disease data that contains the
#' measurement units
#' @return The ages in years
#' @examples
#' disease_data <- import_linelist_disease_year(2019, "DENGUE")
#' disease_data <- clean_header(disease_data)
#' parse_age_to_years(disease_data, col_age = "edad", col_uni_met = "uni_med")
#' @export
parse_age_to_years <- function(disease_data,
                               col_age = "edad",
                               col_uni_met = "uni_med") {
  disease_dt_to_years <-
    dplyr::mutate(disease_data,
                  edad =
                  dplyr::case_when(eval(parse(text = col_uni_met)) == 1 ~
                                     round(eval(parse(text = col_age)), 3),
                                   eval(parse(text = col_uni_met)) == 2 ~
                                     round((eval(parse(text =
                                                         col_age)) / 12), 3),
                                   eval(parse(text = col_uni_met)) == 3 ~
                                     round((eval(parse(text =
                                                         col_age)) / 876), 3),
                                   eval(parse(text = col_uni_met)) == 4 ~
                                     round((eval(parse(text =
                                                         col_age)) / 525960),
                                           3),
                                   eval(parse(text = col_uni_met)) == 5 ~
                                     round((eval(parse(text =
                                                         col_age)) / 3.156e+7),
                                           3)))
  return(disease_dt_to_years)
}

#' Remove NIN (NA, Infinitive, NaN) values
#'
#' Function that removes a rows if the selected column
#' values includes NA, Infinitive or NaN
#' @param disease_data The disease data
#' @param col_name Column name in the disease data to evaluate
#' @return The clean data without NA, Infinitive or NaN values
#' @examples
#' disease_data <- import_linelist_disease_year(2019, "DENGUE")
#' disease_data <- clean_header(disease_data)
#' remove_nin_values(disease_data, col_name = "edad")
#' @export
remove_nin_values <- function(disease_data, col_name) {
  ref_col <- paste0("disease_data$", col_name)
  disease_data_del <- disease_data
  del_rows <-
    which(ifelse(is.na(eval(parse(text = ref_col))), TRUE,
                 ifelse(is.nan(eval(parse(text = ref_col))), TRUE,
                        is.infinite(eval(parse(text = ref_col))))))
  if (length(del_rows) > 0) disease_data_del <- disease_data[-del_rows]
  return(disease_data_del)
}

#' Remove error dates
#'
#' Function that removes dates greater than the comparison value
#' @param disease_data The disease data
#' @param col_init Column name of the date
#' @param col_cmp Column name of the comparison date
#' @return The data without the erroneous dates
#' @examples
#' disease_data <- import_linelist_disease_year(2019, "DENGUE")
#' disease_data <- clean_header(disease_data)
#' remove_error_dates(disease_data, col_init = "ini_sin", col_cmp = "fec_hos")
#' @export
remove_error_dates <- function(disease_data,
                               col_init = "ini_sin",
                               col_cmp = "fec_hos") {
  ref_col_init <- paste0("disease_data$", col_init)
  ref_col_cmp <- paste0("disease_data$", col_cmp)
  del_rows <- which(ref_col_cmp <= ref_col_init)
  disease_data_del <- disease_data[-del_rows]
  return(disease_data_del)
}

#' Format dates
#'
#' Function that gives a specific format to a date
#' @param disease_data The disease data
#' @param date_format Date format
#' @param col_names Column names to format
#' @return The data with formatted dates
#' @examples
#' disease_data <- import_linelist_disease_year(2020, "DENGUE")
#' disease_data <- clean_header(disease_data)
#' format_dates_values(disease_data,
#'                    date_format = "%AAAA-%MM-%DD",
#'                    col_names = c("ini_sin", "fec_hos"))
#' @export
format_dates_values <- function(disease_data,
                                date_format = "%AAAA-%MM-%DD",
                                col_names = c()) {
  clean_dates_disease_dt <- disease_data
  for (name in col_names) {
    if (!is.null(name)) {
      ref_col <- eval(parse(text = paste0("clean_dates_disease_dt$", name)))
      ref_col <- as.Date(ref_col, format = date_format)
    }
  }
  return(clean_dates_disease_dt)
}

#' Clean the header labels of the disease data
#'
#' Function that cleans the header labels of the disease data
#' @param disease_data The disease data
#' @return Formatted header labels
#' @examples
#' disease_data <- import_linelist_disease_year(2019, "DENGUE")
#' clean_header(disease_data)
#' @export
clean_header <- function(disease_data) {
  names(disease_data) <- epitrix::clean_labels(names(disease_data))
  return(disease_data)
}

#' Clean dates of the disease data
#'
#' Function that cleans dates from the disease data
#' @param disease_data The disease data
#' @param year Year of the disease data
#' @param date_format Date format
#' @param col_name Data set column name
#' @param col_cmp Data set column compare
#' @return The data with clean dates
#' @examples
#' disease_data <- import_linelist_disease_year(2020, "DENGUE")
#' disease_data <- clean_header(disease_data)
#' clean_disease_dates(disease_data,
#'                      year = 2020,
#'                      date_format = "%AAAA-%MM-%DD",
#'                      col_name = "ini_sin",
#'                      col_cmp = "fec_hos")
#' @export
clean_disease_dates <- function(disease_data,
                                year,
                                date_format = "%AAAA-%MM-%DD",
                                col_name = "ini_sin",
                                col_cmp = NULL) {
  disease_dt_by_onset_sym <- disease_data
  if (!is.null(col_cmp)) {
    disease_dt_by_onset_sym <-
      remove_error_dates(disease_dt_by_onset_sym,
                         col_name,
                         col_cmp)
  }
  disease_dt_by_onset_sym[order(eval(parse(text =
                                             paste0("disease_dt_by_onset_sym$",
                                                    col_name))),
                                decreasing = TRUE), ]
  disease_dt_by_onset_sym <- disease_dt_by_onset_sym[format(eval(
    parse(text = paste0("disease_dt_by_onset_sym$", col_name))
  ), "%Y") == year, ]
  return(disease_dt_by_onset_sym)
}

#' Clean ages of the disease data
#'
#' Function that cleans ages of the disease data
#' @param disease_data The disease data
#' @param col_name Column name in the disease data that contains the ages
#' @return The disease data with clean ages
#' @examples
#' disease_data <- import_linelist_disease_year(2020, "DENGUE")
#' disease_data <- clean_header(disease_data)
#' clean_disease_ages(disease_data, col_name = "edad")
#' @export
clean_disease_ages <- function(disease_data, col_name = "edad") {
  disease_data_by_years <- parse_age_to_years(disease_data)
  disease_data_by_years <- remove_nin_values(disease_data_by_years, col_name)
}

#' Clean outliers from disease data columns
#'
#' Function that clean outliers from disease data columns
#' @param disease_data The disease data
#' @return The disease data with clean columns
#' @examples
#' disease_data <- import_linelist_disease_year(2020, "DENGUE")
#' disease_data <- clean_header(disease_data)
#' clean_disease_cols(disease_data)
#' @export
clean_disease_cols <- function(disease_data) {
  diseases_cols <- config::get(file =
                                 system.file("extdata",
                                             "config.yml",
                                             package = "sivirep"),
                               "diseases_exceptions")
  disease_code <- disease_data$cod_eve[1]
  if (disease_code > 0) {
    for (event in diseases_cols) {
      code <- names(event)
      if (disease_code %in% code) {
        cols <- event[as.character(disease_code)]
        for (cols_names in cols) {
          for (col in cols_names) {
            disease_data[eval(parse(text = col))] <- NA
          }
        }
      }
    }
  }
}

#' Clean SIVIGILA data
#'
#' Function that cleans the selected disease data of SIVIGILA source
#' @param disease_data The disease data
#' @param year Year of the disease data
#' @return Clean data of the disease
#' @examples
#' year <- 2019
#' disease_data <- import_linelist_disease_year(year, "DENGUE")
#' cleansing_sivigila_data(disease_data, year)
#' @export
cleansing_sivigila_data <- function(disease_data, year) {
  disease_data <- clean_header(disease_data)
  disease_data <- clean_disease_ages(disease_data)
  dates_column_names <- config::get(file = system.file("extdata",
                                                       "config.yml",
                                                       package = "sivirep"),
                                    "dates_column_names")
  clean_disease_data <- format_dates_values(disease_data,
                                            col_names = dates_column_names)
  clean_disease_data <- clean_disease_dates(clean_disease_data, year,
                                            col_name = dates_column_names[3],
                                            col_cmp = dates_column_names[4])
  clean_disease_data <- clean_disease_dates(clean_disease_data, year,
                                            col_name = dates_column_names[2])
  clean_disease_data <- standardize_geo_codes(clean_disease_data)
  clean_disease_data <- clean_disease_cols()
  clean_disease_data <- parse_age_to_years(clean_disease_data,
                                           col_age = "edad",
                                           col_uni_met = "uni_med")
  return(clean_disease_data)
}
