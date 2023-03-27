#' Clean Depto Disease Codes
#' 
#' Función que limpia la información de la enfermedad respecto a los codigos de los departamentos
#' Function that cleans the disease data with respect to the codes of the departments
#' @param depto_codes The department codes
#' @param disease_data The disease data
#' @param make_group Indicates if it is necessary to group by department IDs and case numbers
#' @return Clean codes of disease data departments
#' @examples
#' geo_codes <- import_geo_codes()
#' depto_codes <- get_depto_codes(geo_codes)
#' disease_data <-  import_data_disease_by_year(2019, "DENGUE")
#' disease_data <- group_by_columns_and_cases(disease_data, "cod_dpto_o", wt_percentage = TRUE)
#' clean_depto_disease_codes(depto_codes, disease_data)
#' @export
clean_depto_disease_codes <- function(depto_codes, disease_data, make_group = TRUE) {
  depto_codes$id      <- as.character(depto_codes$cod_dep)
  disease_data$id     <- as.character(disease_data$cod_dpto_o)
  disease_data_clean  <- disease_data
  
  if (make_group)
      disease_data_clean  <- disease_data %>% dplyr::group_by(.data$id) %>% dplyr::summarise(casos = sum(.data$casos))
  
  disease_data_clean$id[
    nchar(disease_data_clean$id) < 2 & disease_data_clean$id != "1" & disease_data_clean$id != "0" 
    &  paste("0", disease_data_clean$id, sep = "") %in% depto_codes$id
  ] <-  paste("0", disease_data_clean$id[
    nchar(disease_data_clean$id) < 2 & disease_data_clean$id != "1" & disease_data_clean$id != "0" 
    &  paste("0", disease_data_clean$id, sep = "") %in% depto_codes$id
  ], sep = "")
  disease_data_clean$id[disease_data_clean$id == "1" &  paste("1", disease_data_clean$id, sep = "") %in% depto_codes$id] <- "11"
  
  return(disease_data_clean)
}

#' clean_depto_codes
#' 
#' Función que limpia los codigos de los departamentos de los datos de la enfermedad
#' Function that cleans the departments codes of the disease data
#' @param disease_data The disease data
#' @param col_data_codes Column name of departments codes of the disease data
#' @param geo_codes Geographical data that includes the departments codes
#' @param col_geo_codes Column name of departments codes of the geographical data
#' @return Clean codes of disease data departments
#' @examples
#' geo_codes <- import_geo_codes()
#' disease_data <-  import_data_disease_by_year(2019, "DENGUE")
#' clean_depto_codes(disease_data, col_data_codes = "cod_dpto_o", geo_codes, col_geo_codes = "c_digo_departamento")
#' @export
clean_depto_codes <- function(disease_data, col_data_codes, geo_data, col_geo_codes) {
  col_detps_geo <- eval(parse(text = paste0("geo_data$", col_geo_codes)))
  col_detps_geo <- as.character(col_detps_geo)
  
  disease_data_clean  <- disease_data
  col_detps_data  <- eval(parse(text = paste0("disease_data_clean$", col_data_codes)))
  col_detps_data  <- as.character(col_detps_data)
  
  
  
  col_detps_data[
    nchar(col_detps_data) < 2 & col_detps_data != "1" & col_detps_data != "0" 
    &  paste("0", col_detps_data, sep = "") %in% col_detps_geo
  ] <-  paste("0", col_detps_data[
    nchar(col_detps_data) < 2 & col_detps_data != "1" & col_detps_data != "0" 
    &  paste("0", col_detps_data, sep = "") %in% col_detps_geo
  ], sep = "")
  col_detps_data[col_detps_data == "1" &  paste("1", col_detps_data, sep = "") %in% col_detps_geo] <- "11"
  
  return(disease_data_clean)
}

#' Parse Age to Years
#' 
#' Función que convierte las edades en años según las unidades de medida de SIVIGILA
#' Function that converts ages into years according to SIVIGILA measurement units
#' @param disease_data Disease data
#' @param col_age Age column name  
#' @param col_uni_med Unit of measure column name 
#' @return The ages in years
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' parse_age_to_years(disease_data, col_age = "edad", col_uni_med = "uni_med")
#' @export
parse_age_to_years <- function(disease_data, col_age = "edad", col_uni_med = "uni_med") {
  disease_dt_to_years <- dplyr::mutate(disease_data,
                                   edad = dplyr::case_when(
                                    eval(parse(text = col_uni_med)) == 1 ~ round(eval(parse(text = col_age)), 3),
                                    eval(parse(text = col_uni_med)) == 2 ~ round((eval(parse(text = col_age))/12), 3),
                                    eval(parse(text = col_uni_med)) == 3 ~ round((eval(parse(text = col_age))/876), 3),
                                    eval(parse(text = col_uni_med)) == 4 ~ round((eval(parse(text = col_age))/525960), 3),
                                    eval(parse(text = col_uni_med)) == 5 ~ round((eval(parse(text = col_age))/3.156e+7), 3)
                                  )
                                )
  return(disease_dt_to_years)
}

#' Remove NIN (NA, Infinitive, NaN) Values
#' 
#' Función que elimina los valores (NA, Infinitive, NaN) de una columna
#' Function that eliminates the values (NA, Infinitive, NaN) of a column
#' @param disease_data Disease data
#' @param name_col Column name  
#' @return The clean data without NA, Infinitive or NaN values of the column
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' remove_nin_values(disease_data, name_col = "edad")
#' @export
remove_nin_values <- function(disease_data, name_col) {
  ref_col  <- paste0("disease_data$", name_col) 
  disease_data_del <- disease_data
  del_rows <- which(ifelse(is.na(eval(parse(text = ref_col))), TRUE, 
               ifelse(is.nan(eval(parse(text = ref_col))), TRUE, 
                      is.infinite(eval(parse(text = ref_col))))))
  if (length(del_rows) > 0) disease_data_del <- disease_data[-del_rows]
  return(disease_data_del)
}

#' Remove Error Dates
#' 
#' Función que elimina las fechas que son superiores al valor de comparación
#' Function that removes greater dates than the comparison value
#' @param disease_data Disease data
#' @param col_init Column name of the date
#' @param col_cmp Column name of comparison value
#' @return The data without the erronous dates
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' remove_error_dates(disease_data, col_init = "INI_SI", col_cmp = "fec_hos")
#' @export
remove_error_dates <- function(disease_data, col_init = "ini_sin", col_cmp = "fec_hos") {
  ref_col_init  <- paste0("disease_data$", col_init) 
  ref_col_cmp  <- paste0("disease_data$", col_cmp) 
  del_rows <- which(ref_col_cmp <= ref_col_init)
  disease_data_del <- disease_data[-del_rows]
  return(disease_data_del)
}

#' format_dates_values
#' 
#' Función que da un formato especifico a una fecha
#' Function that gives a specific format to a date
#' @param disease_data Disease data
#' @param date_format Date format
#' @param col_names Data set column names
#' @return The data with formatted dates
#' @examples
#' disease_data <- import_data_disease_by_year(2020, "DENGUE")
#' remove_error_dates(disease_data, date_format = "%AAAA-%MM-%DD", col_names = c("ini_sin", "fec_hos"))
#' @export
format_dates_values <- function(disease_data, date_format = "%AAAA-%MM-%DD", col_names = c()) {
  clean_dates_disease_dt <- disease_data
  for (name in col_names) {
    if (!is.null(name)) {
        ref_col <- eval(parse(text = paste0("clean_dates_disease_dt$", name)))
        ref_col <- as.Date(ref_col, format = date_format)
    }
  }
  return(clean_dates_disease_dt)
}

#' clean_disease_dates
#' 
#' Función que limpia las fechas de un conjunto de datos
#' Function that cleans dates from a dataset
#' @param disease_data Disease data
#' @param date_format Date format
#' @param col_name Data set column name
#' @param col_cmp Data set column compare
#' @return The data with clean dates
#' @examples
#' disease_data <- import_data_disease_by_year(2020, "DENGUE")
#' clean_disease_dates(disease_data, year, date_format = "%AAAA-%MM-%DD", col_name = "ini_sin", col_cmp = "fec_hos")
#' @export
clean_disease_dates <- function(disease_data, year, date_format = "%AAAA-%MM-%DD", col_name = "ini_sin", col_cmp = NULL) {
  #disease_dt_by_onset_sym <- format_dates_values(disease_data, date_format, c(col_name, col_cmp))
  disease_dt_by_onset_sym <- disease_data
  if (!is.null(col_cmp)) {
      disease_dt_by_onset_sym <- remove_error_dates(disease_dt_by_onset_sym, col_name, col_cmp) 
  }
  
  #disease_dt_by_onset_sym <- group_by_columns_and_cases(disease_dt_by_onset_sym, col_name)
  disease_dt_by_onset_sym[order(eval(parse(text = paste0("disease_dt_by_onset_sym$", col_name))), decreasing = TRUE), ]
  disease_dt_by_onset_sym <- disease_dt_by_onset_sym[format(eval(
    parse(text = paste0("disease_dt_by_onset_sym$", col_name))),'%Y') == year, ]
  return(disease_dt_by_onset_sym)
}

#' clean_disease_dates
#' 
#' Función que limpia las edades de un conjunto de datos
#' Function that cleans ages from a dataset
#' @param disease_data Disease data
#' @param col_name Data set column name
#' @return The data with clean ages
#' @examples
#' disease_data <- import_data_disease_by_year(2020, "DENGUE")
#' clean_disease_ages(disease_data, col_name = "edad")
#' @export
clean_disease_ages <- function(disease_data, col_name = "edad") {
  disease_data_by_years <- parse_age_to_years(disease_data)
  disease_data_by_years <- remove_nin_values(disease_data_by_years, col_name)
}

#' clean_sivigila_data
#' 
#' Función que limpia los datos de la enfermedad seleccionada de la fuente SIVIGILA
#' Function that cleans the selected disease data of source SIVIGILA
#' @param disease_data The disease data
#' @param year Year of the disease data
#' @return Clean data of disease
#' @examples
#' year <- 2019
#' disease_data <-  import_data_disease_by_year(year, "DENGUE")
#' clean_sivigila_data(disease_data, year)
#' @export
clean_sivigila_data <- function(disease_data, year) {
  names(disease_data) <- epitrix::clean_labels(names(disease_data))
  
  disease_data <- clean_disease_ages(disease_data);
  
  dates_column_names <- config::get(file = 
                                      system.file("extdata", "config.yml", 
                                                  package = "sivirep"), "dates_column_names")
  
  clean_disease_data <- format_dates_values(disease_data, col_names = dates_column_names)
  clean_disease_data <- clean_disease_dates(clean_disease_data, year, col_name = dates_column_names[3], col_cmp = dates_column_names[4]);
  clean_disease_data <- clean_disease_dates(clean_disease_data, year, col_name = dates_column_names[2]);
  #clean_disease_data <- clean_disease_dates(clean_disease_data, year, col_name = dates_column_names[5], col_cmp = dates_column_names[1]);
  
  
  depto_column_names <- config::get(file = 
                                      system.file("extdata", "config.yml", 
                                                  package = "sivirep"), "depto_column_names")
  geo_country_data <- import_geo_codes()
  clean_disease_data <- clean_depto_codes(clean_disease_data, col_data_codes = depto_column_names[1], 
                                geo_data = geo_country_data, col_geo_codes = "c_digo_departamento")
  
  clean_disease_data <- parse_age_to_years(disease_data, col_age = "edad", col_uni_med = "uni_med")
}