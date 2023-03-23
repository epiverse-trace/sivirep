#' Filter Disease
#'
#' Función que filtra en un conjunto de datos por el nombre de la enfermedad
#' Function that filters in a dataset by the disease name
#' @param name_disease Name of the disease
#' @param sivigila_summary_data Data set
#' @return The filtered data with the disease selected
#' @examples
#' sivigila_summary_data <- import_sivigila_summary_data()
#' filter_disease("MALAR", sivigila_summary_data)
#' @export
filter_disease  <- function(name_disease, sivigila_summary_data) {
  if ("conteo_casos" %in% names(sivigila_summary_data)) {
      names(sivigila_summary_data)[names(sivigila_summary_data) == "conteo_casos"] = "Casos"
  }
  list_diseases <- unique(sivigila_summary_data$Nombre)
  list_specific <- list_diseases[stringr::str_detect(list_diseases, name_disease) == TRUE]
  filtered_data <- sivigila_summary_data %>% dplyr::filter(.data$Nombre %in% list_specific)
  return(filtered_data)
}

#' Get Department Codes
#'
#' Función que obtiene el listado de departamentos de Colombia con su nombre y código
#' Function that gets the list of departments of Colombia with their name and code
#' @param geo_codes Geographical codes (Colombia departments and municipalities)
#' @return The Departments data with code and name
#' @examples
#' geo_codes <- import_geo_codes()
#' get_depto_codes(geo_codes)
#' @export
get_depto_codes <- function(geo_codes) {
  deptos_data   <- geo_codes %>% dplyr::group_by(cod_dep = .data$codigo_departamento, name_dep = .data$nombre_departamento) %>%
    dplyr::select(.data$cod_dep, .data$name_dep) %>% dplyr::distinct()
  deptos_data   <- deptos_data[1:33,]
  return(deptos_data)
}

#' Get Special Population and Cases
#'
#' Función que obtiene los casos por tipo de población especial de una enfermedad
#' Function that gets the cases by special population type of a disease
#' @param disease_data Disease data
#' @return The cases by special population type of a disease
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' get_special_population_and_cases(disease_data)
#' @export
get_special_population_and_cases <- function(disease_data) {
  special_populations <- config::get(file = system.file("extdata", "config.yml", package = "sivirep"), "special_populations_cols")
  special_populations_names <- config::get(file = system.file("extdata", "config.yml", package = "sivirep"), "special_populations_names")
  special_cases <- c()
  for (sp in special_populations) {
    special_cases <- append(special_cases, sum(eval(parse(text = paste0("disease_data$", sp)))))
  }
  
  disease_data_special_population <- data.frame(Poblacion = special_populations, Casos = special_cases, Nombre = special_populations_names)
  return(disease_data_special_population)
}

#' Group By Week and Cases
#'
#' Función que agrupa los datos por semana epidemiológica y número de casos
#' Function that groups the data by epidemiological week and number of cases
#' @param disease_data Disease data
#' @return The disease data grouped by week
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' group_by_week_and_cases(disease_data)
#' @export
group_by_week_and_cases <- function(disease_data) {
  disease_data_grouped  <- disease_data %>% dplyr::group_by(.data$SEMANA) %>% dplyr::summarise(Casos = sum(.data$UNI_MED))
  disease_data_grouped  <- disease_data_grouped[1:52,]
  return(disease_data_grouped)
}

#' Group By Columns and Cases
#'
#' Función que agrupa los datos por el nombre de las columna(s) y los casos
#' Function that groups the data by a specific column name and cases
#' @param disease_data Disease data
#' @param col_names The column name(s)
#' @param wt_percentage Indicates if it is required to add a percentage of cases as a column
#' @return The disease data grouped by a specific column name(s) and cases
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' group_by_columns_and_cases(disease_data, col_names = "SEXO", wt_percentage = TRUE)
#' group_by_columns_and_cases(disease_data, col_names = c("SEXO","SEMANA"))
#' @export
group_by_columns_and_cases <- function(disease_data, col_names, wt_percentage = FALSE) {
  disease_data_grouped  <- disease_data %>% dplyr::group_by(dplyr::across(dplyr::all_of(col_names))) %>% dplyr::summarise(Casos = dplyr::n(), .groups = "drop")
  if (wt_percentage) {
      disease_data_grouped  <-  disease_data_grouped %>% dplyr::mutate(Porcentaje = round(disease_data_grouped$Casos/sum(disease_data_grouped$Casos)*100, 1))
  }
  return(disease_data_grouped)
}

#' Group By Age Range and Cases
#'
#' Función que agrupa los datos por un rango de edades y por el número de casos
#' Function that groups the data by an age range and cases number
#' @param disease_data Disease data
#' @param col_name Colunm name
#' @param var_a Additional variables
#' @param min_val  Min value to group
#' @param max_val  Max value to group
#' @param step Step for the range
#' @return The disease data grouped by the age range and cases
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' disease_dt_by_age <- group_by_columns_and_cases(disease_data, c("EDAD", "SEMANA"), 
#' wt_percentage = TRUE)
#' group_by_age_range_and_cases(disease_dt_by_age, "EDAD", min_val = 0, 
#' max_val = max(disease_dt_by_age$EDAD), step = 10)
#' @export
group_by_age_range_and_cases <- function(disease_data, col_name, var_a = NULL, min_val, max_val, step) {
  data_values_range <- data.frame()
  if (!is.null(var_a) & length(var_a) > 0 ) {
      data_values_range <-  disease_data %>%
        dplyr::mutate(ranges = cut(.data$EDAD,
                                   seq(min_val, max_val, step))) %>%
        dplyr::group_by_("ranges", var_a) %>%
        dplyr::summarize(Casos = sum(.data$Casos), .groups = "drop") %>% as.data.frame()
      names(data_values_range)[names(data_values_range) == "ranges" ] <- col_name
  }
  else {
      data_values_range <-  disease_data %>%
        dplyr::mutate(ranges = cut(.data$EDAD,
                                   seq(min_val, max_val, step))) %>%
        dplyr::group_by_("ranges") %>%
        dplyr::summarize(Casos = sum(.data$Casos), .groups = "drop") %>% as.data.frame()
      names(data_values_range)[names(data_values_range) == "ranges" ] <- col_name
  }
  return(data_values_range)
}


#' Group By Columns and Cases
#'
#' Función que agrupa los datos por el nombre de las columna(s) y los casos
#' Function that groups the data by a specific column name and cases
#' @param disease_data Disease data
#' @param col_names The column name(s)
#' @param wt_percentage Indicates if it is required to add a percentage of cases as a column
#' @return The disease data grouped by a specific column name(s) and cases
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' group_by_columns_and_cases(disease_data, col_names = "SEXO", wt_percentage = TRUE)
#' group_by_columns_and_cases(disease_data, col_names = c("SEXO","SEMANA"))
#' @export
group_by_columns_and_cases <- function(disease_data, col_names, wt_percentage = FALSE) {
  disease_data_grouped  <- disease_data %>% dplyr::group_by(dplyr::across(dplyr::all_of(col_names))) %>% dplyr::summarise(Casos = dplyr::n(), .groups = "drop")
  if (wt_percentage) {
    disease_data_grouped  <-  disease_data_grouped %>% dplyr::mutate(Porcentaje = round(disease_data_grouped$Casos/sum(disease_data_grouped$Casos)*100, 1))
  }
  return(disease_data_grouped)
}