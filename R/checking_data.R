#' Filter Disease
#'
#' Función que filtra en un conjunto de datos por el nombre de la enfermedad
#' Function that filters in a dataset by the disease name
#' @param name_disease Name of the disease
#' @param data Data set
#' @return The filtered data with the disease selected
#' @examples
#' filter_disease(name_disease = "MALARIA", sivigila_summary_data)
#' @export
filter_disease  <- function(name_disease, sivigila_summary_data) {
  list_diseases <- unique(sivigila_summary_data$Nombre)
  list_specific <- list_diseases[stringr::str_detect(list_diseases, name_disease) == TRUE]
  filtered_data <- sivigila_summary_data %>% dplyr::filter(.data$Nombre %in% list_specific)
  return(filtered_data)
}

#' Get Department Codes
#'
#' Función que obtiene el listado de departamentos de Colombia con su nombre y código
#' Function that gets the list of departments of Colombia with their name and code
#' @param deptos_data Departments data
#' @return The Departments data with code and name
#' @examples
#' get_depto_codes(geo_codes)
#' @export
get_depto_codes <- function(geo_codes) {
  deptos_data   <- geo_codes %>% dplyr::group_by(cod_dep = .data$Código.Departamento, name_dep = .data$Nombre.Departamento) %>%
    dplyr::select(cod_dep, name_dep) %>% dplyr::distinct()
  deptos_data   <- deptos_data[1:33,]
  return(deptos_data)
}

#' Group By Week and Cases
#'
#' Función que agrupa los datos por semana epidemiológica y número de casos
#' Function that groups the data by epidemiological week and number of cases
#' @param disease_data Disease data
#' @return The disease data grouped by week
#' @examples
#' group_by_week_and_cases(disease_data)
#' @export
group_by_week_and_cases <- function(disease_data) {
  disease_data_grouped  <- disease_data %>% dplyr::group_by(SEMANA) %>% dplyr::summarise(cases_count = sum(UNI_MED))
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
#' group_by_columns_and_cases(disease_data, col_names = "SEXO", wt_percentage = TRUE)
#' group_by_columns_and_cases(disease_data, col_names = c("SEXO","SEMANA"))
#' @export
group_by_columns_and_cases <- function(disease_data, col_names, wt_percentage = FALSE) {
  disease_data_grouped  <- disease_data %>% dplyr::group_by(across(all_of(col_names))) %>% dplyr::summarise(Casos = dplyr::n(), .groups = "drop")
  if (wt_percentage) {
      disease_data_grouped  <-  disease_data_grouped %>% dplyr::mutate(Porcentaje = round(Casos/sum(Casos)*100, 1))
  }
  return(disease_data_grouped)
}

#' Group By Age Range and Cases
#'
#' Función que agrupa los datos por un rango de edades y por el número de casos
#' Function that groups the data by an age range and cases number
#' @param disease_data Disease data
#' @param col_name Colunm name
#' @param min_val  Min value to group
#' @param max_val  Max value to group
#' @param step Step for the range
#' @return The disease data grouped by the age range and cases
#' @examples
#' group_by_age_range_and_cases(disease_data, min_val = 0, max_val = 100, step = 5)
#' @export
group_by_age_range_and_cases <- function(disease_data, var, var_a = NULL, min_val, max_val, step) {
  data_values_range <- data.frame()
  if (!is.null(var_a) & length(var_a) > 0 ) {
      data_values_range <-  disease_data %>%
        dplyr::mutate(ranges = cut(disease_data$EDAD,
                                   seq(min_val, max_val, step))) %>%
        dplyr::group_by_("ranges", var_a) %>%
        dplyr::summarize(Casos = sum(Casos), .groups = "drop") %>% as.data.frame()
      names(data_values_range)[names(data_values_range) == "ranges" ] <- var
  }
  else {
      data_values_range <-  disease_data %>%
        dplyr::mutate(ranges = cut(disease_data$EDAD,
                                   seq(min_val, max_val, step))) %>%
        dplyr::group_by_("ranges") %>%
        dplyr::summarize(Casos = sum(Casos), .groups = "drop") %>% as.data.frame()
      names(data_values_range)[names(data_values_range) == "ranges" ] <- var
  }
  return(data_values_range)
}