#' Función que seleccione una enfermedad por ejemplo Tuberculosis
#' Function that selects a disease for example Tuberculosis
#' @param name_disease Name of disease
#' @param data data set
#' @return The filtered data with the disease selected information
#' @examples
#' select_disease("MALARIA", data_sivigila)
select_disease  <- function(name_disease, data) {
  list_diseases <- unique(data$Nombre)
  list_specific <- list_diseases[stringr::str_detect(list_diseases, name_disease) == TRUE]
  filtered_data <- data %>% filter(Nombre %in% list_specific)
  
  return(filtered_data)
}

#' Función que obtiene el listado de departamentos con su nombre y código
#' Function that obtains the list of departments with their name and code
#' @param deptos_data Departments data
#' @return The Departments data with code and name
#' @examples
#' get_depto_codes(deptos_data)
get_depto_codes <- function(deptos_data) {
  deptos_data   <- deptos_data %>% group_by(cod_dep = Código.Departamento, name_dep = Nombre.Departamento) %>%  
    select(cod_dep, name_dep) %>% distinct()
  deptos_data   <- deptos_data[1:33,]
  
  return(deptos_data)
}

#' Función que agrupa los datos por Semana
#' Function that groups the data by week
#' @param disease_data Disease data
#' @return The disease data grouped by week
#' @examples
#' group_by_week(disease_data)
group_by_week <- function(disease_data) {
  disease_data_grouped  <- disease_data %>% group_by(SEMANA) %>% dplyr::summarise(cases_count = sum(UNI_MED))
  disease_data_grouped  <- disease_data_grouped[1:52,]
  return(disease_data_grouped)
}