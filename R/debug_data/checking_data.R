#' Select Disease
#' 
#' Función que seleccione una enfermedad por ejemplo Tuberculosis
#' Function that selects a disease for example Tuberculosis
#' @param name_disease Name of disease
#' @param data data set
#' @return The filtered data with the disease selected information
#' @examples
#' select_disease("MALARIA", data_sivigila)
#' @export
select_disease  <- function(name_disease, data) {
  list_diseases <- unique(data$Nombre)
  list_specific <- list_diseases[stringr::str_detect(list_diseases, name_disease) == TRUE]
  filtered_data <- data %>% filter(Nombre %in% list_specific)
  
  return(filtered_data)
}

#' Get Department Codes
#' 
#' Función que obtiene el listado de departamentos con su nombre y código
#' Function that obtains the list of departments with their name and code
#' @param deptos_data Departments data
#' @return The Departments data with code and name
#' @examples
#' get_depto_codes(deptos_data)
#' @export
get_depto_codes <- function(deptos_data) {
  deptos_data   <- deptos_data %>% dplyr::group_by(cod_dep = Código.Departamento, name_dep = Nombre.Departamento) %>%  
    dplyr::select(cod_dep, name_dep) %>% dplyr::distinct()
  deptos_data   <- deptos_data[1:33,]
  
  return(deptos_data)
}

#' Group By Week
#' 
#' Función que agrupa los datos por Semana
#' Function that groups the data by week
#' @param disease_data Disease data
#' @return The disease data grouped by week
#' @examples
#' group_by_week(disease_data)
#' @export
group_by_week <- function(disease_data) {
  disease_data_grouped  <- disease_data %>% dplyr::group_by(SEMANA) %>% dplyr::summarise(cases_count = sum(UNI_MED))
  disease_data_grouped  <- disease_data_grouped[1:52,]
  return(disease_data_grouped)
}

#' Group By Variable
#' 
#' Función que agrupa los datos por una variable(s) y/o por semana
#' Function that groups the data by a specific variable and/or week
#' @param disease_data Disease data
#' @param param 
#' @return The disease data grouped by week
#' @examples
#' group_by_variable(disease_data, week = "SEMANA", wt_percentage = TRUE)
#' @export
group_by_variable <- function(disease_data, var, week = NULL, wt_percentage = FALSE) {
  if (!is.null(week) & length(week) > 0 ) {
     disease_data_grouped  <- disease_data %>% dplyr::group_by_(week, var) %>% dplyr::summarise(Casos = dplyr::n(), .groups = "drop")
  }
  else {
    disease_data_grouped  <- disease_data %>% dplyr::group_by_(var) %>% dplyr::summarise(Casos = dplyr::n(), .groups = "drop")
  }
  
  if (wt_percentage) {
      disease_data_grouped  <-  disease_data_grouped %>% dplyr::mutate(Porcentaje = round(Casos/sum(Casos)*100, 1))
  }
  
  if (var == "SEMANA") {
      disease_data_grouped  <- disease_data_grouped[1:52,] %>% as.data.frame()
  }
  #names(disease_data_grouped )[names(disease_data_grouped) == "var" ] <- var
  return(disease_data_grouped)
}

#' Group By Range
#' 
#' Función que agrupa los datos por un rango
#' Function that groups the data by a range
#' @param disease_data The disease data
#' @param var The variable to group
#' @param min_val The min value to group
#' @param max_val The max value to group
#' @param step The step for the range
#' @return The disease data grouped by week
#' @examples
#' group_by_range(disease_data, min_val = 2020-10-10, max_val = 2022-12-10, step = 5) 
#' @export
group_by_range <- function(disease_data, var, var_a = NULL, min_val, max_val, step) {
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