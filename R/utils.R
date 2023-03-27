#' get_months_major_cases
#' 
#' Función que obtiene los meses con la cantidad mayor de casos
#' Function that gets the months with the largest number of cases
#' @param disease_data Disease data
#' @param col_dates Data set column dates name
#' @param col_cases Data set column cases name
#' @param top Top
#' @param concat_values Concatenate values
#' @return months major cases
#' @examples
#' disease_data <- import_data_disease_by_year(2020, "DENGUE")
#' get_months_major_cases(disease_data, col_dates, col_cases, top = 3, concat_values = T)
#' @export
get_months_major_cases <- function(disease_data, col_dates, col_cases = "casos", top = 3, concat_values = T)  {
  data_major_cases <- disease_data[order(eval(parse(text = paste0("disease_data$", col_cases))), decreasing = TRUE), ]
  if (nrow(data_major_cases) < top) {
    top <- nrow(data_major_cases)
  }
  
  data_major_cases <- data_major_cases[1:top, ]
  
  data_major_cases$Meses <- sapply(eval(parse(text = paste0("data_major_cases$", col_dates))), months)
  if (concat_values) {
    months_concat <- concatenate_values_with_token(as.character(data_major_cases$Meses)[1:top])
    return(months_concat)
  }
  return(data_major_cases)
}

#' get_depto_names
#' 
#' Función que obtiene los nombres de los departamentos
#' Function that gets the department names
#' @param disease_data Disease data
#' @return dataset with the department names
#' @examples
#' disease_data <- import_data_disease_by_year(2020, "DENGUE")
#' get_depto_names(disease_data)
#' @export
get_depto_names <- function(disease_data) {
  disease_data_deptos <- disease_data
  disease_data_deptos$codigo <- disease_data$id
  geo_country_data <- import_geo_codes()
  deptos_data <- data.frame(id = geo_country_data$c_digo_departamento, 
                            nombre = geo_country_data$nombre_departamento)
  i <- 1
  for (code in deptos_data$id) {
    disease_data_deptos$id[disease_data_deptos$id == code] <- deptos_data$nombre[i]
    i <- i + 1 
  }
  colnames(disease_data_deptos)[colnames(disease_data_deptos) == "id"] <- "nombre"
  disease_data_deptos <- disease_data_deptos[order(disease_data_deptos$nombre, decreasing = F), ]
  disease_data_deptos <- disease_data_deptos[5:nrow(disease_data_deptos), ]
  return(disease_data_deptos)
}

#' get_row_major_cases
#' 
#' Función que obtiene la fila con mayor numero de casos
#' Function that gets the row with the major number of cases
#' @param disease_data Disease data
#' @param col_name Data set column name
#' @param percentage Percentage value
#' @return row with major cases
#' @examples
#' disease_data <- import_data_disease_by_year(2020, "DENGUE")
#' get_major_cases(disease_data, col_name = "casos", percentage = T)
#' @export
get_major_cases <- function(disease_data, col_name = "casos", percentage = T) {
  data_major_cases <- disease_data[order(eval(parse(text = paste0("disease_data$", col_name))), decreasing = TRUE), ]
  data_major_cases <- data_major_cases[1, ]
  if (percentage) {
    data_major_cases$porcentaje <-  round((data_major_cases$casos[1]/sum(eval(parse(text = paste0("disease_data$", col_name))))) * 100, 2)
  }
  return(data_major_cases)
}

#' concatenate_values_with_token
#' 
#' Función que concantena valores con un separador o token especifico
#' Function that concatenates values with a specific separator or token
#' @param values Values
#' @param length Length
#' @param main_token Main token
#' @param final_token Final token
#' @return Concatenated final value
#' @examples
#' concatenate_values_with_token(values = c("enero", "febrero", "marzo"), length = 3, main_token = ", ", final_token = "y ") 
#' @export
concatenate_values_with_token <- function(values, length = 3, main_token = ", ", final_token = "y ") {
  final_value <- ""
  i <- 1
  for (value in values) {
     if (i != length) final_value <- paste0(final_value, value, main_token)
     else final_value <- paste0(final_value, final_token, value)
     i <- i + 1
  }
  return(final_value)
}