#' Get months major cases
#'
#' Function that gets the months with the major number of cases
#' @param disease_data The disease data
#' @param col_dates Column names in the disease data that contains the dates
#' @param col_cases Column name in the disease data that contains
#' the cases number
#' @param top Top
#' @param concat_values Indicates if it is required to concatenate
#' the months as a string
#' @return Months major cases
#' @examples
#' disease_data <- import_linelist_disease_year(2020, "DENGUE")
#' disease_data <- clean_header(disease_data)
#' cases_onset_symptoms <- group_onset_symptoms(disease_data, type = "day")
#' get_months_most_cases(cases_onset_symptoms,
#'                        col_dates = "ini_sin",
#'                        col_cases = "casos",
#'                        top = 3,
#'                        concat_values = TRUE)
#' @export
get_months_most_cases <- function(disease_data,
                                  col_dates,
                                  col_cases = "casos",
                                  top = 3,
                                  concat_values = TRUE) {
  data_major_cases <- disease_data[order(eval(
    parse(text = paste0("disease_data$", col_cases))),
    decreasing = TRUE), ]
  if (nrow(data_major_cases) < top) {
    top <- nrow(data_major_cases)
  }
  data_major_cases <- data_major_cases[1:top, ]
  data_major_cases$Meses <- sapply(eval(
    parse(text = paste0("data_major_cases$",
                        col_dates))),
    months)
  if (concat_values) {
    months_concat <- concatenate_values_token(
      as.character(data_major_cases$Meses)[1:top])
    return(months_concat)
  }
  return(data_major_cases)
}

#' Get department names
#'
#' Function that gets the department names
#' @param disease_data The disease data
#' @return Dataframe with the department names
#' @examples
#' disease_data <- import_linelist_disease_year(2020, "DENGUE")
#' @export
get_depto_names <- function(disease_data) {
  disease_data_deptos <- disease_data
  disease_data_deptos$codigo <- disease_data$id
  geo_country_data <- import_geo_codes()
  deptos_data <- data.frame(
    id = geo_country_data$c_digo_departamento,
    nombre = geo_country_data$nombre_departamento
  )
  i <- 1
  for (code in deptos_data$id) {
    disease_data_deptos$id[disease_data_deptos$id == code] <-
                                            deptos_data$nombre[i]
    i <- i + 1
  }
  colnames(disease_data_deptos)[colnames(disease_data_deptos) == "id"] <-
                                                                    "nombre"
  disease_data_deptos <- disease_data_deptos[order(disease_data_deptos$nombre,
                                                   decreasing = FALSE), ]
  disease_data_deptos <- disease_data_deptos[5:nrow(disease_data_deptos), ]
  return(disease_data_deptos)
}

#' Get row with major cases
#'
#' Function that gets the row with the major number of cases
#' @param disease_data The disease data
#' @param col_name Column name in the disease data that contains
#' the cases number
#' @param percentage Indicates if it is required to add a percentage of
#' cases as a column
#' @return Row with major cases
#' @examples
#' disease_data <- import_linelist_disease_year(2020, "DENGUE")
#' disease_data <- clean_header(disease_data)
#' cases_sex <- group_sex(disease_data,
#'                        percentage = TRUE)
#' get_most_cases(cases_sex,
#'                col_name = "casos",
#'                percentage = TRUE)
#' @export
get_most_cases <- function(disease_data,
                           col_name = "casos",
                           percentage = TRUE) {
  data_major_cases <- disease_data[order(eval(
    parse(text = paste0("disease_data$",
                        col_name))),
    decreasing = TRUE), ]
  data_major_cases <- data_major_cases[1, ]
  if (percentage) {
    data_major_cases$porcentaje <- round((data_major_cases$casos[1] /
                                            sum(eval(
                                              parse(
                                                text = paste0("disease_data$",
                                                              col_name)
                                              )))) * 100,
                                         2)
  }
  return(data_major_cases)
}

#' Concatenate values with separator or token
#'
#' Function that concatenates values with a specific separator or token
#' @param values The values
#' @param length Values length
#' @param main_token Main separator or token
#' @param final_token Final separator or token
#' @return Concatenated final value
#' @examples
#' concatenate_values_token(values = c("enero", "febrero", "marzo"),
#'                          length = 3,
#'                          main_token = ", ",
#'                          final_token = "y ")
#' @export
concatenate_values_token <- function(values,
                                     length = 3,
                                     main_token = ", ",
                                     final_token = "y ") {
  final_value <- ""
  i <- 1
  for (value in values) {
    if (i != length) {
      final_value <- paste0(final_value, value, main_token)
    } else {
      final_value <- paste0(final_value, final_token, value)
    }
    i <- i + 1
  }
  return(final_value)
}

#' Get geographic information of the disease data
#'
#' Function that standardizes the geographic codes of the disease data
#' @param code_disease The disease code
#' @return The geographic codes of the disease data
#' @examples
#' get_geo_occurrence_type(code_disease = 210)
#' @export
get_geo_occurrence_type <- function(code_disease) {
  geo_occurrences <- config::get(file =
                                system.file("extdata", "config.yml",
                                            package = "sivirep"),
                      "occurrence_geo_diseases")
  col_ocurrences <- c("cod_dpto_o", "cod_mpio_o")
  if (grep(code_disease, geo_occurrences$cod_dpto_o) > 0) {
    col_ocurrences <- c("cod_dpto_o", "cod_mpio_o")
  }
  else if (grep(code_disease, geo_occurrences$cod_dpto_r) > 0) {
    col_ocurrences <- c("cod_dpto_r", "cod_mpio_r")
  }
}

#' Get geographic information of the disease data
#'
#' Function that standardizes the geographic codes of the disease data
#' @param department The disease data
#' @param municipalitie The disease data
#' @return The geographic codes of the disease data
#' @examples
#' get_info_depts(department = "ANTIOQUIA")
#' @export
get_info_depts <- function(department = NULL, municipalitie = NULL) {
  geo_data <- import_geo_codes()
  
  list_departments <- unique(geo_data$nombre_departamento)
  list_specific <- list_departments[
    stringr::str_detect(list_departments, toupper(department)) == TRUE]
  dept_data <- dplyr::filter(geo_data, .data$nombre_departamento %in%
                               list_specific)
  if (!is.null(municipalitie)) {
    list_municipalities <- unique(geo_data$nombre_municipio)
    list_specific <- list_municipalities[
      stringr::str_detect(list_municipalities, toupper(municipalitie)) == TRUE]
    dept_data <- dplyr::filter(geo_data, .data$nombre_municipio %in%
                                 list_specific)
  }
  return(dept_data)
}

#' Set geographic information of the disease data
#'
#' Function that standardizes the geographic codes of the disease data
#' @param code_dept The disease data
#' @param code_mpio The disease data
#' @return The geographic codes of the disease data
#' @examples
#' set_code_mpio(code_dept = 01, code_mpio = "001")
#' @export
set_code_mpio <- function(code_dept, code_mpio) {
  code_mpio <- as.character(code_mpio)
  if (substr(code_dept, 1, 1) == "0") {
    code_dept <- substr(code_dept, 2, 2)
    code_mpio <- gsub(code_dept, "", code_mpio) 
  }
  return(code_mpio)
}