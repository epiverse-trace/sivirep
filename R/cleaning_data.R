#' Clean Depto Disease Codes
#' 
#' Función que limpia la información de la enfermedad respecto a los codigos de los departamentos
#' Function that cleans the disease data with respect to the codes of the departments
#' @param depto_codes The departments data
#' @param disease_data The disease data
#' @return Clean codes of disease data departments
#' @examples
#' clean_depto_disease_codes(depto_codes, disease_data)
#' @export
clean_depto_disease_codes <- function(depto_codes, disease_data, make_group = TRUE) {
  depto_codes$id      <- as.character(depto_codes$cod_dep)
  disease_data$id     <- as.character(disease_data$COD_DPTO_O)
  disease_data_clean  <- disease_data
  
  if (make_group)
      disease_data_clean  <- disease_data %>% dplyr::group_by(id) %>% dplyr::summarise(casos = sum(conteo_casos))
  
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

#' Parse Age to Years
#' 
#' Función que convierte las edades en años según las unidades de medida de SIVIGILA
#' Function that converts ages into years according to SIVIGILA measurement units
#' @param disease_data Disease data
#' @param col_age Age column name  
#' @param col_uni_med Unit of measure column name 
#' @return The ages in years
#' @examples
#' parse_age_to_years(disease_data, col_age = "EDAD", col_uni_med = "UNI_MED")
#' @export
parse_age_to_years <- function(disease_data, col_age = "EDAD", col_uni_med = "UNI_MED") {
  disease_dt_to_years <- disease_data %>%
                          dplyr::mutate(
                                  EDAD = dplyr::case_when(
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
#' remove_nin_values(disease_data, name_col = "EDAD")
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
#' remove_error_dates(disease_data, col_init = "INI_SI", col_cmp = "FEC_HOS")
#' @export
remove_error_dates <- function(disease_data, col_init = "INI_SI", col_cmp = "FEC_HOS") {
  ref_col_init  <- paste0("disease_data$", col_init) 
  ref_col_cmp  <- paste0("disease_data$", col_cmp) 
  del_rows <- which(ref_col_cmp <= ref_col_init)
  disease_data_del <- disease_data[-del_rows]
  return(disease_data_del)
}