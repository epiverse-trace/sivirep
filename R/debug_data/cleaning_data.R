#' Función que limpia la información de la enfermedad respecto a los codigos de los departamentos
#' Function that cleans the disease data with respect to the codes of the departments
#' @param deptos_data Departments data
#' @param disease_data Disease data
#' @return The filtered data with the disease selected information
#' @examples
#' clean_depto_disease_codes(deptos_data, disease_data)
clean_depto_disease_codes <- function(deptos_data, disease_data, make_group = TRUE) {
  deptos_data$id      <- as.character(deptos_data$cod_dep)
  disease_data$id     <- as.character(disease_data$COD_DPTO_O)
  disease_data_clean  <- disease_data
  
  if (make_group)
      disease_data_clean  <- disease_data %>% group_by(id) %>% dplyr::summarise(casos = sum(conteo_casos))
  
  disease_data_clean$id[
    nchar(disease_data_clean$id) < 2 & disease_data_clean$id != "1" & disease_data_clean$id != "0" 
    &  paste("0", disease_data_clean$id, sep = "") %in% deptos_data$id
  ] <-  paste("0", disease_data_clean$id[
    nchar(disease_data_clean$id) < 2 & disease_data_clean$id != "1" & disease_data_clean$id != "0" 
    &  paste("0", disease_data_clean$id, sep = "") %in% deptos_data$id
  ], sep = "")
  disease_data_clean$id[disease_data_clean$id == "1" &  paste("1", disease_data_clean$id, sep = "") %in% deptos_data$id] <- "11"
  
  return(disease_data_clean)
}