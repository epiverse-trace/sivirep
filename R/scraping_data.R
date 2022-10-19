#' Get Path Data Disease by Year
#' 
#' Función que obtiene la ruta o URL del servicio para descargar la informacion de una enfermedad por año desde SIVIGILA
#' Function that obtains the path or URL of the service to download the information of a disease per year from SIVIGILA
#' @param year The year
#' @param disease_name The disease name
#' @return Path or URL for download the disease data by selected year
#' @examples
#' get_path_data_disease_by_year(2010, "DENGUE")
#' @export
get_path_data_disease_by_year <- function(year, disease_name) {
  base_path <- config::get("base_path_microdata")
  file_path_parameters <- config::get("file_path_parameters_microdata")
  
  microdata_path <- config::get("path_microdata")
  query_path <- config::get("query_path_microdata")
  
  year <- as.character(year)
  
  disease_name <- stringr::str_replace_all(string = disease_name, pattern = " ", repl = "%20")
  query_path   <- stringr::str_replace(query_path, "_year_", year)
  query_path   <- stringr::str_replace(query_path, "_disease_", disease_name)
  
  query_disease_path <- paste(base_path, paste(microdata_path, query_path, sep = ""), sep = "")
  get_query_disease <- httr::GET(query_disease_path, httr::add_headers("Accept" = "*/*"))
  
  content_type_response <- stringr::str_split_fixed(httr::headers(get_query_disease)$`content-type`, pattern = ";", 3)
  content_type_response <- stringr::str_replace(content_type_response[[1]], "atom\\+", "")
  query_disease_content <- httr::content(get_query_disease, type = content_type_response, encoding = "UTF-8")
  
  entry_nodes_query_disease_content <- xml2::xml_child(query_disease_content, 4)
  edit_node_query_disease_content   <- xml2::xml_child(entry_nodes_query_disease_content, 12)
  list_file_path <- xml2::xml_attr(edit_node_query_disease_content, "href")[1]
  
  get_info_file <- httr::GET(paste(base_path, list_file_path,  sep = ""), httr::add_headers("Accept" = "*/*"))
  
  content_type_response <- stringr::str_split_fixed(httr::headers(get_info_file)$`content-type`, pattern = ";", 3)
  content_type_response <- stringr::str_replace(content_type_response[[1]], "atom\\+", "")
  info_file_content <- httr::content(get_info_file, type = content_type_response, encoding = "UTF-8")
  
  edit_node_info_file_content <- xml2::xml_child(info_file_content, 3)
  file_path <- xml2::xml_attr(edit_node_info_file_content, "href")[1]
  
  file_download_path <- paste(base_path, paste(file_path, file_path_parameters, sep = ""), sep = "")

  return(file_download_path)
}