#' Get Path Data Disease by Year
#' 
#' Función que obtiene la ruta o URL del servicio de SIVIGILA para descargar los datos de una enfermedad por un año especifico
#' Function that gets the path or URL of the SIVIGILA service to download the data of a disease for a specific year
#' @param year Specific year
#' @param disease_name Disease name
#' @return Path or URL for download the disease data by a specific year
#' @examples
#' get_path_data_disease_by_year(year = 2010, disease_name = "DENGUE")
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