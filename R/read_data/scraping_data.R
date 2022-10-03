#' Función que obtiene las enfermedades y los años disponibles de los microdatos de SIVIGILA
#' Function that obtains the diseases and the years available from the SIVIGILA microdata
#' @return The diseases and the years available
#' @examples 
#' get_avaliable_diseases_and_years()
get_avaliable_diseases_and_years() <- {
  
  query_diseases_by_year_path <- "https://portalsivigila.ins.gov.co/_api/web/lists/GetByTitle('Microdatos')/items?$select=Evento,A_x00f1_o,NombreEvento&$orderby=NombreEvento%20asc&$top=1000"
  get_query_diseases_by_year  <- httr::GET(query_diseases_by_year_path, add_headers("Accept" = "*/*"))
  
  content_type_response <- str_split_fixed(headers(get_query_diseases_by_year)$`content-type`, pattern = ";", 3)
  content_type_response <- str_replace(content_type_response[[1]], "atom\\+", "")
  query_diseases_by_year_content <- content(get_query_diseases_by_year, type = content_type_response, encoding = "UTF-8")
  
  
  children      <- xml_children(query_diseases_by_year_content)
  children      <- xml_children(children)
  children      <- xml_children(children)
  children      <- xml_children(children)
  children_text <- xml_text(children)
  
  data_avaliable_diseases_and_years <- data.frame( 
    title = unique(children_text),
    stringsAsFactors = FALSE
  )
  
  return(data_avaliable_diseases_and_years)
  
}

#' Función que obtiene los datos de la enfermedad por año
#' Function that obtains the disease data by year
#' @param year The year
#' @param disease_name The disease name
#' @return The disease data by year
#' @examples
#' get_data_disease_by_year(2018, "DENGUE")
get_data_disease_by_year <- function(year, disease_name) {
  
  base_path <- "https://portalsivigila.ins.gov.co/_api/"
  file_path_parameters <- "/$value?binaryStringResponseBody=true"
  
  microdata_path <- "web/lists/GetByTitle('Microdatos')/"
  query_path <- "items?$select=*,FileRef&$filter=(A_x00f1_o%20eq%20%27_year_%27)and(NombreEvento%20eq%20%27_disease_%27)"
  
  year <- as.character(year)
  
  disease_name <- str_replace_all(string = disease_name, pattern = " ", repl = "%20")
  query_path   <- str_replace(query_path, "_year_", year)
  query_path   <- str_replace(query_path, "_disease_", disease_name)

  query_disease_path <- paste(base_path, paste(microdata_path, query_path, sep = ""), sep = "")
  get_query_disease <- httr::GET(query_disease_path, add_headers("Accept" = "*/*"))
  
  content_type_response <- str_split_fixed(headers(get_query_disease)$`content-type`, pattern = ";", 3)
  content_type_response <- str_replace(content_type_response[[1]], "atom\\+", "")
  query_disease_content <- content(get_query_disease, type = content_type_response, encoding = "UTF-8")
  
  entry_nodes_query_disease_content <- xml_child(query_disease_content, 4)
  edit_node_query_disease_content   <- xml_child(entry_nodes_query_disease, 12)
  list_file_path <- xml_attr(edit_node_query_disease_content, "href")[1]
  
  get_info_file <- httr::GET(paste(base_path, list_file_path,  sep = ""), add_headers("Accept" = "*/*"))
  
  content_type_response <- str_split_fixed(headers(get_info_file)$`content-type`, pattern = ";", 3)
  content_type_response <- str_replace(content_type_response[[1]], "atom\\+", "")
  info_file_content <- content(get_info_file, type = content_type_response, encoding = "UTF-8")

  edit_node_info_file_content <- xml_child(info_file_content, 3)
  file_path <- xml_attr(edit_node_info_file_content, "href")[1]
  
  file_download_path <- paste(base_path, paste(file_path, file_path_parameters, sep = ""), sep = "")
  file_download_path
  
  return(file_download_path)
}