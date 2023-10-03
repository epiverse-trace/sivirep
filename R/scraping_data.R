#' Get the download path for a disease by specific year
#'
#' Function that gets the path or URL of the SIVIGILA service to
#' download the data of a disease for a specific year
#' @param year Specific year to download
#' @param disease_name Disease name to download
#' @return Path or URL for download the disease data by a specific year of
#' SIVIGILA source
#' @examples
#' get_path_data_disease_year(year = 2010, disease_name = "DENGUE")
#' @export
get_path_data_disease_year <- function(year, disease_name) {
  config_file <- system.file("extdata", "config.yml", package = "sivirep")
  base_path <- config::get(file = config_file, "base_path_microdata")
  file_path <- config::get(file = config_file, "file_path_microdata")
  file_path_parameters <- config::get(file = config_file,
                                      "file_path_parameters_microdata")
  microdata_path <- config::get(file = config_file, "path_microdata")
  query_path <- config::get(file = config_file, "query_path_microdata")
  year <- as.character(year)
  disease_name <- stringr::str_replace_all(string = disease_name,
                                           pattern = " ",
                                           replacement = "%20")
  query_path <- stringr::str_replace(query_path, "_year_", year)
  query_path <- stringr::str_replace(query_path, "_disease_", disease_name)
  query_disease_path <- paste(base_path,
                              paste(microdata_path,
                                    query_path,
                                    sep = ""),
                              sep = "")
  get_query_disease <- httr::GET(query_disease_path,
                                 httr::add_headers("Accept" = "*/*"))
  content_type_response <-
    stringr::str_split_fixed(httr::headers(get_query_disease)$`content-type`,
                             pattern = ";",
                             3)
  content_type_response <- stringr::str_replace(content_type_response[[1]],
                                                "atom\\+",
                                                "")
  query_disease_content <- httr::content(get_query_disease,
                                         type = content_type_response,
                                         encoding = "UTF-8")
  entry_nodes_disease_content <- xml2::xml_child(query_disease_content, 4)
  edit_node_disease_content <-
    xml2::xml_child(entry_nodes_disease_content,
                    18)
  properties_node_content <-
    xml2::xml_child(edit_node_disease_content,
                    1)
  file_ref_node_disease_content <-
    xml2::xml_child(properties_node_content,
                    17)
  file_ref_disease_contents <- xml2::xml_contents(file_ref_node_disease_content)
  file_ref_disease <- xml2::xml_text(file_ref_disease_contents)
  file_path <- stringr::str_replace(file_path, "_filepath_", file_ref_disease)
  file_download_path <- paste0(base_path,
                               file_path, file_path_parameters,
                               sep = "")
  return(file_download_path)
}
