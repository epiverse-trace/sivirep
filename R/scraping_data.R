#' Get the download path for a disease by specific year
#'
#' Function that gets the path or URL of the SIVIGILA service to
#' download the data of a disease for a specific year
#' @param year Specific year to download
#' @param disease_name Disease name to download
#' @return Path or URL for download the disease data by a specific year of
#' SIVIGILA source
#' @keywords internal
get_path_data_disease_year <- function(year, disease_name) {
  config_file <- system.file("extdata", "config.yml", package = "sivirep")
  base_path <- config::get(file = config_file, "base_path_microdata")
  file_path <- config::get(file = config_file, "file_path_microdata")
  file_path_parameters <- config::get(file = config_file,
                                      "file_path_parameters_microdata")
  microdata_path <- config::get(file = config_file, "path_microdata")
  query_path <- config::get(file = config_file, "query_path_microdata")
  year <- as.character(year)
  disease_name <- utils::URLencode(disease_name)
  query_path <- stringr::str_replace(query_path, stringr::fixed("_year_"),
                                     year)
  query_path <- stringr::str_replace(query_path, stringr::fixed("_disease_"),
                                     disease_name)
  query_disease_path <- paste0(base_path,
                              paste0(microdata_path,
                                    query_path))
  query_disease_request <- httr2::request(query_disease_path)
  query_disease_get <- httr2::req_perform(query_disease_request)
  query_disease_response <- httr2::resp_body_string(query_disease_get)
  response_document <- xml2::as_xml_document(query_disease_response)
  property_file_ref <- xml2::xml_find_all(response_document, "//d:FileRef")
  disease_file_ref <- xml2::xml_text(property_file_ref)
  if (length(disease_file_ref) >= 2) {
    disease_file_ref <- disease_file_ref[
      which(stringr::str_detect(disease_file_ref,
                                stringr::fixed(year)))]
  }
  file_path <- stringr::str_replace(file_path,
                                    stringr::fixed("_filepath_"),
                                    disease_file_ref)
  file_download_path <- paste0(base_path,
                               file_path, file_path_parameters)
  return(file_download_path)
}
