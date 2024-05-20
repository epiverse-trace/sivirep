#' Get the download path for a disease by specific year
#'
#' Function that gets the path or URL of the SIVIGILA service to
#' download the data of a disease for a specific year
#' @param year Specific year to download
#' @param disease_name Disease name to download
#' @return Path or URL for download the disease data by a specific year of
#' SIVIGILA source
#' @keywords internal
obtener_ruta_data_event_year <- function(nombre_event, year) {
  archivo_config <- system.file("extdata", "config.yml", package = "sivirep")
  ruta_base <- config::get(file = archivo_config, "base_path_microdata")
  ruta_archivo <- config::get(file = archivo_config, "file_path_microdata")
  ruta_archivo_params <- config::get(file = archivo_config,
                                     "file_path_parameters_microdata")
  ruta_microdata <- config::get(file = archivo_config, "path_microdata")
  ruta_query <- config::get(file = archivo_config, "query_path_microdata")
  year <- as.character(year)
  nombre_event <- utils::URLencode(nombre_event)
  ruta_query <- stringr::str_replace(ruta_query, stringr::fixed("_year_"),
                                     year)
  ruta_query <- stringr::str_replace(ruta_query, stringr::fixed("_disease_"),
                                     nombre_event)
  ruta_query_event <- paste0(ruta_base,
                             paste0(ruta_microdata,
                                    ruta_query))
  solicitud_query_event <- httr2::request(ruta_query_event)
  get_query_event <- httr2::req_perform(solicitud_query_event)
  respuesta_query_event <- httr2::resp_body_string(get_query_event)
  respuesta_document <- xml2::as_xml_document(respuesta_query_event)
  ref_archivo_propiedad <- xml2::xml_find_all(respuesta_document, "//d:FileRef")
  ref_archivo_event <- xml2::xml_text(ref_archivo_propiedad)
  if (length(ref_archivo_event) >= 2) {
    ref_archivo_event <- ref_archivo_event[
      which(stringr::str_detect(ref_archivo_event,
                                stringr::fixed(year)))]
  }
  ruta_archivo <- stringr::str_replace(ruta_archivo,
                                    stringr::fixed("_filepath_"),
                                    ref_archivo_event)
  ruta_descarga_archivo <- paste0(ruta_base,
                               ruta_archivo, ruta_archivo_params)
  return(ruta_descarga_archivo)
}
