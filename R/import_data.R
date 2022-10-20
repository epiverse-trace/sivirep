#' @import dplyr
#' @import config
#' @import xml2
#' @import stringr
#' @import httr

#' Import SIVIGILA Summary Data
#'
#' Función que importa la informacion de SIVIGILA a través de una URL
#' Function that imports SIVIGILA data through a URL
#' @param url_data URL of SIVIGILA data
#' @return The data downloaded in csv format
#' @examples
#' import_sivigila_summary_data("https://www.datos.gov.co/api/views/qvnt-2igj/rows.csv?accessType=DOWNLOAD")
#' @export
import_sivigila_summary_data <- function(url_data = config::get("sivigila_open_data_path")) {
  data <- utils::read.csv(url_data)
  return(data)
}

#' Import Geo Data
#'
#' Función que importa los nombres y códigos de los departamentos de Colombia a través de una URL
#' Function that imports the names and codes of the departments and municipalities of Colombia through a URL
#' @param url_data URL of geographical data
#' @return A data frame with the names and codes of the departments and municipalities of Colombia in csv format
#' @examples
#' import_geo_codes("https://www.datos.gov.co/api/views/gdxc-w37w/rows.csv?accessType=DOWNLOAD")
#' @export
import_geo_codes <- function(url_data = config::get("geo_data_path")) {
  data <- utils::read.csv(url_data)
  return(data)
}

#' Import Data Delim
#'
#' Función que identifica el separador con el que viene la informacion desde SIVIGILA para poderla tabular
#' Function that identifies the separator with which the information comes to be able to tabulate it
#' @param path_data Path or URL of SIVIGILA data
#' @return A data frame
#' @examples
#' import_data_delim("https://www.datos.gov.co/api/views/qvnt-2igj/rows.csv?accessType=DOWNLOAD")
#' @export
import_data_delim <- function(path_data) {
  delims <- config::get("data_delim")
  data <- data.frame()
  for (delim in delims) {
       if (delim %in% data.table::strsplit(readLines(path_data, n = 1)[1], split = "")[[1]] ) {
            data <- data.table::fread(path_data, sep = delim)
            break;
        }
  }

  if (plyr::empty(data)) {
       data <- data.table::fread(path_data)
  }

  return(data)
}

#' Import Data Endemic Channel
#'
#' Función que importa la informacion del SIVIGILA para la construcción del canal endémico
#' Function that imports SIVIGILA for building the endemic channel
#' @param path_data Path or URL of SIVIGILA data
#' @param disease_name The disease name
#' @param year Last year
#' @return The last five years data of a disease
#' @examples
#' import_data_endemic_channel("MALARIA", 2020)
#' @export
import_data_endemic_channel <- function(disease_name, year) {
  disease_data_by_years <- data.frame()
  return(disease_data_by_years)
}

#' Import Avaliable Diseases and Years
#'
#' Función que obtiene las enfermedades y los años disponibles de los microdatos de SIVIGILA
#' Function that obtains the diseases and the years available from the SIVIGILA microdata
#' @return The diseases and the years available
#' @examples
#' import_avaliable_diseases_and_years()
#' @export
import_avaliable_diseases_and_years <- function()  {

  query_diseases_by_year_path <- config::get("query_diseases_by_year_path")
  get_query_diseases_by_year  <- httr::GET(query_diseases_by_year_path, httr::add_headers("Accept" = "*/*"))

  content_type_response <- stringr::str_split_fixed(httr::headers(get_query_diseases_by_year)$`content-type`, pattern = ";", 3)
  content_type_response <- stringr::str_replace(content_type_response[[1]], "atom\\+", "")
  query_diseases_by_year_content <- httr::content(get_query_diseases_by_year, type = content_type_response, encoding = "UTF-8")


  children      <- xml2::xml_children(query_diseases_by_year_content)
  children      <- xml2::xml_children(children)
  children      <- xml2::xml_children(children)
  children      <- xml2::xml_children(children)
  children_text <- xml2::xml_text(children)

  data_avaliable <- data.frame(
    disease = unique(children_text),
    year = unique(children_text),
    stringsAsFactors = FALSE
  )

  years <- c(data_avaliable$year[1:20])
  years <- years[-2:-3]
  diseases <- data_avaliable$disease[21:95]

  data_avaliable_diseases_and_years <- list(disease = diseases, year = years)

  return(data_avaliable_diseases_and_years)
}

#' Import Data of a Disease By Year
#'
#' Función que obtiene los datos de una enfermedad por año
#' Function that obtains the data of a disease by year
#' @param year The selected year
#' @param disease_name The disease name
#' @param cache Indicates if the downloaded data to be cached
#' @return The disease data by year
#' @examples
#' import_data_disease_by_year(2018, "DENGUE")
#' @export
import_data_disease_by_year <- function(year, disease_name, cache = TRUE) {
  data_url <- get_path_data_disease_by_year(year, disease_name)
  data_disease_by_year <- data.frame()
  data_file_name <- paste0(tempdir(), "/", get_name_file_path(data_url))

  if (cache) {
      if (file.exists(data_file_name)) {
           data_disease_by_year <- utils::read.csv(file = data_file_name) 
      }
      else{
           data_disease_by_year <- import_data_delim(data_url)
           utils::write.csv(data_disease_by_year, file = data_file_name) 
      }
  }
  else {
      data_disease_by_year <- import_data_delim(data_url)
  }
  return(data_disease_by_year)
}

#' Get Name File Path or URL
#'
#' Función que obtiene el nombre del archivo a descargar desde una URL o ruta
#' Function that gets the file name for download from a URL or path
#' @param path The path or URL
#' @return The name file
#' @examples
#' find_name_file_path("DENGUE")
#' @export
get_name_file_path <- function(path) {
  name_file <- strsplit(path, config::get("name_file_split"))
  name_file <- strsplit(name_file[[1]][2], "')")[[1]][1] %>% as.character()
  return(name_file)
}