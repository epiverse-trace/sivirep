library(dplyr)
config   <- config::get()
temp_dir <- tempdir()

#' Import Path Data
#'
#' Función que importa la informacion de SIVIGILA a través de una URL
#' Function that imports SIVIGILA data through a URL
#' @param url_data URL of SIVIGILA data
#' @return The data downloaded in csv format
#' @examples
#' import_path_data("https://www.datos.gov.co/api/views/qvnt-2igj/rows.csv?accessType=DOWNLOAD")
#' @export
import_path_data <- function(url_data, cache = TRUE) {
  data <- read.csv(url_data)
  return(data)
}

#' Import Data Delim
#'
#' Función que importa la informacion del SIVIGILA y la tabula a traves de un delimitador
#' Function that imports the SIVIGILA information and tabulates it through a delimiter
#' @param path_data Path of SIVIGILA data
#' @return Data tabulated
#' @examples
#' import_data_delim(https://www.datos.gov.co/api/views/qvnt-2igj/rows.csv?accessType=DOWNLOAD)
#' @export
import_data_delim <- function(path_data) {
  delims <- config::get("data_delim")
  data <- data.frame()
  for (delim in delims) {
       if (delim %in% strsplit(readLines(path_data, n = 1)[1], split = "")[[1]] ) {
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
#' @param path_data Path of SIVIGILA data
#' @param disease_name Disease name
#' @param year Last year
#' @return The 5 years data of a disease
#' @examples
#' import_data_endemic_channel("MALARIA", 2020)
#' @export
import_data_endemic_channel <- function(disease_name, year) {
  file_path <- "../data/malaria/complicada/datos_"
  initial_year <- year - 4

  disease_data <- import_data_delim(paste(file_path, "_495.csv", sep =  as.character(year)))
  disease_data_by_years <- data.frame(group_by_week(disease_data))

  initial_year <- initial_year + 1
  while (initial_year < year) {
     disease_data   <- import_data_delim(paste(file_path, "_495.csv", sep =  as.character(initial_year)))
     disease_data_by_years <- cbind(disease_data_by_years, cases_count = group_by_week(disease_data)$cases_count)
     initial_year <- initial_year + 1

  }

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
#' Función que obtiene los datos de la enfermedad por año
#' Function that obtains the disease data by year
#' @param year The year
#' @param disease_name The disease name
#' @return The disease data by year
#' @examples
#' import_data_disease_by_year(2018, "DENGUE")
#' @export
import_data_disease_by_year <- function(year, disease_name, cache = TRUE) {
  data_url <- get_path_data_disease_by_year(year, disease_name)
  data_disease_by_year <- data.frame()
  data_file_name <- paste0(temp_dir, "/", find_name_file_path(data_url))

  if (cache) {
      if (file.exists(data_file_name)) {
           data_disease_by_year <- read.csv(file = data_file_name)
      }
      else{
           data_disease_by_year <- import_data_delim(data_url)
           write.csv(data_disease_by_year, file = data_file_name)
      }
  }
  else {
      data_disease_by_year <- import_data_delim(data_url)
  }
  return(data_disease_by_year)
}

#' Find Name File Path or URL
#'
#' Función que obtiene el nombre del archivo a descargar desde una URL o ruta
#' Function that obtains the name of the file to download from a URL or path
#' @param path The path or URL
#' @return The name file
#' @examples
#' find_name_file_path("DENGUE")
#' @export
find_name_file_path <- function(path) {
  name_file <- strsplit(path, config::get("name_file_split"))
  name_file <- strsplit(name_file[[1]][2], "')")[[1]][1] %>% as.character()
  return(name_file)
}
