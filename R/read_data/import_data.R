library(dplyr)
library(tidyverse)
library(config)

#' Función que importa la informacion de SIVIGILA a través de una URL
#' Function that imports SIVIGILA data through a URL
#' @param url_data URL of SIVIGILA data
#' @return The data downloaded in csv format
#' @examples
#' import_sivigila_data("https://www.datos.gov.co/api/views/qvnt-2igj/rows.csv?accessType=DOWNLOAD")
import_sivigila_data <- function(url_data) {
  data <- read.csv(url_data)
  return(data)
}

#' Función que importa la informacion del SIVIGILA y la tabula a traves de un delimitador
#' Function that imports the SIVIGILA information and tabulates it through a delimiter
#' @param path_data Path of SIVIGILA data
#' @return Data tabulated
#' @examples
#' import_data_delim(https://www.datos.gov.co/api/views/qvnt-2igj/rows.csv?accessType=DOWNLOAD")
import_data_delim <- function(path_data) {
  data <- if ("|" %in% strsplit(readLines(path_data, n = 1)[1], split = "")[[1]] ) { 
            fread(path_data, sep = "|")  
        }else if ("," %in% strsplit(readLines(path_data, n = 1)[1], split = "")[[1]] ){
            fread(path_data, sep = ",")
        }else if (";" %in% strsplit(readLines(path_data, n = 1)[1], split = "")[[1]] ){
            fread(path_data, sep = ";")
        }else if (":" %in% strsplit(readLines(path_data, n = 1)[1], split = "")[[1]] ){
          fread(path_data, sep = ":")
        }else {
          fread(path_data)
        }
  return(data)
}

#' Función que importa la informacion del SIVIGILA para la construcción del canal endémico
#' Function that imports SIVIGILA for building the endemic channel
#' @param path_data Path of SIVIGILA data
#' @param disease_name Disease name
#' @param year Last year
#' @return The 5 years data of a disease
#' @examples
#' import_data_endemic_channel("MALARIA", 2020)
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

#' Función que obtiene las enfermedades y los años disponibles de los microdatos de SIVIGILA
#' Function that obtains the diseases and the years available from the SIVIGILA microdata
#' @return The diseases and the years available
#' @examples 
#' get_avaliable_diseases_and_years()
import_avaliable_diseases_and_years <- function()  {
  
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

#' Función que obtiene los datos de la enfermedad por año
#' Function that obtains the disease data by year
#' @param year The year
#' @param disease_name The disease name
#' @return The disease data by year
#' @examples
#' get_data_disease_by_year(2018, "DENGUE")
import_data_disease_by_year <- function(year, disease_name, cache = FALSE, use_cache_file = FALSE) {
  
  data_url <- get_path_data_disease_by_year(year, disease_name)
  data_disease_by_year <- data.frame()
  data_file_name <- find_name_file_path(data_url)
  
  if (!use_cache_file) {
      data_disease_by_year <- import_data_delim(data_url)
      if (cache) {
          write.csv(data_disease_by_year, file = paste0("../data/", data_file_name)) 
      }
  }
  else {
    data_disease_by_year <- read.csv(file = paste0("../data/", data_file_name))
  }
  
  return(data_disease_by_year)
}


find_name_file_path <- function(path) {
  name_file <- strsplit(path, "/Microdatos/")
  name_file <- strsplit(name_file[[1]][2], "')")[[1]][1] %>% as.character()
  return(name_file)
}