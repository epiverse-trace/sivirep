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