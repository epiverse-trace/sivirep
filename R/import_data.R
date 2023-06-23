#' Import SIVIGILA summary data
#'
#' Function that imports SIVIGILA data through a URL
#' @param url_data URL of SIVIGILA data
#' @return The data downloaded in csv format
#' @examples
#' import_sivigila_summary_data()
#' @export
import_sivigila_summary_data <- function(url_data = NULL) {
  if (is.null(url_data)) {
    url_data <- config::get(
      file = system.file("extdata", "config.yml",
                         package = "sivirep"),
      "sivigila_open_data_path")
  }
  data <- utils::read.csv(url_data)
  return(data)
}

#' Import geographical data of Colombia
#'
#' Function that imports the names and codes of the departments and
#' municipalities of Colombia through a URL
#' @param url_data URL of geographical data
#' @return A data frame with the names and codes of the departments and
#' municipalities of Colombia in csv format
#' @examples
#' import_geo_codes(
#'  "https://www.datos.gov.co/api/views/gdxc-w37w/rows.csv?accessType=DOWNLOAD")
#' @export
import_geo_codes <- function(url_data = NULL) {
  if (is.null(url_data)) {
    url_data <- config::get(
      file = system.file("extdata", "config.yml",
                         package = "sivirep"),
      "geo_data_path")
  }
  data <- utils::read.csv(url_data)
  names(data) <- epitrix::clean_labels(names(data))
  return(data)
}

#' Import data with a specific separator
#'
#' Function that identifies the separator with which the information
#' comes to be able to tabulate it
#' @param path_data Path or URL of SIVIGILA data
#' @return A data frame
#' @examples
#' import_data_separator(
#'  "https://www.datos.gov.co/api/views/qvnt-2igj/rows.csv?accessType=DOWNLOAD")
#' @export
import_data_separator <- function(path_data) {
  delims <- config::get(file = system.file("extdata", "config.yml",
                                           package = "sivirep"), "data_delim")
  data <- data.frame()
  for (delim in delims) {
    if (delim %in% strsplit(readLines(path_data, n = 1)[1], split = "")[[1]]) {
      data <- data.table::fread(path_data, sep = delim)
      break
    }
  }
  if (nrow(data) == 0) {
    data <- data.table::fread(path_data)
  }
  return(data)
}

#' Import data of a disease to build its endemic channel
#'
#' Function that imports the last five years of a disease data for building
#' the endemic channel from SIVIGILA source
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

#' Import diseases and years available for download from the SIVIGILA microdata
#'
#' Function that obtains the diseases and the years available from
#' the SIVIGILA microdata source
#' @return The diseases and the years available from the SIVIGILA
#' microdata source
#' @examples
#' list_available_diseases_years()
#' @export
list_available_diseases_years <- function() {
  query_diseases_by_year_path <- config::get(file =
                                        system.file("extdata", "config.yml",
                                            package = "sivirep"),
                                            "query_diseases_by_year_path")
  get_query_diseases_by_year <- httr::GET(query_diseases_by_year_path,
                                          httr::add_headers("Accept" = "*/*"))
  content_type_response <- stringr::str_split_fixed(httr::headers(
    get_query_diseases_by_year)$`content-type`,
    pattern = ";", 3)
  content_type_response <- stringr::str_replace(
                              content_type_response[[1]],
                              "atom\\+", "")
  query_diseases_by_year_content <- httr::content(get_query_diseases_by_year,
                                                  type = content_type_response,
                                                  encoding = "UTF-8")
  children <- xml2::xml_children(query_diseases_by_year_content)
  children <- xml2::xml_children(children)
  children <- xml2::xml_children(children)
  children <- xml2::xml_children(children)
  children_text <- xml2::xml_text(children)
  i <- 2
  name_diseases <- c()
  years_diseases <- c()
  children <- children[-base::seq(3, length(children), 3)]
  children_text <- children_text[-base::seq(3, length(children_text), 3)]
  while (i < base::length(children)) {
    disease <- xml2::xml_text(children[i])
    name_diseases <- base::append(name_diseases, disease)
    tmp_diseases <- base::which(children_text == disease)
    tmp_years <- tmp_diseases - 1
    years_diseases <- base::append(years_diseases, base::toString(
      base::sort(
        children_text[tmp_years],
        decreasing = FALSE)))
    children <- children[-tmp_years]
    children_text <- children_text[-(tmp_diseases - 1)]
    children <- children[-base::which(children_text == disease)]
    children_text <- children_text[-base::which(children_text == disease)]
    i <- i + 2
  }
  list_available_diseases_years <- data.frame(enfermedad = name_diseases,
                                                  aa = years_diseases)
  return(list_available_diseases_years)
}

#' Import data of a disease by year from the SIVIGILA microdata source
#'
#' Function that obtains the data of a disease by year
#' @param year The selected year
#' @param disease_name The disease name
#' @param cache Indicates if the downloaded data to be cached
#' @return The disease data by year from the SIVIGILA microdata source
#' @examples
#' import_linelist_disease_year(2018, "DENGUE")
#' @export
import_linelist_disease_year <- function(year,
                                         disease_name,
                                         cache = TRUE) {
  data_url <- get_path_data_disease_year(year, disease_name)
  data_disease_by_year <- data.frame()
  data_disease_by_year <- import_data_separator(data_url)
  return(data_disease_by_year)
}

#' Get name file path or URL
#'
#' Function that gets the file name for download from a URL or path
#' @param path The path or URL
#' @return The name file for download
#' @examples
#' get_name_file_path("DENGUE")
#' @export
get_name_file_path <- function(path) {
  name_file <- strsplit(path, config::get(file =
                                 system.file("extdata", "config.yml",
                                             package = "sivirep"),
                                          "name_file_split"))
  name_file <- strsplit(name_file[[1]][2], "')")[[1]][1] %>% as.character()
  return(name_file)
}
