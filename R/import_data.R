#' Importar datos resumidos de SIVIGILA
#'
#' Función que importa datos de SIVIGILA a través de una URL
#' @param url_data URL de los datos de SIVIGILA
#' @return Los datos descargados en formato csv
#' @examples
#' import_data_resumen_sivigila()
#' @export
import_data_resumen_sivigila <- function(url_data = NULL) {
  if (is.null(url_data)) {
    url_data <- config::get(file =
                              system.file("extdata", "config.yml",
                                          package = "sivirep"),
                            "sivigila_open_data_path")
  }
  data <- utils::read.csv(url_data)
  return(data)
}

#' Importar datos geográficos de Colombia
#'
#' Función que importa los nombres y códigos de los departamentos
#' y municipios de Colombia a través de una URL
#' @param url_data URL de los datos geográficos
#' @return Un data frame con los nombres y códigos de los departamentos
#' y municipios de Colombia en formato csv
#' @examples
#' import_geo_cods(
#' "https://www.datos.gov.co/api/views/gdxc-w37w/rows.csv?accessType=DOWNLOAD")
#' @export
import_geo_cods <- function(url_data = NULL) {
  if (is.null(url_data)) {
    url_data <- config::get(file =
                              system.file("extdata",
                                          "config.yml",
                                          package = "sivirep"),
                            "geo_data_path")
  }
  data <- utils::read.csv(url_data)
  names(data) <- epitrix::clean_labels(names(data))
  return(data)
}

#' Importar datos con un separador específico
#'
#' Función que identifica el separador con el que
#' llega la información para poder tabularla
#' @param path_data Ruta o URL de los datos de SIVIGILA
#' @return Un data frame
#' @examples
#' import_sep_data(
#' "https://www.datos.gov.co/api/views/qvnt-2igj/rows.csv?accessType=DOWNLOAD")
#' @export
import_sep_data <- function(path_data) {
  seps <- config::get(file = system.file("extdata", "config.yml",
                                         package = "sivirep"), "data_delim")
  data <- data.frame()
  for (sep in seps) {
    if (sep %in% strsplit(readLines(path_data, n = 1)[1], split = "")[[1]]) {
      data <- data.table::fread(path_data, sep = sep)
      break
    }
  }
  if (nrow(data) == 0) {
    data <- data.table::fread(path_data)
  }
  return(data)
}

#' Importar datos de una enfermedad para construir su canal endémico
#'
#' Función que importa los últimos cinco años de datos de
#' una enfermedad para construir
#' el canal endémico desde la fuente de SIVIGILA
#' @param nombre_event El nombre del evento
#' @param year Año final
#' @return Los datos de los últimos cinco años de una enfermedad
#' @examples
#' import_data_canal_endemico("MALARIA", 2020)
#' @export
import_data_canal_endemico <- function(nombre_event, year) {
  event_data <- data.frame()
  return(event_data)
}

#' Importar una enfermedad o evento y años disponibles para descargar
#' desde los microdatos de SIVIGILA
#'
#' Función que obtiene las una enfermedad o evento y los años disponibles
#' desde los microdatos de SIVIGILA
#' @return Las una enfermedad o evento y los años disponibles desde
#' los microdatos de SIVIGILA
#' @examples
#' list_events()
#' @export
list_events <- function() {
  query_event_year_path <- config::get(file =
                                         system.file("extdata",
                                                     "config.yml",
                                                     package = "sivirep"),
                                       "query_diseases_by_year_path")
  query_event_year <- httr::GET(query_event_year_path,
                                httr::add_headers("Accept" = "*/*"))
  content_type_response <-
    stringr::str_split_fixed(httr::headers(query_event_year)$`content-type`,
                             pattern = ";",
                             3)
  content_type_response <-
    stringr::str_replace(content_type_response[[1]],
                         "atom\\+", "")
  query_event_year_content <- httr::content(query_event_year,
                                            type = content_type_response,
                                            encoding = "UTF-8")
  children <- xml2::xml_children(query_event_year_content)
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
    diseases <- base::which(children_text == disease)
    years <- diseases - 1
    years_diseases <-
      base::append(years_diseases,
                   base::toString(base::sort(children_text[years],
                                             decreasing = FALSE)))
    children <- children[-years]
    children_text <- children_text[-(diseases - 1)]
    children <- children[-base::which(children_text == disease)]
    children_text <- children_text[-base::which(children_text == disease)]
    i <- i + 2
  }
  list_events <- data.frame(enfermedad = name_diseases,
                            aa = years_diseases)
  return(list_events)
}

#' Importar datos de una enfermedad por año desde los microdatos de SIVIGILA
#'
#' Función que obtiene los datos de una enfermedad por año
#' @param year El año seleccionado
#' @param nombre_event El nombre de la enfermedad
#' @param cache Indica si los datos descargados deben ser almacenados en caché
#' @return Los datos de la enfermedad por año desde los microdatos de SIVIGILA
#' @examples
#' import_data_event(2018, "DENGUE")
#' @export
import_data_event <- function(year,
                              nombre_event,
                              cache = TRUE) {
  data_url <- get_path_data_disease_year(year, nombre_event)
  event_data <- data.frame()
  event_data <- import_sep_data(data_url)
  return(event_data)
}

#' Obtener el nombre del archivo de ruta o URL
#'
#' Función que obtiene el nombre del archivo a descargar desde una URL o ruta
#' @param ruta La ruta o URL
#' @return El nombre del archivo a descargar
#' @examples
#' obtener_ruta_descarga("DENGUE")
#' @export
obtener_ruta_descarga <- function(ruta) {
  nombre_archivo <- strsplit(ruta,
                             config::get(file =
                                           system.file("extdata", "config.yml",
                                                       package = "sivirep"),
                                         "name_file_split"))
  nombre_archivo <- strsplit(nombre_archivo[[1]][2],
                             "')")[[1]][1] %>% as.character()
  return(nombre_archivo)
}
