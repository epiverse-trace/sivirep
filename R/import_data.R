#' Importar datos resumidos del SIVIGILA
#'
#' Función que importa datos resumidos de SIVIGILA a través de una URL
#' @param url_data Un character (cadena de caracteres) que contiene
#' la URL de los datos de SIVIGILA; su valor por defecto es NULL
#' @return Un `data.frame` con los datos descargados
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
#' @param url_data Un character (cadena de caracteres) que contiene
#' la URL de los datos geográficos; su valor por defecto es NULL
#' @return Un `data.frame` con los nombres y códigos de los departamentos
#' y municipios de Colombia
#' @examples
#' import_geo_cods(url_data =
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
#' Función que identifica el separador que tiene
#' los datos para tabularla
#' @param path_data Un character (cadena de caracteres) que contiene
#' la URL de los datos de SIVIGILA
#' @return Un `data.frame` con los datos
#' @examples
#' import_sep_data(path_data =
#' "https://www.datos.gov.co/api/views/qvnt-2igj/rows.csv?accessType=DOWNLOAD")
#' @export
import_sep_data <- function(path_data) {
  seps <- config::get(file = system.file("extdata", "config.yml",
                                         package = "sivirep"), "data_delim")
  data <- data.frame()
  response <- httr::GET(path_data)
  if (httr::status_code(response) == 200) {
    start_file_name <- stringr::str_locate(path_data, "Microdatos/")[2] + 1
    end_file_name <- stringr::str_locate(path_data, "value")[1] - 5
    file_name <- stringr::str_sub(path_data, start_file_name, end_file_name)
    con_file <- file(file_name, "wb")
    chunk <- httr::content(response, "raw", as = "raw")
    if (length(chunk) > 0) {
      writeBin(chunk, con_file)
    }
    close(con_file)
    if (stringr::str_detect(file_name, ".xls")) {
      data <- readxl::read_excel(file_name)
    } else {
      for (sep in seps) {
        if (sep %in% strsplit(readLines(path_data, n = 1)[1], split = "")[[1]]) {
          data <- data.table::fread(path_data, sep = sep)
          break
        }
      }
      if (nrow(data) == 0) {
        data <- data.table::fread(path_data)
      }
    }
  }
  
  return(data)
}

#' Importar datos de una enfermedad o evento para construir
#' su canal endémico
#'
#' Función que importa los datos necesarios de una enfermedad
#' o evento para construir el canal endémico desde la fuente
#' de SIVIGILA
#' @param nombre_event Un character (cadena de caracteres) que
#' contiene el nombre de la enfermedad o evento
#' @param year Un numeric (numerico) que contiene el año
#' de referencia para la descarga de los datos
#' @return Un `data.frame` con los datos de los últimos cinco años
#' de una enfermedad
#' @examples
#' import_data_canal_endemico(nombre_event = "MALARIA",
#'                            year = 2020)
#' @export
import_data_canal_endemico <- function(nombre_event, year) {
  event_data <- data.frame()
  return(event_data)
}

#' Importar las enfermedades y años disponibles disposibles
#' para su descarga desde los microdatos de SIVIGILA
#'
#' Función que obtiene las enfermedades y los años disponibles
#' para su descarga desde los microdatos de SIVIGILA
#' @return Una list con las enfermedades y los años disponibles
#' para su descarga desde los microdatos de SIVIGILA
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
  additional_diseases <- config::get(file =
                                       system.file("extdata",
                                                   "config.yml",
                                                   package = "sivirep"),
                                     "additional_diseases")
  name_diseases <- base::append(name_diseases, additional_diseases)
  years_diseases <- base::append(years_diseases, c("", ""))
  list_events <- data.frame(enfermedad = name_diseases,
                            aa = years_diseases)
  list_events <- list_events[order(list_events$enfermedad,
                                   decreasing = FALSE), ]
  return(list_events)
}

#' Importar los datos de una enfermedad o evento por año
#' desde los microdatos de SIVIGILA
#'
#' Función que obtiene los datos de una enfermedad por año
#' desde los microdatos de SIVIGILA
#' @param year Un numeric (numerico) con el año deseado para la descarga
#' de los datos
#' @param nombre_event Un character (cadena de caracteres) con el nombre de
#' la enfermedad o evento
#' @param cache Un boolean (TRUE o FALSE) que indica si los datos descargados
#' deben ser almacenados en caché; su valor por defecto es TRUE
#' @return Un `data.frame` con los datos de la enfermedad o evento seleccionado
#' por año desde los microdatos de SIVIGILA
#' @examples
#' import_data_event(2018, "DENGUE")
#' @export
import_data_event <- function(year,
                              nombre_event,
                              cache = TRUE) {
  data_event <- data.frame()
  list_events <- list_events()
  nombre_event <- stringr::str_to_title(nombre_event)
  grupo_events <-
    list_events[which(stringr::str_detect(list_events$enfermedad,
                                          substr(nombre_event,
                                                 1,
                                                 nchar(nombre_event) - 1))), ]
  for (event in grupo_events$enfermedad) {
    if (event != "MALARIA") {
      data_url <- get_path_data_disease_year(year, event)
      data_import <- import_sep_data(data_url)
      data_import <- limpiar_encabezado(data_import)
      data_import$fec_def <- as.character(data_import$fec_def)
      data_event <- rbind(data_event, data_import)
    }
  }
  return(data_event)
}

#' Obtener el nombre del archivo desde una URL
#'
#' Función que obtiene el nombre del archivo a descargar desde una URL o ruta
#' @param ruta Un character (cadena de caracteres) con la ruta o URL
#' de descarga
#' @return Un character (cadena de caracteres) con el nombre del
#' archivo a descargar
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
