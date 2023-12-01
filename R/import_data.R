#' Importar datos geográficos de Colombia
#'
#' Función que importa los nombres y códigos de los departamentos
#' y municipios de Colombia a través de una URL
#' @param descargar Un `boolean` (TRUE o FALSE) que indica si los datos
#' se deben descargar desde la API de datos abiertos; su valor por
#' defecto es `FALSE`
#' @return Un `data.frame` con los nombres y códigos de los departamentos
#' y municipios de Colombia
#' @examples
#' import_geo_cods(descargar = FALSE)
#' @export
import_geo_cods <- function(descargar = FALSE) {
  data_geo <- NULL
  if (descargar) {
    path_data <- config::get(file =
                              system.file("extdata",
                                          "config.yml",
                                          package = "sivirep"),
                            "geo_data_path")
    data_geo <- utils::read.csv(path_data)
    names(data_geo) <- epitrix::clean_labels(names(data_geo))
  } else {
    stopifnot("El parametro descargar debe ser un booleano"
              = is.logical(descargar))
    path_data <- config::get(file =
                              system.file("extdata",
                                          "config.yml",
                                          package = "sivirep"),
                            "divipola_data_path")
    archivo_geo <- system.file(path_data,
                               package = "sivirep")
    data_geo <- readxl::read_excel(archivo_geo)
  }
  return(data_geo)
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
  query_event_year <- httr2::request(query_event_year_path)
  query_event_year_response <- httr2::req_perform(query_event_year)
  query_event_year_content <- httr2::resp_body_xml(query_event_year_response)
  children <- xml2::xml_children(query_event_year_content)
  children <- xml2::xml_children(children)
  children <- xml2::xml_children(children)
  children <- xml2::xml_children(children)
  children_text <- xml2::xml_text(children)
  i <- 2
  name_diseases <- NULL
  years_diseases <- NULL
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
  name_diseases <- base::append(stringr::str_to_title(name_diseases),
                                additional_diseases)
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
#' @param year Un `numeric` (numerico) con el año deseado para la descarga
#' de los datos
#' @param nombre_event Un `character` (cadena de caracteres) con el nombre de
#' la enfermedad o evento
#' @param cache Un `boolean` (TRUE o FALSE) que indica si los datos descargados
#' deben ser almacenados en caché; su valor por defecto es TRUE
#' @return Un `data.frame` con los datos de la enfermedad o evento seleccionado
#' por año desde los microdatos de SIVIGILA
#' @examples
#' import_data_event(nombre_event = "DENGUE",
#'                   year = 2020,
#'                   cache = TRUE)
#' @export
import_data_event <- function(nombre_event,
                              year,
                              cache = TRUE) {
  stopifnot("El parametro year no debe estar vacio" = !missing(year),
            "El parametro year debe ser numerico" = is.numeric(year),
            "El parametro nombre_event no debe estar vacio"
            = !missing(nombre_event),
            "El parametro nombre_event debe ser una cadena de caracteres"
            = is.character(nombre_event),
            "El parametro cache debe ser un booleano"
            = is.logical(cache))
  data_event <- data.frame()
  list_events <- list_events()
  nombre_event <- stringr::str_to_title(nombre_event)
  list_events_relacionados <- config::get(file =
                                            system.file("extdata",
                                                        "config.yml",
                                                        package = "sivirep"),
                                          "related_diseases")
  cols_remover <- config::get(file =
                                system.file("extdata",
                                            "config.yml",
                                            package = "sivirep"),
                              "cols_remover")
  grupo_events <-
    list_events[which(stringr::str_detect(list_events$enfermedad,
                                          substr(nombre_event,
                                                 1,
                                                 nchar(nombre_event) - 1))), ]
  stopifnot("La enfermedad o evento no esta disponible para su descarga"
            = !(is.null(grupo_events) || nrow(grupo_events) == 0),
            "El year no esta disponible para su descarga"
            = stringr::str_detect(grupo_events$aa,
                                  as.character(year)))
  if (length(list_events_relacionados) > 0) {
    events_relacionados <- list_events_relacionados[[nombre_event]]
    for (event in events_relacionados) {
      grupo_events_relacionados <-
        list_events[which(list_events$enfermedad == event), ]
      if (is.null(grupo_events) || nrow(grupo_events) == 0) {
        warning("La enfermedad o evento relacionado: ",
                event,
                "no esta disponible para su descarga")
      } else if (stringr::str_detect(grupo_events_relacionados$aa,
                                     as.character(year))) {
        warning("El year: ", year,
                "de la enfermedad o evento relacionado: ",
                event,
                "no esta disponible para su descarga")
      } else {
        grupo_events <- rbind(grupo_events, grupo_events_relacionados)
      }
    }
  }
  for (event in grupo_events$enfermedad) {
    if (event != "Malaria") {
      data_url <- get_path_data_disease_year(year, event)
      data_import <- import_sep_data(data_url, cache)
      data_import <- limpiar_encabezado(data_import)
      data_import$fec_def <- as.character(data_import$fec_def)
      nombre_cols <- names(data_import)
      nombre_cols <- nombre_cols[-which(nombre_cols %in% cols_remover)]
      data_event <- rbind(data_event, data_import[, nombre_cols])
    }
  }
  return(data_event)
}

#' Importar datos con un separador específico
#'
#' Función que identifica el separador que tiene
#' los datos para tabularla
#' @param path_data Un `character` (cadena de caracteres) que contiene
#' la URL de los datos de SIVIGILA
#' @param cache Un `boolean` (TRUE o FALSE) que indica si los datos descargados
#' deben ser almacenados en caché; su valor por defecto es TRUE
#' @return Un `data.frame` con los datos
#' @keywords internal
import_sep_data <- function(path_data = NULL, cache = TRUE) {
  stopifnot("El parametro path_data debe ser una cadena de caracteres" =
              is.character(path_data),
            "El parametro cache debe ser un booleano"
            = is.logical(cache))
  data <- data.frame()
  extdata_path <- system.file("extdata", package = "sivirep")
  if (!is.null(path_data)) {
    start_file_name <- stringr::str_locate(path_data,
                                           stringr::fixed("Microdatos/"))[2] + 1
    end_file_name <- stringr::str_locate(path_data,
                                         stringr::fixed("value"))[1] - 5
    file_name <- stringr::str_sub(path_data, start_file_name, end_file_name)
    file_path <- file.path(extdata_path, file_name)
    if (!file.exists(file_path) || !cache) {
      file_request <- httr2::request(path_data)
      file_response <- httr2::req_perform(file_request)
      if (httr2::resp_status(file_response) == 200) {
        file_content <- httr2::resp_body_raw(file_response)
        con_file <- file(file_path, "wb")
        if (length(file_content) > 0) {
          writeBin(file_content, con_file)
        }
        close(con_file)
      }
    }
    if (stringr::str_detect(file_name, ".xls")) {
      data <- readxl::read_excel(file_path)
    }
  }
  return(data)
}

#' Obtener el nombre del archivo desde una URL
#'
#' Función que obtiene el nombre del archivo a descargar desde una URL o ruta
#' @param ruta Un `character` (cadena de caracteres) con la ruta o URL
#' de descarga
#' @return Un `character` (cadena de caracteres) con el nombre del
#' archivo a descargar
#' @keywords internal
obtener_ruta_descarga <- function(ruta) {
  stopifnot("El parametro ruta no debe estar vacio"
            = !missing(ruta),
            "El parametro ruta debe ser una cadena de caracteres"
            = is.character(ruta))
  nombre_archivo <- strsplit(ruta,
                             config::get(file =
                                           system.file("extdata", "config.yml",
                                                       package = "sivirep"),
                                         "name_file_split"))
  nombre_archivo <- strsplit(nombre_archivo[[1]][2],
                             "')", fixed = TRUE)[[1]][1] %>% as.character()
  return(nombre_archivo)
}
