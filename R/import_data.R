#' Función de conveniencia para hacer peticiones a SIVIGILA
#' @param url La dirección HTTP desde donde descargar la información
#' @return Si la petición es exitosa, retorna una respuesta HTTP.
#' De lo contrario arroja un mensaje de error explicando el problema
#' y finaliza la ejecución del programa
#' @examples
#' \donttest{
#' query_event_year_path <- config::get(file =
#'            system.file("extdata",
#'            "config.yml",
#'            package = "sivirep"),
#'            "query_diseases_by_year_path")
#' make_request(query_event_year_path)
#' }
#' @noRd

peticion_http <- function(url) {
  request_timeout <- config::get(file =
                                        system.file("extdata",
                                                    "config.yml",
                                                    package = "sivirep"),
                                      "request_timeout")
  return(tryCatch(
    httr2::request(url) %>%
    httr2::req_timeout(request_timeout) %>%
    httr2::req_perform(),
    httr2_failure = function(e) {
      stop(
        "No se pudo conectar al servidor de SIVIGILA para descargar los datos")
    },
    httr2_error = function(e) {
      stop(
        "Error al conectarse al servidor de SIVIGILA para descargar los datos")
    },
    httr2_http_404 = function(e) {
      stop(
        "El dato no existe en los servidores de SIVIGILA")
    },
    httr2_http = function(e) {
      stop(
        "Error al conectarse al servidor de SIVIGILA para descargar los datos")
    },
    error = function(e) {
      # Check if the error message indicates a timeout
      if (grepl("Timeout", e$message, fixed = TRUE)) {
       stop(
        "No se pudo conectar al servidor de SIVIGILA para descargar los datos")
      } else {
        stop("Ha ocurrido un error inesperado ", parent = e)
      }
    }
  ))
}


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
  stopifnot("El parametro descargar debe ser un booleano"
            = is.logical(descargar))
  if (descargar) {
    path_data <- config::get(file =
                              system.file("extdata",
                                          "config.yml",
                                          package = "sivirep"),
                            "geo_data_path")
    data_geo <- utils::read.csv(path_data)
    names(data_geo) <- epitrix::clean_labels(names(data_geo))
  } else {
    divipoladata <- NULL
    extdata_path <- system.file("extdata", package = "sivirep")
    config_file <- system.file("extdata", "config.yml", package = "sivirep")
    path_data <- config::get(file = config_file, "divipola_data_path")
    load(file.path(extdata_path, path_data))
    data_geo <- divipoladata
  }
  return(data_geo)
}

#' Importar las enfermedades y años disponibles disposibles
#' para su descarga desde los microdatos del SIVIGILA
#'
#' Función que obtiene las enfermedades y los años disponibles
#' para su descarga desde los microdatos del SIVIGILA
#' @return Una `list` con las enfermedades y los años disponibles
#' para su descarga desde los microdatos del SIVIGILA
#' @examples
#' \donttest{
#' list_events()
#' }
#' @export
list_events <- function() {

  query_event_year_path <- config::get(file =
                                         system.file("extdata",
                                                     "config.yml",
                                                     package = "sivirep"),
                                       "query_diseases_by_year_path")
  query_event_year_content <-
    peticion_http(query_event_year_path) %>%
    httr2::resp_body_xml()

  children <- xml2::xml_children(query_event_year_content) %>%
    xml2::xml_children() %>%
    xml2::xml_children() %>%
    xml2::xml_children()
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
  years_diseases <- base::append(years_diseases, c("", "", ""))
  list_events <- data.frame(enfermedad = name_diseases,
                            aa = years_diseases)
  list_events <- list_events[order(list_events$enfermedad,
                                   decreasing = FALSE), ]
  return(list_events)
}

#' Importar los datos de una enfermedad o evento por año
#' desde los microdatos del SIVIGILA
#'
#' Función que importa los datos de una enfermedad o evento por año
#' desde los microdatos del SIVIGILA
#' @param nombre_event Un `character` (cadena de caracteres) con el nombre de
#' la enfermedad o evento
#' @param years Un `numeric` (numerico) con el año o años deseado(s) para
#' la descarga de los datos
#' @param cache Un `boolean` (TRUE o FALSE) que indica si los datos descargados
#' deben ser almacenados en caché; su valor por defecto es `TRUE`
#' @return Un `data.frame` con los datos del año de la enfermedad o evento
#' seleccionado desde los microdatos del SIVIGILA
#' @examples
#' \donttest{
#' import_data_event(nombre_event = "DENGUE",
#'                   years = 2020,
#'                   cache = TRUE)
#' }
#' \dontrun{
#' import_data_event(nombre_event = "DENGUE",
#'                   years = c(2019, 2020),
#'                   cache = TRUE)
#' import_data_event(nombre_event = "DENGUE",
#'                   years = seq(2007, 2020),
#'                   cache = TRUE)
#' }
#' @export
import_data_event <- function(nombre_event,
                              years,
                              cache = TRUE) {
  stopifnot("El parametro years no debe estar vacio" = !missing(years),
            "El parametro years debe ser numerico" = is.numeric(years),
            "El parametro nombre_event no debe estar vacio"
            = !missing(nombre_event),
            "El parametro nombre_event debe ser una cadena de caracteres"
            = is.character(nombre_event),
            "El parametro cache debe ser un booleano"
            = is.logical(cache))
  data_event <- data.frame()
  nombre_event <- stringr::str_to_title(nombre_event)
  cols_remover <- config::get(file =
                                system.file("extdata",
                                            "config.yml",
                                            package = "sivirep"),
                              "cols_remover")
  grupo_events <- obtener_eventos_relacionados(nombre_event, years)
  for (year in years) {
    for (event in grupo_events$enfermedad) {
      if (event != "Malaria") {
        data_url <- obtener_ruta_data_event_year(nombre_event = event,
                                                 year = year)
        data_import <- import_sep_data(data_url, cache)
        data_import <- limpiar_encabezado(data_import)
        data_import$fec_def <- as.character(data_import$fec_def)
        nombre_cols <- names(data_import)
        index_cols_eve <- which(stringr::str_detect(nombre_cols,
                                                    stringr::fixed("cod_eve_")))
        if (!identical(index_cols_eve,
                       integer(0))) {
          names(data_import)[index_cols_eve[1]] <- "cod_eve"
          index_cols_eve[1] <- index_cols_eve[-1]
          data_import <-
            data_import[, -index_cols_eve]
          nombre_cols <- names(data_import)
        }
        nombre_cols <- nombre_cols[-which(nombre_cols %in% cols_remover)]
        data_event <- rbind(data_event, data_import[, nombre_cols])
      }
    }
  }
  return(data_event)
}

#' Importar datos con un separador específico
#'
#' Función que identifica el separador que tiene los datos para tabularlos
#' @param ruta_data Un `character` (cadena de caracteres) que contiene
#' la URL de los datos de SIVIGILA
#' @param cache Un `boolean` (TRUE o FALSE) que indica si los datos descargados
#' deben ser almacenados en caché; su valor por defecto es `TRUE`
#' @return Un `data.frame` con los datos
#' @keywords internal
import_sep_data <- function(ruta_data = NULL, cache = TRUE) {
  stopifnot("El parametro ruta_data debe ser una cadena de caracteres" =
              is.character(ruta_data),
            "El parametro cache debe ser un booleano"
            = is.logical(cache))
  data <- data.frame()
  extdata_path <- system.file("extdata", package = "sivirep")
  if (!is.null(ruta_data)) {
    start_file_name <- stringr::str_locate(ruta_data,
                                           stringr::fixed("Microdatos/"))[2] + 1
    end_file_name <- stringr::str_locate(ruta_data,
                                         stringr::fixed("value"))[1] - 5
    file_name <- stringr::str_sub(ruta_data, start_file_name, end_file_name)
    file_path <- file.path(extdata_path, file_name)
    if (!file.exists(file_path) || !cache) {
      file_response <- peticion_http(ruta_data)
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
      data <- readxl::read_excel(file_path,
                                 col_types = "text")
    }
  }
  return(data)
}

#' Importar la población para efectuar el cálculo de la incidencia
#'
#' Función que importa la población a riesgo de un evento o enfermedad
#' o las proyecciones poblacionales DANE desde el año 2005 hasta el 2035
#' @param poblacion Un `character` (cadena de caracteres) con el tipo de
#' población que se desea importar. Indica si se desea descargar la población
#' a riesgo del evento `"riesgo"` o las proyecciones poblacionales DANE
#' `"proyecciones"`; su valor por defecto es `"riesgo"`
#' @param event Un `character` (cadena de caracteres) o un `numeric` (numerico)
#' con el nombre o código de la enfermedad o evento. Es obligatorio para
#' importar la población a riesgo
#' @param year Un `numeric` (numerico) con el año deseado de la población a
#' riesgo. Es obligatorio para importar la población a riesgo
#' @return Un `data.frame` con la población a riesgo o las proyecciones
#' poblacionaldes DANE
#' @examples
#'  \dontrun{
#' import_pob_incidencia(poblacion = "proyecciones")
#' import_pob_incidencia(poblacion = "riesgo", event = "dengue", year = 2020)
#' }
#' @export
import_pob_incidencia <- function(poblacion = "riesgo", event, year) {
  stopifnot("El parametro poblacion no debe estar vacio" =
              !missing(poblacion),
            "El parametro poblacion debe ser una cadena de caracteres" =
              is.character(poblacion),
            "Valor invalido para el parametro poblacion" =
              (poblacion %in% c("riesgo", "proyecciones")))
  if (poblacion == "proyecciones") {
    poblacion <- import_pob_proyecciones()
  } else {
    poblacion <- import_pob_riesgo(event = event, year = year)
  }
  return(poblacion)
}

#' Importar las proyecciones DANE del año 2005 hasta el 2035
#'
#' Función que importa las proyecciones poblacionales DANE desde
#' el año 2005 hasta el 2035
#' @return Un `data.frame` con las proyecciones poblacionales DANE
#' @examples
#' \dontrun{
#' import_pob_proyecciones()
#' }
#' @export
import_pob_proyecciones <- function() {
  proyecciones <- NULL
  proyecs_2005_2035 <- NULL
  nomb_proyecs <- config::get(file =
                                system.file("extdata",
                                            "config.yml",
                                            package = "sivirep"),
                              "projections_file_name")
  ruta_extdata <- system.file("extdata", package = "sivirep")
  ruta_proyecs <- file.path(ruta_extdata, nomb_proyecs)
  if (!file.exists(ruta_proyecs)) {
    url_proyecs <- config::get(file =
                                 system.file("extdata",
                                             "config.yml",
                                             package = "sivirep"),
                               "projections_path")
    utils::download.file(url_proyecs, ruta_proyecs)
  }
  load(ruta_proyecs)
  proyecciones <- proyecs_2005_2035
  return(proyecciones)
}

#' Importar la población a riesgo de un evento o enfermedad
#'
#' Función que importa la población a riesgo de un evento o enfermedad de un
#' año específico
#' @param event Un `character` (cadena de caracteres) o un `numeric` (numerico)
#' con el nombre o código de la enfermedad o evento
#' @param year Un `numeric` (numerico) con el año deseado de la población a
#' riesgo
#' @return Un `data.frame` con la población a riesgo de un año específico
#' @examples
#' \dontrun{
#' import_pob_riesgo(event = "Dengue", year = 2020)
#' }
#' @export
import_pob_riesgo <- function(event, year) {
  stopifnot("El parametro event no debe estar vacio" =
              !missing(event),
            "El parametro event debe ser una cadena de caracteres" =
              is.character(event),
            "El parametro year no debe estar vacio" = !missing(year),
            "El parametro year debe ser numerico" = is.numeric(year))
  rutas_pop_riesgo <- config::get(file =
                                      system.file("extdata",
                                                  "config.yml",
                                                  package = "sivirep"),
                                  "risk_population_paths")
  etiqueta_year <- config::get(file =
                                    system.file("extdata",
                                                "config.yml",
                                                package = "sivirep"),
                               "label_year")
  etiqueta_year <- paste0(tolower(etiqueta_year), "s")
  ruta_extdata <- system.file("extdata", package = "sivirep")
  pop_event <- NULL
  years_disponibles <- NULL
  pob_riesgo_event <- NULL
  pop_event_ruta <- NULL
  event_min <- tolower(event)
  for (pop_riesgo in rutas_pop_riesgo) {
    if (stringr::str_detect(event_min, pop_riesgo$event) ||
        stringr::str_detect(as.character(event_min),
                            as.character(pop_riesgo$cod_eve))) {
      years_disponibles <- pop_riesgo$years
      if (year %in% pop_riesgo$years) {
        pop_event <- pop_riesgo
        pop_event_ruta <- file.path(ruta_extdata, paste0(pop_riesgo$file_name,
                                                         pop_riesgo$extension))
        if (!file.exists(pop_event_ruta)) {
          utils::download.file(pop_event$url, pop_event_ruta)
        }
        break
      }
    }
  }
  if (!is.null(pop_event) && !is.null(pop_event_ruta)) {
      load(pop_event_ruta)
      pob_riesgo_event <- eval(parse(text = pop_event$file_name))
  } else if (!is.null(years_disponibles)) {
    warning("Para el ", year, " la poblacion a riesgo no esta disponible.",
            " Los ", etiqueta_year, " disponibles para la enfermedad o ",
            "evento son: ",
            toString(years_disponibles, collapse = ", "))
  } else {
    warning("Para ", event, " no hay poblacion a riesgo disponible de ",
            "ningun year")
  }
  return(pob_riesgo_event)
}
