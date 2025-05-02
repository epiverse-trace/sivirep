#' @title Realizar petición HTTP
#' @description Función que gestiona las peticiones HTTP a la API del
#' SIVIGILA.
#' @param url La dirección HTTP desde donde se descargará la información.
#' @return Si la petición es exitosa, retorna una respuesta HTTP.
#' De lo contrario, arroja un mensaje de error explicando el problema
#' y finaliza la ejecución del programa.
#' @examples
#' \donttest{
#' ruta_consulta_event <- obtener_val_config("query_diseases_by_year_path")
#' realizar_peticion_http(ruta_consulta_event)
#' }
#' @noRd
realizar_peticion_http <- function(url) {
  request_timeout <- obtener_val_config("request_timeout")
  tryCatch(
    httr2::req_perform(
      httr2::req_timeout(httr2::request(url),
                         request_timeout)),
    httr2_failure = function(e) {
      stop(
        "No se pudo conectar al servidor de SIVIGILA para descargar los datos",
        call. = FALSE)
    },
    httr2_error = function(e) {
      stop(
        "Error al conectarse al servidor de SIVIGILA para descargar los datos",
        call. = FALSE)
    },
    httr2_http_404 = function(e) {
      stop(
        "El dato no existe en los servidores de SIVIGILA", call. = FALSE)
    },
    httr2_http = function(e) {
      stop(
        "Error al conectarse al servidor de SIVIGILA para descargar los datos",
        call. = FALSE)
    },
    error = function(e) {
      if (grepl("Timeout", e$message, fixed = TRUE)) {
       stop(
        "No se pudo conectar al servidor de SIVIGILA para descargar los datos",
        call. = FALSE)
      } else {
        stop("Ha ocurrido un error inesperado", parent = e, call. = FALSE)
      }
    }
  )
}

#' @title Importar datos geográficos de Colombia
#' @description Función que importa los nombres y códigos de
#' los departamentos y municipios de Colombia a través de una URL.
#' @param descargar Un `logical` (`TRUE` o `FALSE`) que indica si los
#' datos deben descargarse desde la API de datos abiertos de Colombia;
#' su valor por defecto es `FALSE`.
#' @return Un `data.frame` con los nombres y códigos de los departamentos
#' y municipios de Colombia.
#' @examples
#' \donttest{
#' import_geo_cods(descargar = FALSE)
#' }
#' @export
import_geo_cods <- function(descargar = FALSE) {
  stopifnot("El parametro descargar debe ser un booleano"
            = is.logical(descargar))
  if (descargar) {
    ruta_data <- obtener_val_config("geo_data_path")
    data_geo <- utils::read.csv(ruta_data)
    names(data_geo) <- clean_labels(names(data_geo))
  } else {
    ruta_extdata <- system.file("extdata", package = "sivirep")
    ruta_data <- obtener_val_config("divipola_data_path")
    data_geo <- readRDS(file.path(ruta_extdata, ruta_data))
  }
  return(data_geo)
}

#' @title Importar enfermedades y años disponibles para
#' su descarga desde los microdatos del SIVIGILA
#' @description Función que obtiene las enfermedades y los años
#' disponibles para su descarga desde los microdatos del SIVIGILA.
#' @return Una `list` con las enfermedades y los años disponibles
#' para su descarga desde los microdatos del SIVIGILA.
#' @examples
#' \donttest{
#' if (interactive()) {
#'   list_events()
#'  }
#' }
#' @export
list_events <- function() {
  ruta_consulta_event_year <-
    obtener_val_config("query_diseases_by_year_path")
  conten_consulta_event_year <-
    realizar_peticion_http(ruta_consulta_event_year)
  conten_consulta_event_year <- httr2::resp_body_xml(conten_consulta_event_year)
  children <- xml2::xml_children(conten_consulta_event_year)
  children <-  xml2::xml_children(children)
  children <-  xml2::xml_children(children)
  children <-  xml2::xml_children(children)
  text_children <- xml2::xml_text(children)

  i <- 2
  nomb_events <- NULL
  years_events <- NULL
  children <- children[-base::seq(3, length(children), 3)]
  text_children <- text_children[-base::seq(3, length(text_children), 3)]
  while (i < base::length(children)) {
    event <- xml2::xml_text(children[i])
    nomb_events <- c(nomb_events, event)
    events <- base::which(text_children == event)
    years <- events - 1
    years_events <-
      c(years_events,
        base::toString(base::sort(text_children[years],
                                  decreasing = FALSE)))
    children <- children[-years]
    text_children <- text_children[-(events - 1)]
    children <- children[-base::which(text_children == event)]
    text_children <- text_children[-base::which(text_children == event)]
    i <- i + 2
  }
  events_adicionales <- obtener_val_config("additional_diseases")
  nomb_events_ad <- list()
  years_ad <- list()
  for (adicional in events_adicionales) {
    nomb_events_ad[[length(nomb_events_ad) + 1]] <- adicional$event
    years_ad[[length(years_ad) + 1]] <- toString(seq(adicional$start_year,
              adicional$final_year))
  }
  nomb_events <- c(stringr::str_to_title(nomb_events),
                   unlist(nomb_events_ad))
  years_events <- c(years_events, unlist(years_ad))
  eventos <- obtener_val_config("list_diseases")
  cod_events <- NULL
  for (nomb in nomb_events) {
    for (event in eventos) {
      if (stringr::str_equal(nomb, event$event)) {
        cod_events <- c(cod_events, event$cod_eve)
      }
    }
  }
  list_events <- data.frame(codigo = cod_events,
                            enfermedad = nomb_events,
                            aa = years_events)
  list_events <- list_events[order(list_events$enfermedad,
                                   decreasing = FALSE), ]
  list_events
}

#' @title Importar los datos de una enfermedad o evento por año
#' desde los microdatos del SIVIGILA
#' @description Función que importa los datos de una enfermedad o evento por
#' año desde los microdatos del SIVIGILA.
#' @param nombre_event Un `character` (cadena de caracteres) con el nombre de
#' la enfermedad o evento.
#' @param years Un `numeric` (numérico) con el año o años deseado(s) para
#' la descarga de los datos.
#' @param ruta_dir Un `character` (cadena de caracteres) que contiene la ruta
#' del directorio donde se almacenarán los datos del evento o enfermedad.
#' Su valor por defecto es `NULL`.
#' @param cache Un `logical` (`TRUE` o `FALSE`) que indica si los datos
#' descargados deben ser almacenados en caché. Su valor por defecto
#' es `FALSE`.
#' @return Un `data.frame` con los datos del año de la enfermedad o evento
#' seleccionado desde los microdatos del SIVIGILA.
#' @examples
#' \donttest{
#' if (interactive()) {
#' import_data_event(nombre_event = "DENGUE",
#'                   years = 2020,
#'                   cache = TRUE)
#' import_data_event(nombre_event = "CHAGAS",
#'                   years = c(2019, 2020),
#'                   ruta_dir = tempdir())
#' import_data_event(nombre_event = "CHAGAS",
#'                   years = seq(2018, 2020),
#'                   cache = TRUE)
#'  }
#' }
#' @export
import_data_event <- function(nombre_event,
                              years,
                              ruta_dir = NULL,
                              cache = FALSE) {
  stopifnot("El parametro years no debe estar vacio" = !missing(years),
            "El parametro years debe ser numerico" = is.numeric(years),
            "El parametro nombre_event no debe estar vacio"
            = !missing(nombre_event),
            "El parametro nombre_event debe ser una cadena de caracteres"
            = is.character(nombre_event),
            "El parametro cache debe ser un booleano"
            = is.logical(cache))
  data_event <- list()
  nombre_event <- stringr::str_to_title(nombre_event)
  cols_remover <- obtener_val_config("cols_remover")
  grupo_events <- obtener_eventos_relacionados(nombre_event, years)
  eventos_disponibles <- list_events()
  if (toupper(nombre_event) == "MALARIA") {
    grupo_events <-
      grupo_events[-which(grupo_events$enfermedad == nombre_event), ]
    eventos_disponibles <-
      eventos_disponibles[-which(eventos_disponibles$enfermedad
                                == nombre_event), ]
  }
  for (year in years) {
    for (event in grupo_events$enfermedad) {
      pos_event <- which(eventos_disponibles$enfermedad
                         == event)
      validation_year <-
        stringr::str_detect(eventos_disponibles[pos_event, ]$aa,
                            as.character(year))
      if (identical(pos_event, integer(0)) ||
          identical(validation_year, logical(0)) || !validation_year) {
        warning("El year: ", year,
                " de la enfermedad o evento: ",
                event,
                " no esta disponible para su descarga",
                call. = FALSE
        )
        next
      }
      data_url <- obtener_ruta_data_event_year(nombre_event = event,
                                               year = year)
      data_import <- import_sep_data(ruta_data = data_url,
                                     ruta_dir = ruta_dir,
                                     cache = cache)
      data_import <- limpiar_encabezado(data_import)
      if ("fec_def" %in% names(data_import)) {
        data_import$fec_def <- as.character(data_import$fec_def)
      }
      nombre_cols <- names(data_import)
      indice_cols_eve <- which(stringr::str_detect(nombre_cols,
                                                  stringr::fixed("cod_eve_")))
      if (length(indice_cols_eve) != 0) {
        names(data_import)[indice_cols_eve[1]] <- "cod_eve"
        indice_cols_eve[1] <- indice_cols_eve[-1]
        data_import <-
          data_import[, -indice_cols_eve]
        nombre_cols <- names(data_import)
      }
      indices_cols_remov <- which(nombre_cols %in% cols_remover)
      if (length(indices_cols_remov) != 0) {
        nombre_cols <- nombre_cols[-indices_cols_remov]
      }
      data_event <- c(data_event, list(data_import[, nombre_cols]))
    }
  }
  data_event <- dplyr::bind_rows(data_event)
  return(data_event)
}

#' @title Importar datos con un separador específico
#' @description Función que importa e identifica el separador que tiene los
#' datos para tabularlos.
#' @param ruta_data Un `character` (cadena de caracteres) que contiene
#' la URL de los datos de SIVIGILA.
#' @inheritParams import_data_event
#' @return Un `data.frame` con los datos tabulados.
#' @keywords internal
import_sep_data <- function(ruta_data = NULL,
                            ruta_dir = NULL,
                            cache = FALSE) {
  data_archivo <- data.frame()
  ruta_dir <-
    obtener_ruta_dir(ruta_dir = ruta_dir, cache = cache,
                     mensaje_error = "los datos de la enfermedad o evento")
  if (!dir.exists(ruta_dir)) {
    stop("La ruta ingresada en el parametro ruta_dir no existe",
         call. = FALSE)
  }
  if (!is.null(ruta_data)) {
    ini_nomb_archivo <-
      stringr::str_locate(ruta_data,
                          stringr::fixed("Microdatos/"))[2] + 1
    fin_nomb_archivo <-
      stringr::str_locate(ruta_data, stringr::fixed("value"))[1] - 5
    nomb_archivo <- stringr::str_sub(ruta_data, ini_nomb_archivo,
                                     fin_nomb_archivo)
    ruta_archivo <- file.path(ruta_dir, nomb_archivo)
    if (!file.exists(ruta_archivo) || !cache) {
      respuesta_archivo <- realizar_peticion_http(ruta_data)
      if (httr2::resp_status(respuesta_archivo) == 200) {
        conten_archivo <- httr2::resp_body_raw(respuesta_archivo)
        con_archivo <- file(ruta_archivo, "wb")
        if (length(conten_archivo) > 0) {
          writeBin(conten_archivo, con_archivo)
        }
        close(con_archivo)
      }
    }
    if (stringr::str_detect(nomb_archivo, ".xls")) {
      data_archivo <- readxl::read_excel(ruta_archivo,
                                         col_types = "text")
      if (!cache) {
        file.remove(ruta_archivo)
      }
    }
  }
  return(data_archivo)
}

#' @title Importar la población para efectuar el cálculo de la incidencia
#' @description Función que importa la población a riesgo de un evento o
#' enfermedad o las proyecciones poblacionales DANE desde el año 2005 hasta
#' el 2035.
#' @param poblacion Un `character` (cadena de caracteres) con el tipo de
#' población que se desea importar. Puede ser `"riesgo"` para la población
#' a riesgo del evento o `"proyecciones"` para las proyecciones poblacionales
#' DANE; su valor por defecto es `"riesgo"`.
#' @param event Un `character` (cadena de caracteres) o un `numeric` (numérico)
#' con el nombre o código de la enfermedad o evento. Es obligatorio para
#' importar la población a riesgo.
#' @param year Un `numeric` (numérico) con el año deseado de la población a
#' riesgo. Es obligatorio para importar la población a riesgo.
#' @param ruta_dir Un `character` (cadena de caracteres) que especifica la ruta
#' del directorio donde se almacenarán la población a riesgo o las proyecciones
#' poblacionales DANE. Su valor por defecto es `NULL`.
#' @param cache Un `logical` (`TRUE` o `FALSE`) que indica si la población a
#' riesgo o las proyecciones poblacionales DANE descargadas deben ser
#' almacenados en caché. Su valor por defecto es `FALSE`.
#' @return Un `data.frame` con la población a riesgo o las proyecciones
#' poblacionales DANE.
#' @examples
#'  \donttest{
#' # Importación proyecciones poblaciones DANE
#' if (interactive()) {
#'   import_pob_incidencia(poblacion = "proyecciones", year = 2020,
#'                         cache = TRUE)
#' }
#' # Importación población a riesgo de Dengue del año 2020
#' import_pob_incidencia(poblacion = "riesgo", event = "dengue", year = 2020,
#'                       ruta_dir = tempdir())
#' }
#' @export
import_pob_incidencia <- function(
    poblacion = c("riesgo", "proyecciones"),
    event,
    year,
    ruta_dir = NULL,
    cache = FALSE
  ) {
  stopifnot("El parametro poblacion no debe estar vacio" =
              !missing(poblacion),
            "El parametro poblacion debe ser una cadena de caracteres" =
              is.character(poblacion))
  poblacion <- match.arg(poblacion)

    if (poblacion == "proyecciones") {
    poblacion <- import_pob_proyecciones(year = year, cache = cache,
                                         ruta_dir = ruta_dir)
  } else {
    poblacion <- import_pob_riesgo(event = event, year = year, cache = cache,
                                   ruta_dir = ruta_dir)
  }
  return(poblacion)
}

#' @title Importar las proyecciones DANE del año 2005 hasta el 2035
#' @description Función que importa las proyecciones poblacionales
#' DANE desde el año 2005 hasta el 2035.
#' @param year Un `numeric` (numérico) con el año de las proyecciones
#' poblacionales DANE que desea importar.
#' @inheritParams import_pob_incidencia
#' @return Un `data.frame` con las proyecciones poblacionales DANE.
#' @examples
#' \donttest{
#' import_pob_proyecciones(year = 2020, ruta_dir = tempdir())
#' if (interactive()) {
#'   import_pob_proyecciones(year = 2020, cache = TRUE)
#'   }
#' }
#' @export
import_pob_proyecciones <- function(year,
                                    ruta_dir = NULL,
                                    cache = FALSE) {
  ruta_proyecciones <- obtener_val_config("projections_population")
  years_disp <- seq(ruta_proyecciones$start_year,
                    ruta_proyecciones$final_year)
  if (!year %in% years_disp) {
    return(NULL)
  }
  nomb_proyecs <-
    stringr::str_replace(ruta_proyecciones$file_name,
                         stringr::fixed("{year}"),
                         year)
  ruta_dir <-
    obtener_ruta_dir(ruta_dir = ruta_dir, cache = cache,
                     mensaje_error = "las proyeciones poblacionales DANE")
  ruta_proyecs <- file.path(ruta_dir,
                            paste0(nomb_proyecs,
                                   ruta_proyecciones$extension))
  if (!file.exists(ruta_proyecs)) {
    url_proyecs <-
      stringr::str_replace(ruta_proyecciones$url, stringr::fixed("{year}"),
                           year)
    utils::download.file(url_proyecs, ruta_proyecs)
  }
  proyecciones <- readRDS(ruta_proyecs)
  if (!cache) {
    file.remove(ruta_proyecs)
  }
  return(proyecciones)
}

#' @title Importar la población a riesgo de un evento o enfermedad
#' @description Función que importa la población a riesgo de un evento
#'o enfermedad para un año específico.
#' @param event Un `character` (cadena de caracteres) o un `numeric` (numérico)
#' con el nombre o código de la enfermedad o evento.
#' @param year Un `numeric` (numérico) con el año deseado de la población a
#' riesgo.
#' @inheritParams import_pob_incidencia
#' @return Un `data.frame` con la población a riesgo de un año específico.
#' @examples
#' \donttest{
#' import_pob_riesgo(event = "Dengue", year = 2020, ruta_dir = tempdir())
#' if (interactive()) {
#'   import_pob_riesgo(event = "Dengue", year = 2020, cache = TRUE)
#'   }
#' }
#' @export
import_pob_riesgo <- function(event, year,
                              ruta_dir = NULL,
                              cache = FALSE) {
  stopifnot("El parametro event no debe estar vacio" =
              !missing(event),
            "El parametro event debe ser una cadena de caracteres" =
              is.character(event),
            "El parametro year no debe estar vacio" = !missing(year),
            "El parametro year debe ser numerico" = is.numeric(year))
  rutas_pop_riesgo <- obtener_val_config("risk_population")
  etiqueta_year <- obtener_val_config("label_year")
  etiqueta_year <- paste0(tolower(etiqueta_year), "s")
  ruta_dir <-
    obtener_ruta_dir(ruta_dir = ruta_dir, cache = cache,
                     mensaje_error = "las poblaciones a riesgo")
  pop_event <- NULL
  years_disponibles <- NULL
  pob_riesgo_event <- NULL
  pop_event_ruta <- NULL
  event_min <- tolower(event)
  for (pop_riesgo in rutas_pop_riesgo) {
    if (stringr::str_detect(event_min, pop_riesgo$event) ||
        stringr::str_detect(event_min,
                            as.character(pop_riesgo$cod_eve))) {
      years_disponibles <- pop_riesgo$years
      if (year %in% pop_riesgo$years) {
        pop_event <- pop_riesgo
        pop_event$file_name <-
          stringr::str_replace(pop_event$file_name, stringr::fixed("{year}"),
                               year)
        pop_event_ruta <- file.path(ruta_dir,
                                    paste0(pop_event$file_name,
                                           pop_event$extension))
        if (!file.exists(pop_event_ruta)) {
          pop_event$url <-
            stringr::str_replace(pop_event$url, stringr::fixed("{year}"),
                                 year)
          utils::download.file(pop_event$url, pop_event_ruta)
        }
        break
      }
    }
  }
  if (!is.null(pop_event_ruta)) {
      pob_riesgo_event <- readRDS(pop_event_ruta)
      if (!cache) {
        file.remove(pop_event_ruta)
      }
  } else if (!is.null(years_disponibles)) {
    warning("Para el ", year, " la poblacion a riesgo no esta disponible.",
            " Los ", etiqueta_year, " disponibles para la enfermedad o ",
            "evento son: ",
            toString(years_disponibles),
            call. = FALSE)
  } else {
    warning("Para ", event, " no hay poblacion a riesgo disponible de ",
            "ningun year",
            call. = FALSE)
  }
  return(pob_riesgo_event)
}

#' @title Importar el Shapefile del mapa de Colombia
#' @description Función que importa el Shapefile del mapa de Colombia.
#' @param ruta_dir Un `character` (cadena de caracteres) que contiene la
#' ruta del directorio donde se almacenará el Shapefile del mapa de
#' Colombia. Su valor por defecto es `NULL`.
#' @param cache Un `logical` (`TRUE` o `FALSE`) que indica si el Shapefile
#' del mapa de Colombia debe ser almacenado en caché. Su valor por defecto
#' es `FALSE`.
#' @return Un objeto `sf` que contiene los elementos del Shapefile
#' del mapa.
#' @keywords internal
import_shape_map <- function(ruta_dir = NULL,
                             cache = FALSE) {
  ruta_dir <- obtener_ruta_dir(ruta_dir = ruta_dir, cache = cache,
                               mensaje_error = "el Shapefile del mapa")
  archivo_zip <- obtener_val_config("map_shape_zip_file")
  ruta_zip <- file.path(ruta_dir, archivo_zip)
  if (!file.exists(ruta_zip)) {
    url_base <- obtener_val_config("map_shape_path")
    utils::download.file(url_base, ruta_zip)
    utils::unzip(zipfile = ruta_zip, exdir = ruta_dir)
  }
  carpeta_base <- obtener_val_config("map_shape_folder")
  ruta_shape <- file.path(ruta_dir, carpeta_base,
                          obtener_val_config("map_shape_file"))
  if (!file.exists(ruta_shape)) {
    stop("No es posible obtener el Shapefile del mapa",
         call. = FALSE)
  }
  shp <- sf::st_read(dsn = ruta_shape, quiet = TRUE)
  return(shp)
}
