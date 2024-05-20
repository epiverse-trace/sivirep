#' Estandarizar códigos geográficos de los datos de una enfermedad o evento
#'
#' Función que estandariza los códigos geográficos de los datos
#' de una enfermedad o evento según la codificación del DIVIPOLA
#' @param data_event Un `data.frame` que contiene los datos de una
#' enfermedad o evento
#' @return Un `data.frame` que contiene los códigos geográficos estandarizados
#' de los datos de una enfermedad o evento según la codificación del DIVIPOLA
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' estandarizar_geo_cods(data_event = data_limpia)
#' @export
estandarizar_geo_cods <- function(data_event) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0)
  geo_columns <- config::get(file =
                               system.file("extdata", "config.yml",
                                           package = "sivirep"),
                             "geo_column_names")
  for (column in geo_columns) {
    if (stringr::str_detect(column, stringr::fixed("dpto"))) {
      data_event[[column]] <- formatC(data_event[[column]],
                                      width = 2,
                                      format = "d",
                                      flag = "0")
    }
    if (stringr::str_detect(column, stringr::fixed("mun"))) {
      data_event[[column]] <- formatC(data_event[[column]],
                                      width = 3,
                                      format = "d",
                                      flag = "0")
      col_dpto <- stringr::str_replace(column, stringr::fixed("_mun_"),
                                       "_dpto_")
      if (max(nchar(data_event[[column]])) == 3) {
        data_event[[column]] <- paste0(data_event[[col_dpto]],
                                       data_event[[column]])
      }
    }
  }
  return(data_event)
}

#' Convertir edad a años
#'
#' Función que convierte las edades en años según las unidades de medida del
#' SIVIGILA
#' @param data_event Un `data.frame` que contiene los datos de una
#' enfermedad o evento
#' @param col_edad Un `character` (cadena de caracteres) con
#' el nombre de la columna que contiene las edades en los datos de
#' la enfermedad o evento; su valor por defecto es `"edad"`
#' @param col_uni_med Un `character` (cadena de caracteres) con el nombre
#' de la columna que contiene las unidades de medida en los datos de una
#' enfermedad o evento; su valor por defecto es `"uni_med"`
#' @return Un `data.frame` con las edades en años según las unidades de medida
#' del SIVIGILA
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' convert_edad(data_event = data_limpia,
#'              col_edad = "edad",
#'              col_uni_med = "uni_med")
#' @export
convert_edad <- function(data_event,
                         col_edad = "edad",
                         col_uni_med = "uni_med") {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro col_edad debe ser una cadena de caracteres" =
              is.character(col_edad),
            "El parametro col_uni_med debe ser una cadena de caracteres" =
              is.character(col_uni_med))
  data_event$uni_med <- as.numeric(data_event$uni_med)
  data_event$edad <- as.numeric(data_event$edad)
  data_event_years <-
    dplyr::mutate(data_event,
                  edad = dplyr::case_when(eval(parse(text =
                                                       col_uni_med)) == 1 ~
                                            round(eval(parse(text = col_edad)),
                                                  3),
                                          eval(parse(text = col_uni_med)) == 2 ~
                                            round((eval(parse(text =
                                                                col_edad))
                                                   / 12),
                                                  3),
                                          eval(parse(text = col_uni_med)) == 3 ~
                                            round((eval(parse(text = col_edad))
                                                   / 876),
                                                  3),
                                          eval(parse(text = col_uni_med)) == 4 ~
                                            round((eval(parse(text =
                                                                col_edad))
                                                   / 525960),
                                                  3),
                                          eval(parse(text = col_uni_med)) == 5 ~
                                            round((eval(parse(text =
                                                                col_edad))
                                                   / 3.156e+7),
                                                  3)))
  return(data_event_years)
}

#' Eliminar valores NIN (NA, Infinito, NaN)
#'
#' Función que elimina filas si los valores de la columna seleccionada
#' incluyen NA, Infinito o NaN
#' @param data_event Un `data.frame` que contiene los datos de una
#' enfermedad o evento
#' @param nomb_col Un `character` (cadena de caracteres) que contiene el
#' nombre de la columna en los datos de una enfermedad o evento a evaluar
#' @return Los datos limpios sin valores NA, Infinito o NaN
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' remove_val_nin(data_event = data_limpia, nomb_col = "edad")
#' @export
remove_val_nin <- function(data_event, nomb_col) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro nomb_col es obligatorio" = !missing(nomb_col),
            "El parametro nomb_col debe ser una cadena de caracteres" =
              is.character(nomb_col))
  ref_col <- paste0("data_event$", nomb_col)
  data_event_del <- data_event
  del_rows <- is.na(eval(parse(text = ref_col))) |
    is.nan(eval(parse(text = ref_col))) |
    is.infinite(eval(parse(text = ref_col)))
  if (any(del_rows)) data_event_del <- data_event[!del_rows]
  return(data_event_del)
}

#' Eliminar fechas mayores que el valor de comparación
#'
#' Función que elimina fechas mayores que el valor de comparación
#' @param data_event Un `data.frame` que contiene los datos de
#' una enfermedad o evento
#' @param col_ini Un `character` (cadena de caracteres) que contiene
#' el nombre de la columna de la fecha inicial;
#' su valor por defecto es ini_sin
#' @param col_comp Un `character` (cadena de caracteres) que contiene el
#' nombre de la columna de la fecha de comparación;
#' su valor por defecto es fec_hos
#' @return Un `data.frame` con los datos sin las fechas mayores que el
#' valor de comparación
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' remove_error_fecha(data_event = data_limpia,
#'                    col_ini = "ini_sin",
#'                    col_comp = "fec_hos")
#' @export
remove_error_fecha <- function(data_event,
                               col_ini = "ini_sin",
                               col_comp = "fec_hos") {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro col_ini debe ser una cadena de caracteres" =
              is.character(col_ini),
            "El parametro col_comp debe ser una cadena de caracteres" =
              is.character(col_comp))
  del_rows <- which(data_event[[col_comp]] <= data_event[[col_ini]])
  data_event_del <- data_event[-del_rows, ]
  return(data_event_del)
}

#' Formatear fechas
#'
#' Función que da un formato específico a una fecha
#' @param data_event Un `data.frame` que contiene los datos
#' de un evento o enfermedad
#' @param format_fecha Un `character` (cadena de caracteres)
#' que contiene  el formato deseado de la fecha
#' @param nomb_cols Un `character` (cadena de caracteres) que
#' contiene los nombres de la columna a formatear; su valor por defecto
#' es `NULL`
#' @return Un `data.frame` con los datos con las fechas formateadas
#' @keywords internal
format_fecha <- function(data_event,
                         format_fecha = "%Y-%m-%d",
                         nomb_cols = NULL) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro format_fecha debe ser una cadena de caracteres" =
              is.character(format_fecha),
            "El parametro nomb_cols no debe estar vacio" =
              !is.null(nomb_cols),
            "El parametro nomb_cols debe ser una cadena de caracteres o 
            un arreglo de cadenas de caracteres" =
              (is.character(nomb_cols) && !is.array(nomb_cols)) ||
              (!is.character(nomb_cols) && is.array(nomb_cols)))
  data_limpia <- data_event
  if (!is.null(nomb_cols)) {
    for (name in nomb_cols) {
      if (!is.null(name)) {
        data_limpia[[name]] <- as.Date(data_event[[name]],
                                           format = format_fecha)
      }
    }
  }
  return(data_limpia)
}

#' Limpiar las etiquetas del encabezado de los datos de una enfermedad
#'
#' Función que limpia las etiquetas del encabezado de los datos
#' de una enfermedad o evento
#' @param data_event Un `data.frame` que contiene los datos de una
#' enfermedad o evento
#' @return Un `data.frame` con las etiquetas del encabezado formateadas
#' con guión bajo (_)
#' @examples
#' data(dengue2020)
#' limpiar_encabezado(data_event = dengue2020)
#' @export
limpiar_encabezado <- function(data_event) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0)
  names(data_event) <- epitrix::clean_labels(names(data_event))
  return(data_event)
}

#' Limpiar fechas de los datos de una enfermedad o evento
#'
#' Función que limpia y estandariza las fechas de los datos de una
#' enfermedad o evento
#' @param data_event Un `data.frame` que contiene los datos de
#' una enfermedad o evento
#' @param year Un `numeric` (numerico) o `character` (cadena de caracteres)
#' que contiene el año de los datos de una enfermedad o evento
#' @param format_fecha Un `character` (cadena de caracteres) que contiene
#' el formato deseado de fecha; su valor por defecto es "\%AAAA-\%MM-\%DD"
#' @param nomb_col Un `character` (cadena de caracteres) que contiene
#' el nombre de la columna con la fecha que se desea limpiar en los datos
#' de la enfermedad o evento
#' @param col_comp Un `character` (cadena de caracteres) que contiene el
#' nombre de la columna con la cual se va a comparar la columna `nomb_col`
#' para limpiarla, estandarizarla o aplicar las reglas definidas
#' @return Un `data.frame` con las fechas limpias
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' limpiar_fecha_event(data_event = data_limpia,
#'                     year = 2020,
#'                     format_fecha = "%Y-%m-%d",
#'                     nomb_col = "ini_sin",
#'                     col_comp = "fec_hos")
#' @export
limpiar_fecha_event <- function(data_event,
                                year,
                                format_fecha = "%Y-%m-%d",
                                nomb_col = "ini_sin",
                                col_comp = NULL) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro year es obligatorio" = !missing(year),
            "El parametro year debe ser una cadena de caracteres
            o numerico" =
            (is.numeric(year) && !is.character(year)) ||
            (!is.numeric(year) && is.character(year)),
            "El parametro format_fecha debe ser una cadena de caracteres" =
              is.character(format_fecha),
            "El parametro nomb_col debe ser una cadena de caracteres" =
              is.character(nomb_col))
  data_event_fecha_ini <- data_event
  if (!is.null(col_comp)) {
    data_event_fecha_ini <-
      remove_error_fecha(data_event_fecha_ini,
                         nomb_col,
                         col_comp)
    stopifnot("El parametro col_comp debe ser una cadena de caracteres" =
              is.character(col_comp))
  }
  data_event_fecha_ini[order(data_event_fecha_ini[[nomb_col]],
                             decreasing = TRUE), ]
  data_event_fecha_ini <-
    data_event_fecha_ini[format(data_event_fecha_ini[[nomb_col]],
                                "%Y") == year, ]
  return(data_event_fecha_ini)
}

#' Limpiar las edades de los datos de una enfermedad o evento
#'
#' Función que limpia y estandariza las edades de los datos de una
#' enfermedad o evento, conviertiendolas en años, según la clasificación
#' del Instituto Nacional de Salud:
#' No aplica = 0
#' Años = 1
#' Meses = 2
#' Días = 3
#' Horas = 4
#' Minutos = 5
#' @param data_event Un `data.frame` que contiene los datos de una
#' enfermedad o evento
#' @param nomb_col Un `character` (cadena de caracteres) con el nombre
#' de la columna de los datos que contiene las edades;
#' su valor por defecto es edad
#' @return Un `data.frame` con los datos de una enfermedad o evento
#' con las edades limpias
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' limpiar_edad_event(data_event = data_limpia, nomb_col = "edad")
#' @export
limpiar_edad_event <- function(data_event, nomb_col = "edad") {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro nomb_col debe ser una cadena de caracteres" =
              is.character(nomb_col))
  data_event_years <- convert_edad(data_event)
  data_event_years <- remove_val_nin(data_event_years, nomb_col)
}

#' Limpiar los valores atipicos de los datos
#'
#' Función que limpia los valores atipicos de los datos de una
#' enfermedad o evento del SIVIGILA
#' @param data_event Un data frame que contiene los datos de una
#' enfermedad o evento
#' @return Un data framecon los datos de una enfermedad o
#' evento con los valores atípicos limpios (NA)
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_encabezado(data_event = dengue2020)
#' @export
limpiar_val_atipic <- function(data_event) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0)
  cols_events <- config::get(file =
                               system.file("extdata",
                                           "config.yml",
                                           package = "sivirep"),
                             "diseases_exceptions")
  cod_event <- data_event$cod_eve[1]
  if (cod_event > 0) {
    for (event in cols_events) {
      code <- names(event)
      if (cod_event %in% code) {
        cols <- event[as.character(cod_event)]
        for (nom_cols in cols) {
          for (col in nom_cols) {
            data_event[eval(parse(text = col))] <- NA
          }
        }
      }
    }
  }
}

#' Limpiar datos de SIVIGILA
#'
#' Función que limpia los datos seleccionados de una enfermedad o evento de
#' la fuente de SIVIGILA
#' @param data_event Un data frame que contiene los datos de
#' una enfermedad o evento
#' @return Un data frame con los datos limpios de la enfermedad o evento
#' @examples
#' data(dengue2020)
#' limpiar_data_sivigila(data_event = dengue2020)
#' @export
limpiar_data_sivigila <- function(data_event) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0)
  data_event <- limpiar_encabezado(data_event)
  nom_cols_fechas <- config::get(file = system.file("extdata",
                                                    "config.yml",
                                                    package = "sivirep"),
                                 "dates_column_names")
  year <- names(sort(table(data_event$ano), decreasing = TRUE)[1])
  data_limpia <- format_fecha(data_event,
                                  nomb_cols = nom_cols_fechas)
  nombre <- unique(data_event$nombre_evento)
  if (length(nombre) == 1 &&
      !stringr::str_detect(nombre, stringr::fixed("MORTALIDAD"))) {
    data_limpia <- limpiar_fecha_event(data_limpia, year,
                                       nomb_col = nom_cols_fechas[3],
                                       col_comp = nom_cols_fechas[4])
    data_limpia <- limpiar_fecha_event(data_limpia, year,
                                       nomb_col = nom_cols_fechas[2])
  }
  data_limpia <- estandarizar_geo_cods(data_limpia)
  data_limpia <- convert_edad(data_limpia,
                              col_edad = "edad",
                              col_uni_med = "uni_med")
  return(data_limpia)
}
