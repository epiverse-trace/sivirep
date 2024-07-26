#' @title Estandarizar códigos geográficos de los datos de una enfermedad
#' o evento
#' @description Función que estandariza los códigos geográficos de los datos
#' de una enfermedad o evento según la codificación del DIVIPOLA
#' @param data_event Un `data.frame` que contiene los datos de una
#' enfermedad o evento.
#' @return Un `data.frame` que contiene los códigos geográficos estandarizados
#' de los datos de una enfermedad o evento según la codificación del DIVIPOLA.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' estandarizar_geo_cods(data_event = data_limpia)
#' @export
estandarizar_geo_cods <- function(data_event) {
  validar_data_event(data_event)
  geo_columns <- obtener_val_config("geo_column_names")
  for (column in geo_columns) {
    if (stringr::str_detect(column, stringr::fixed("dpto"))) {
      data_event[[column]] <- formatC(data_event[[column]],
        width = 2,
        format = "d",
        flag = "0"
      )
    }
    if (stringr::str_detect(column, stringr::fixed("mun"))) {
      data_event[[column]] <- formatC(data_event[[column]],
        width = 3,
        format = "d",
        flag = "0"
      )
      col_dpto <- stringr::str_replace(
        column, stringr::fixed("_mun_"),
        "_dpto_"
      )
      # formatC() will sometimes return elements longer than 3, even though we
      # specified width = 3, because it will not truncate longer strings
      if (max(nchar(data_event[[column]])) == 3) {
        data_event[[column]] <- paste0(
          data_event[[col_dpto]],
          data_event[[column]]
        )
      }
    }
  }
  return(data_event)
}

#' @title Convertir edad a años
#' @description Función que convierte las edades a años según las unidades de
#' medida del SIVIGILA.
#' @param data_event Un `data.frame` que contiene los datos de una
#' enfermedad o evento.
#' @param col_edad Un `character` (cadena de caracteres) con
#' el nombre de la columna que contiene las edades en los datos de
#' la enfermedad o evento; su valor por defecto es `"edad"`.
#' @param col_uni_med Un `character` (cadena de caracteres) con el nombre
#' de la columna que contiene las unidades de medida en los datos de una
#' enfermedad o evento; su valor por defecto es `"uni_med"`.
#' @return Un `data.frame` con las edades en años según las unidades de medida
#' del SIVIGILA.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' convert_edad(
#'   data_event = data_limpia,
#'   col_edad = "edad",
#'   col_uni_med = "uni_med"
#' )
#' @importFrom rlang :=
#' @export
convert_edad <- function(data_event,
                         col_edad = "edad",
                         col_uni_med = "uni_med") {
  validar_data_event(data_event)
  validar_edad(data_event, col_edad)
  stopifnot(
    "El parametro col_uni_med debe ser una cadena de caracteres" =
      is.character(col_uni_med)
  )
  data_event[[col_uni_med]] <- as.numeric(data_event[[col_uni_med]])
  data_event[[col_edad]] <- as.numeric(data_event[[col_edad]])
  data_event_years <-
    dplyr::mutate(
      data_event,
      {{ col_edad }} := dplyr::case_when(
        .data[[col_uni_med]] == 1 ~ round(.data[[col_edad]], 3),
        .data[[col_uni_med]] == 2 ~ round(.data[[col_edad]] / 12, 3),
        .data[[col_uni_med]] == 3 ~ round(.data[[col_edad]] / 876, 3),
        .data[[col_uni_med]] == 4 ~ round(.data[[col_edad]] / 525960, 3),
        .data[[col_uni_med]] == 5 ~ round(.data[[col_edad]] / 3.156e+7, 3)
      )
    )
  return(data_event_years)
}

#' @title Eliminar valores NIN (NA, Infinito, NaN)
#' @description Función que elimina filas si los valores de la columna
#' seleccionada incluyen NA, Infinito o NaN.
#' @param data_event Un `data.frame` que contiene los datos de una
#' enfermedad o evento.
#' @param nomb_col Un `character` (cadena de caracteres) que contiene el
#' nombre de la columna a evaluar en los datos de una enfermedad o evento.
#' @return Un `data.frame` con los datos limpios sin valores NA,
#' Infinito o NaN.
#' @keywords internal
remove_val_nin <- function(data_event, nomb_col) {
  validar_data_event(data_event)
  del_rows <- is.na(data_event[[nomb_col]]) |
    is.nan(data_event[[nomb_col]]) |
    is.infinite(data_event[[nomb_col]])
  data_event_del <- data_event[!del_rows]
  return(data_event_del)
}

#' @title Eliminar fechas mayores que el valor de comparación
#' @description Función que elimina fechas mayores que el valor de
#' comparación.
#' @param data_event Un `data.frame` que contiene los datos de
#' una enfermedad o evento.
#' @param col_ini Un `character` (cadena de caracteres) que contiene
#' el nombre de la columna de la fecha inicial en los datos de una
#' enfermedad o evento; su valor por defecto es `"ini_sin"`.
#' @param col_comp Un `character` (cadena de caracteres) que contiene el
#' nombre de la columna de la fecha de comparación en los datos de una
#' enfermedad o evento; su valor por defecto es `"fec_hos`".
#' @return Un `data.frame` con los datos sin las fechas mayores que el
#' valor de comparación.
#' @keywords internal
remove_error_fecha <- function(data_event,
                               col_ini = "ini_sin",
                               col_comp = "fec_hos") {
  validar_data_event(data_event)
  del_rows <- which(data_event[[col_comp]] <= data_event[[col_ini]])
  data_event_del <- data_event[-del_rows, ]
  return(data_event_del)
}

#' @title Formatear fechas
#' @description Función que da un formato específico a una fecha.
#' @param data_event Un `data.frame` que contiene los datos
#' de un evento o enfermedad.
#' @param format_fecha Un `character` (cadena de caracteres)
#' que contiene  el formato deseado de la fecha; su valor por
#' defecto es `"\%Y-\%m-\%d"`.
#' @param nomb_cols Un `character` (cadena de caracteres) que
#' contiene los nombres de la columna a formatear en los datos de
#' una enfermedad o evento; su valor por defecto es `NULL`.
#' @return Un `data.frame` con los datos con las fechas formateadas.
#' @keywords internal
format_fecha <- function(data_event,
                         format_fecha = "%Y-%m-%d",
                         nomb_cols = NULL) {
  validar_data_event(data_event)
  validar_format_fecha(format_fecha)
  data_limpia <- data_event
  for (name in nomb_cols) {
    data_limpia[[name]] <- as.Date(data_event[[name]],
      format = format_fecha
    )
  }
  return(data_limpia)
}

#' @title Formatear código geográfico
#' @description Función que da el formato deseado a un código geográfico.
#' @param cod_geo Un `numeric` (númerico) o `character`
#' (cadena de caracteres) que contiene el código geográfico.
#' @param etiqueta Un `character` (cadena de caracteres) con el nombre
#' de la etiqueta de la validación relacionada a la longitud máxima del
#' código geográfico; se refiere al tipo de división geográfica ("municipio",
#' "departamento").
#' @param digitos Un `numeric` (númerico) que contiende el número de digitos
#' que debe tener individualmente el código geográfico.
#' @param tam Un `numeric` (númerico) que contiende el tamaño o la longitud
#' máxima que debe tener el código geográfico.
#' @return Un `character` (cadena de caracteres) con el código geográfico
#' formateado.
#' @keywords internal
format_cod_geo <- function(cod_geo, etiqueta, digitos, tam) {
  cod_format <- NULL
  if (is.numeric(cod_geo) ||
    !is.na(suppressWarnings(as.numeric(cod_geo)))) {
    cod_format <- formatC(cod_geo,
      width = digitos,
      format = "d",
      flag = "0"
    )
    if (nchar(cod_geo) > tam) {
      stop(
        "El codigo del ", etiqueta,
        " debe tener maximo ", tam, " digitos"
      )
    }
    if (nchar(cod_format) == tam - 1) {
      cod_format <- paste0("0", cod_format)
    }
  }
  return(cod_format)
}

#' @title Limpiar las etiquetas del encabezado
#' @description Función que limpia las etiquetas del encabezado de los
#' datos de una enfermedad o evento.
#' @param data_event Un `data.frame` que contiene los datos de una
#' enfermedad o evento.
#' @return Un `data.frame` con las etiquetas del encabezado formateadas
#' con guión bajo (_).
#' @examples
#' data(dengue2020)
#' limpiar_encabezado(data_event = dengue2020)
#' @export
limpiar_encabezado <- function(data_event) {
  validar_data_event(data_event)
  names(data_event) <- epitrix::clean_labels(names(data_event))
  return(data_event)
}

#' @title Limpiar fechas de los datos de una enfermedad o evento
#' @description Función que limpia y estandariza las fechas de los datos
#' de una enfermedad o evento.
#' @param data_event Un `data.frame` que contiene los datos de
#' una enfermedad o evento.
#' @param year Un `numeric` (númerico) o `character` (cadena de caracteres)
#' que contiene el año de los datos de una enfermedad o evento.
#' @param format_fecha Un `character` (cadena de caracteres) que contiene
#' el formato deseado de fecha; su valor por defecto es "\%AAAA-\%MM-\%DD".
#' @param col_fecha Un `character` (cadena de caracteres) que contiene
#' el nombre de la columna con la fecha que se desea limpiar en los datos
#' de la enfermedad o evento.
#' @param col_comp Un `character` (cadena de caracteres) que contiene el
#' nombre de la columna con la cual se va a comparar la columna `nomb_col`
#' para limpiarla, estandarizarla o aplicar las reglas definidas.
#' @return Un `data.frame` con las fechas limpias.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' limpiar_fecha_event(
#'   data_event = data_limpia,
#'   year = 2020,
#'   format_fecha = "%Y-%m-%d",
#'   col_fecha = "ini_sin",
#'   col_comp = "fec_hos"
#' )
#' @export
limpiar_fecha_event <- function(data_event,
                                year,
                                format_fecha = "%Y-%m-%d",
                                col_fecha = "ini_sin",
                                col_comp = NULL) {
  validar_data_event(data_event)
  stopifnot(
    "El parametro year es obligatorio" = !missing(year),
    "El parametro year debe ser una cadena de caracteres
            o numerico" =
      (is.numeric(year) && !is.character(year)) ||
        (!is.numeric(year) && is.character(year)),
    "El parametro col_fecha debe ser una cadena de caracteres" =
      is.character(col_fecha)
  )
  validar_format_fecha(format_fecha)
  data_event_fecha_ini <- data_event
  if (!is.null(col_comp)) {
    stopifnot(
      "El parametro col_comp debe ser una cadena de caracteres" =
        is.character(col_comp)
    )
    data_event_fecha_ini <-
      remove_error_fecha(
        data_event_fecha_ini,
        col_fecha,
        col_comp
      )
  }
  data_event_fecha_ini[order(data_event_fecha_ini[[col_fecha]],
    decreasing = TRUE
  ), ]
  data_event_fecha_ini <-
    data_event_fecha_ini[format(
      data_event_fecha_ini[[col_fecha]],
      "%Y"
    ) == year, ]
  return(data_event_fecha_ini)
}

#' @title Limpiar las edades de los datos de una enfermedad o evento
#' @description Función que limpia y estandariza las edades de los datos
#' de una enfermedad o evento, conviertiendolas en años, según la
#' clasificación del Instituto Nacional de Salud:
#' No aplica = 0
#' Años = 1
#' Meses = 2
#' Días = 3
#' Horas = 4
#' Minutos = 5.
#' @param data_event Un `data.frame` que contiene los datos de una
#' enfermedad o evento.
#' @param col_edad Un `character` (cadena de caracteres) con
#' el nombre de la columna que contiene las edades en los datos de
#' la enfermedad o evento; su valor por defecto es `"edad"`.
#' @return Un `data.frame` con los datos de una enfermedad o evento
#' con las edades limpias.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' limpiar_edad_event(data_event = data_limpia, col_edad = "edad")
#' @export
limpiar_edad_event <- function(data_event, col_edad = "edad") {
  validar_data_event(data_event)
  validar_edad(data_event, col_edad)
  data_event_years <- convert_edad(data_event)
  data_event_years <- remove_val_nin(data_event_years, col_edad)
}

#' @title Limpiar los valores atipicos de los datos
#' @description Función que limpia los valores atipicos de los datos
#' de una enfermedad o evento del SIVIGILA.
#' @param data_event Un `data.frame` que contiene los datos de una
#' enfermedad o evento.
#' @return Un `data.frame` con los datos de una enfermedad o
#' evento con los valores atípicos limpios (NA).
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_encabezado(data_event = dengue2020)
#' @export
limpiar_val_atipic <- function(data_event) {
  validar_data_event(data_event)
  cols_events <- obtener_val_config("diseases_exceptions")
  cod_event <- data_event$cod_eve[1]
  if (cod_event > 0) {
    for (event in cols_events) {
      cod <- names(event)
      if (cod_event %in% cod) {
        cols <- event[as.character(cod_event)]
        for (nom_cols in cols) {
          for (col in nom_cols) {
            data_event[, col] <- NA
          }
        }
      }
    }
  }
}

#' @title Limpiar datos de SIVIGILA
#' @description Función que limpia los datos seleccionados de una enfermedad
#' o evento de la fuente SIVIGILA.
#' @param data_event Un `data.frame` que contiene los datos de
#' una enfermedad o evento.
#' @return Un `data.frame` con los datos limpios de la enfermedad o evento.
#' @examples
#' data(dengue2020)
#' limpiar_data_sivigila(data_event = dengue2020)
#' @export
limpiar_data_sivigila <- function(data_event) {
  validar_data_event(data_event)
  data_event <- limpiar_encabezado(data_event)
  nom_cols_fechas <- obtener_val_config("dates_column_names")
  year <- names(sort(table(data_event$ano), decreasing = TRUE)[1])
  data_limpia <- format_fecha(data_event,
    nomb_cols = nom_cols_fechas
  )
  nombre <- unique(data_event$nombre_evento)
  if (length(nombre) == 1 &&
    !stringr::str_detect(nombre, stringr::fixed("MORTALIDAD"))) {
    data_limpia <- limpiar_fecha_event(data_limpia, year,
      col_fecha = nom_cols_fechas[3],
      col_comp = nom_cols_fechas[4]
    )
    data_limpia <- limpiar_fecha_event(data_limpia, year,
      col_fecha = nom_cols_fechas[2]
    )
  }
  data_limpia <- estandarizar_geo_cods(data_limpia)
  data_limpia <- convert_edad(data_limpia,
    col_edad = "edad",
    col_uni_med = "uni_med"
  )
  return(data_limpia)
}
