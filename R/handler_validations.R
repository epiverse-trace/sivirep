#' @title Validar parámetro `data_event`
#' @description Función que realiza las validaciones correspondientes
#' del parámetro `data_event`.
#' @param data_event Un `data.frame` que contiene los datos de
#' una enfermedad o evento.
#' @return \value{None}
#' @noRd
validar_data_event <- function(data_event) {
  stopifnot(
    "El parametro data_event es obligatorio" = !missing(data_event),
    "El parametro data_event debe ser un data.frame" =
      is.data.frame(data_event),
    "El parametro data_event no debe estar vacio" =
      nrow(data_event) > 0
  )
}

#' @title Validar parámetro `data_agrupada`
#' @description Función que realiza las validaciones correspondientes
#' del parámetro `data_agrupada`.
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados.
#' @return \value{None}
#' @noRd
validar_data_agrupada <- function(data_agrupada) {
  stopifnot(
    "El parametro data_agrupada es obligatorio" =
      !missing(data_agrupada),
    "El parametro data_agrupada debe ser un data.frame" =
      is.data.frame(data_agrupada),
    "El parametro data_agrupada no debe estar vacio" =
      nrow(data_agrupada) > 0
  )
}

#' @title Validar parámetro `nomb_cols`
#' @description Función que realiza las validaciones correspondientes
#' del parámetro `nomb_cols`.
#' @param data_event Un `data.frame` que contiene los datos de
#' una enfermedad o evento.
#' @param nomb_cols Un `character` (cadena de caracteres) o un
#' `array` (arreglo) de `character` que contiene el nombre de
#' la(s) columna(s) en los datos de la enfermedad o evento.
#' @return \value{None}
#' @noRd
validar_nomb_cols <- function(data_event, nomb_cols) {
  stopifnot(
    "El parametro nomb_cols es obligatorio" = !missing(nomb_cols),
    "El parametro nomb_cols debe ser una cadena de caracteres
            o un arreglo de cadenas de caracteres " =
      (is.character(nomb_cols) && !is.array(nomb_cols)) ||
        (!is.character(nomb_cols) && is.array(nomb_cols)),
    "La(s) columna(s) o variable(s) del parametro nomb_cols no
            se encuentra(n) en los datos de la enfermedad o evento" =
      nomb_cols %in% colnames(data_event)
  )
}

#' @title Validar los parámetros de la lógica de fecha de inicio de síntomas
#' @description Función que realiza las validaciones correspondientes a los
#' parámetros relacionados a lógica y análisis que se efectua sobre
#' la variable de fecha de inicio de síntomas.
#' @param data_event Un `data.frame` que contiene los datos de una enfermedad
#' o evento.
#' @param uni_marca Un `character` (cadena de caracteres) que contiene
#' la unidad de las marcas del gráfico (`"dia"`, `"semanaepi"` y `"mes"`).
#' @param col_fecha Un `character` (cadena de caracteres) que contiene el
#' nombre de la columna con las fechas de notificación en los datos de la
#' enfermedad o evento.
#' @param tipo Un `character` (cadena de caracteres) que contiene el tipo de
#' gráfico (`"barras"` o `"tendencia"`).
#' @return \value{None}
#' @noRd
validar_fecha_inisintomas <- function(data_event, col_fecha,
                                      uni_marca, tipo) {
  stopifnot(
    "El parametro col_fecha debe ser una cadena de caracteres" =
      is.character(col_fecha),
    "La columna o variable del parametro col_fecha no
            se encuentra en los datos de la enfermedad o evento" =
      col_fecha %in% colnames(data_event),
    "El parametro uni_marca debe ser una cadena de caracteres" =
      is.character(uni_marca),
    "Valor invalido para el parametro uni_marca" =
      uni_marca %in% c("dia", "semanaepi", "mes"),
    "El parametro tipo debe ser una cadena de caracteres" =
      is.character(tipo),
    "Valor invalido para el parametro tipo" =
      tipo %in% c("barras", "tendencia")
  )
}

#' @title Validar parámetro `format_fecha`
#' @description Función que realiza las validaciones correspondientes
#' del parámetro `format_fecha`.
#' @param format_fecha Un `character` (cadena de caracteres)
#' que contiene el formato deseado de la fecha.
#' @return \value{None}
#' @noRd
validar_format_fecha <- function(format_fecha) {
  stopifnot(
    "El parametro format_fecha debe ser una cadena de caracteres" =
      is.character(format_fecha)
  )
}

#' @title Validar parámetro `col_sex`
#' @description Función que realiza las validaciones correspondientes
#' del parámetro `col_sex`.
#' @param data_event Un `data.frame` que contiene los datos de una enfermedad
#' o evento.
#' @param col_sex Un `character` (cadena de caracteres) con el nombre
#' de la columna que contiene el sexo en los datos de la enfermedad o evento.
#' @return \value{None}
#' @noRd
validar_sex <- function(data_event, col_sex) {
  stopifnot(
    "El parametro col_sex debe ser una cadena de caracteres" =
      is.character(col_sex),
    "La columna o variable del parametro col_sex no
            se encuentra en los datos de la enfermedad o evento" =
      col_sex %in% colnames(data_event)
  )
}

#' @title Validar parámetro `porcentaje`
#' @description Función que realiza las validaciones correspondientes
#' del parámetro `porcentaje`.
#' @param porcentaje Un `logical` (TRUE o FALSE) que indica
#' si se debe agregar o tener una columna con el porcentaje de casos.
#' @return \value{None}
#' @noRd
validar_porcentaje <- function(porcentaje) {
  stopifnot(
    "El parametro porcentaje debe ser un booleano" =
      is.logical(porcentaje)
  )
}

#' @title Validar parámetro `col_edad`
#' @description Función que realiza las validaciones correspondientes
#' del parámetro `col_edad`.
#' @param data_event Un `data.frame` que contiene los datos de una enfermedad
#' o evento.
#' @param col_edad Un `character` (cadena de caracteres) con el nombre de la
#' columna que contiene las edades en los datos de la enfermedad o evento.
#' @return \value{None}
#' @noRd
validar_edad <- function(data_event, col_edad) {
  stopifnot(
    "El parametro col_edad debe ser una cadena de caracteres" =
      is.character(col_edad),
    "La columna o variable del parametro col_edad no
            se encuentra en los datos de la enfermedad o evento" =
      col_edad %in% colnames(data_event)
  )
}


#' @title Validar parámetro `col_area`
#' @description Función que realiza las validaciones correspondientes
#' del parámetro `col_area`.
#' @param data_event Un `data.frame` que contiene los datos de una enfermedad
#' o evento.
#' @param col_area Un `character` (cadena de caracteres) con el nombre de
#' la columna con el área geográfica en los datos de la enfermedad
#' o evento.
#' @return \value{None}
#' @noRd
validar_area_geo <- function(data_event, col_area, porcentaje) {
  stopifnot(
    "El parametro col_area debe ser una cadena de caracteres" =
      is.character(col_area),
    "La columna o variable del parametro col_area no
            se encuentra en los datos de la enfermedad o evento" =
      col_area %in% colnames(data_event)
  )
}

#' @title Validar parámetro `col_year`
#' @description Función que realiza las validaciones correspondientes
#' del parámetro `col_year`.
#' @param data_event Un `data.frame` que contiene los datos de una enfermedad
#' o evento.
#' @param col_year Un `character` (cadena de caracteres) con el nombre de
#' la columna que contiene el año en los datos de la enfermedad o evento.
#' @return \value{None}
#' @noRd
validar_years <- function(data_event, col_year) {
  stopifnot(
    "El parametro col_year debe ser una cadena de caracteres" =
      is.character(col_year),
    "La columna o variable del parametro col_year no
            se encuentra en los datos de la enfermedad o evento" =
      col_year %in% colnames(data_event)
  )
}

#' @title Validar parámetro `cols_etn`
#' @description Función que realiza las validaciones correspondientes
#' del parámetro `cols_etn`.
#' @param data_event Un `data.frame` que contiene los datos de una enfermedad
#' o evento.
#' @param cols_etn Un `character` (cadena de caracteres) con el nombre de
#' la(s) columna(s) que contiene(n) la pertenencia étnica en los datos de la
#' enfermedad o evento.
#' @return \value{None}
#' @noRd
validar_per_etn <- function(data_event, cols_etn) {
  stopifnot(
    "El parametro cols_etn debe ser una cadena de caracteres" =
      is.character(cols_etn),
    "La(s) columna(s) o variable(s) del parametro cols_etn no
            se encuentra(n) en los datos de la enfermedad o evento" =
      cols_etn %in% colnames(data_event)
  )
}

#' @title Validar parámetro `data_incidencia`
#' @description Función que realiza las validaciones correspondientes
#' del parámetro `data_incidencia`.
#' @param data_incidencia Un `data.frame` que contiene las proyecciones
#' poblacionales del DANE o las poblaciones a riesgo de la enfermedad
#' o evento.
#' @return \value{None}
#' @noRd
validar_data_incidencia <- function(data_incidencia) {
  stopifnot(
    "El parametro data_incidencia debe ser un data.frame" =
      is.data.frame(data_incidencia),
    "El parametro data_incidencia no debe estar vacio" =
      nrow(data_incidencia) > 0
  )
}

#' @title Validar parámetro `grupo_events`
#' @description Función que realiza las validaciones correspondientes
#' del parámetro `grupo_events`.
#' @param grupo_events Un `data.frame` que contiene los eventos relacionados o
#' subtipos de una enfermedad o evento.
#' @param nombre_event Un `character` (cadena de caracteres) que contiene el
#' nombre del evento o la enfermedad.
#' @param nombre_event_estandar Un `character` (cadena de caracteres) que
#' contiene el nombre del evento o la enfermedad estandarizado.
#' @return \value{None}
#' @noRd
validar_grupo_events <- function(grupo_events,
                                 nombre_event,
                                 nombre_event_estandar) {
  diff_length_name <- abs(nchar(grupo_events$enfermedad_estandarizada[1]) -
                            nchar(nombre_event_estandar))
  if (is.null(grupo_events) || nrow(grupo_events) == 0 ||
      diff_length_name > 4) {
    stop("\033[31mLa enfermedad o evento relacionado: ",
         nombre_event,
         " no esta disponible para su descarga\033[0m",
         call. = FALSE
    )
  }
}
