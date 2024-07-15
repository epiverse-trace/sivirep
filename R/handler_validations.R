#' @title Validar párametro `data_event`
#' @description Función que realiza las validaciones correspondientes
#' del párametro `data_event`.
#' @param data_event Un `data.frame` que contiene los datos de
#' una enfermedad o evento.
#' @noRd
validar_data_event <- function(data_event) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0)
}

#' @title Validar párametro `data_agrupada`
#' @description Función que realiza las validaciones correspondientes
#' del párametro `data_agrupada`.
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados.
#' @noRd
validar_data_agrupada <- function(data_agrupada) {
  stopifnot("El parametro data_agrupada es obligatorio" =
              !missing(data_agrupada),
            "El parametro data_agrupada debe ser un data.frame" =
              is.data.frame(data_agrupada),
            "El parametro data_agrupada no debe estar vacio" =
              nrow(data_agrupada) > 0)
}

#' @title Validar párametro `nomb_cols`
#' @description Función que realiza las validaciones correspondientes
#' del párametro `nomb_cols`.
#' @param data_event Un `data.frame` que contiene los datos de
#' una enfermedad o evento.
#' @param nomb_cols Un `character` (cadena de caracteres) o
#' `array (arreglo) de character` que contiene el nombre de
#' la(s) columna(s) en los datos de la enfermedad o evento.
#' @noRd
validar_nomb_cols <- function(data_event, nomb_cols) {
  stopifnot("El parametro nomb_cols es obligatorio"
            = !missing(nomb_cols),
            "El parametro nomb_cols debe ser una cadena de caracteres
            o un arreglo de cadenas de caracteres "
            = (is.character(nomb_cols) && !is.array(nomb_cols)) ||
              (!is.character(nomb_cols) && is.array(nomb_cols)),
            "La(s) columna(s) o variable(s) del parametro nomb_cols no
            se encuentra(n) en los datos de la enfermedad o evento" =
            nomb_cols %in% colnames(data_event))
}

#' @title Validar los párametros de la lógica de fecha de inicio de sintomas
#' @description Función que realiza las validaciones correspondientes a los
#' parametros relacionados a lógica y análisis que se efectua sobre
#' la variable de fecha de inicio de sintoma.
#' @param data_event Un `data.frame` que contiene los datos de una enfermedad
#' o evento.
#' @param uni_marca Un `character` (cadena de caracteres) que contiene
#' la unidad de las marcas del gráfico (`"dia"`, `"semanaepi"` y `"mes"``).
#' @param col_fecha Un `character` (cadena de caracteres) que contiene el
#' nombre de la columna con las fechas de notificación en los datos de la
#' enfermedad o evento.
#' @param tipo Un `character` (cadena de caracteres) que contiene el tipo de
#' gráfico (`"barras"` o `"tendencia"`).
#' @noRd
validar_fecha_inisintomas <- function(data_event, col_fecha,
                                             uni_marca, tipo) {
  stopifnot("El parametro col_fecha debe ser una cadena de caracteres" =
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
              tipo %in% c("barras", "tendencia"))
}

#' @title Validar párametro `format_fecha`
#' @description Función que realiza las validaciones correspondientes
#' del párametro `format_fecha`.
#' @param format_fecha Un `character` (cadena de caracteres)
#' que contiene  el formato deseado de la fecha.
#' @noRd
validar_format_fecha <- function(format_fecha) {
  stopifnot("El parametro format_fecha debe ser una cadena de caracteres" =
              is.character(format_fecha))
}

#' @title Validar párametro `col_sex`
#' @description Función que realiza las validaciones correspondientes
#' del párametro `col_sex`.
#' @param data_event Un `data.frame` que contiene los datos de una enfermedad
#' o evento.
#' @param col_sex Un `character` (cadena de caracteres) con el nombre
#' de la columna que contiene el sexo en los datos de la enfermedad o evento.
#' @noRd
validar_sex <- function(data_event, col_sex) {
  stopifnot("El parametro col_sex debe ser una cadena de caracteres" =
              is.character(col_sex),
            "La columna o variable del parametro col_sex no
            se encuentra en los datos de la enfermedad o evento" =
            col_sex %in% colnames(data_event))
}

#' @title Validar párametro `porcentaje`
#' @description Función que realiza las validaciones correspondientes
#' del párametro `porcentaje`.
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica
#' si se debe agregar o tener una columna con el porcentaje de casos.
#' @noRd
validar_porcentaje <- function(porcentaje) {
   stopifnot("El parametro porcentaje debe ser un booleano" =
              is.logical(porcentaje))
}

#' @title Validar párametro `col_edad`
#' @description Función que realiza las validaciones correspondientes
#' del párametro `col_edad`.
#' @param data_event Un `data.frame` que contiene los datos de una enfermedad
#' o evento.
#' @param col_edad Un `character` (cadena de caracteres) con el nombre de la
#' columna que contiene las edades en los datos de la enfermdedad o evento.
#' @noRd
validar_edad <- function(data_event, col_edad) {
  stopifnot("El parametro col_edad debe ser una cadena de caracteres"
            = is.character(col_edad),
            "La columna o variable del parametro col_edad no
            se encuentra en los datos de la enfermedad o evento" =
            col_edad %in% colnames(data_event))
}


#' @title Validar párametro `col_area`
#' @description Función que realiza las validaciones correspondientes
#' del párametro `col_area`.
#' @param data_event Un `data.frame` que contiene los datos de una enfermedad
#' o evento.
#' @param col_area Un `character` (cadena de carácteres) con el nombre de
#' la columna con el área geografica en los datos de la enfermedad
#' o evento.
#' @noRd
validar_area_geo <- function(data_event, col_area, porcentaje) {
  stopifnot("El parametro col_area debe ser una cadena de caracteres"
            = is.character(col_area),
            "La columna o variable del parametro col_area no
            se encuentra en los datos de la enfermedad o evento" =
            col_area %in% colnames(data_event))
}

#' @title Validar párametro `col_year`
#' @description Función que realiza las validaciones correspondientes
#' del párametro `col_year`.
#' @param data_event Un `data.frame` que contiene los datos de una enfermedad
#' o evento.
#' @param col_year Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene el año en los datos de la enfermedad o evento.
#' @noRd
validar_years <- function(data_event, col_year) {
  stopifnot("El parametro col_year debe ser una cadena de caracteres"
            = is.character(col_year),
            "La columna o variable del parametro col_year no
            se encuentra en los datos de la enfermedad o evento" =
            col_year %in% colnames(data_event))
}

#' @title Validar párametro `cols_etn`
#' @description Función que realiza las validaciones correspondientes
#' del párametro `cols_etn`.
#' @param data_event Un `data.frame` que contiene los datos de una enfermedad
#' o evento.
#' @param cols_etn Un `character` (cadena de caracteres) con el nombre de
#' las columna(s) que contiene(n) la pertenencia étnica en los datos de la
#' enfermedad o evento.
#' @noRd
validar_per_etn <- function(data_event, cols_etn) {
  stopifnot("El parametro cols_etn debe ser una cadena de caracteres"
            = is.character(cols_etn),
            "La(s) columna(s) o variable(s) del parametro cols_etn no
            se encuentra(n) en los datos de la enfermedad o evento" =
            cols_etn %in% colnames(data_event))
}

#' @title Validar párametro `data_incidencia`
#' @description Función que realiza las validaciones correspondientes
#' del párametro `data_incidencia`.
#' @param data_incidencia Un `data.frame` que contiene la proyecciones
#' poblacionales del DANE o las poblaciones a riesgo de la enfermedad
#' o evento.
#' @noRd
validar_data_incidencia <- function(data_incidencia) {
  stopifnot("El parametro data_incidencia debe ser un data.frame" =
                is.data.frame(data_incidencia),
              "El parametro data_incidencia no debe estar vacio" =
                nrow(data_incidencia) > 0)
}
