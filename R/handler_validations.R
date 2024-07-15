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
