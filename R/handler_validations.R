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
