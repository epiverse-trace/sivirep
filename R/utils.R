#' Obtener los meses con mayor número de casos
#'
#' Función que obtiene los meses con el mayor número de casos
#' @param data_event Un data frame con los datos de la enfermedad
#' o vento
#' @param col_fechas Un array (arreglo) de character (cadena de caracteres)
#' con los nombres de columna de los datos de la enfermedad o evento
#' que contienen las fechas
#' @param col_casos Un character (cadena de caracteres) con el nombre de la
#' columna de los datos de la enfermedad o evento que contiene el número
#' de casos; su valor por defecto es "casos"
#' @param top Un numeric (numerico) que contiene la cantidad máxima
#' de meses a retornar; su valor por defecto es 3
#' @param concat_vals Un boolean (TRUE/FALSE) que indica si se requiere
#' concatenar los meses como una cadena; su valor por defecto es TRUE
#' @return Un data frame que contiene los meses con mayor número de casos
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' casos_inisintomas <- agrupar_fecha_inisintomas(data_event)
#' obtener_meses_mas_casos(data_event= casos_inisintomas,
#'                         col_fechas = "ini_sin",
#'                         col_casos = "casos",
#'                         top = 3,
#'                         concat_vals = TRUE)
#' @export
obtener_meses_mas_casos <- function(data_event,
                                    col_fechas,
                                    col_casos = "casos",
                                    top = 1,
                                    concat_vals = TRUE) {
  data_mas_casos <-
    data_event[order(eval(parse(text = paste0("data_event$", col_casos))),
                     decreasing = TRUE), ]
  if (nrow(data_mas_casos) < top) {
    top <- nrow(data_mas_casos)
  }
  data_mas_casos <- data_mas_casos[1:top, ]
  data_mas_casos$Meses <-
    sapply(eval(parse(text = paste0("data_mas_casos$",
                                    col_fechas))),
           months)
  if (concat_vals && length(data_mas_casos$Meses) >= 2) {
    months_concat <-
      concatenar_vals_token(as.character(data_mas_casos$Meses)[1:top])
    return(months_concat)
  }
  return(data_mas_casos)
}

#' Obtener nombres de departamentos
#'
#' Función que obtiene los nombres de los departamentos
#' @param data_event Un data frame que contiene los datos de
#' la enfermedad o evento
#' @return Un data frame con los nombres de los departamentos
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' @export
obtener_nombres_dptos <- function(data_event) {
  data_event_dptos <- data_event
  data_event_dptos$codigo <- data_event$id
  geo_country_data <- import_geo_cods()
  deptos_data <- data.frame(
    id = geo_country_data$c_digo_departamento,
    nombre = geo_country_data$nombre_departamento
  )
  i <- 1
  for (code in deptos_data$id) {
    data_event_dptos$id[data_event_dptos$id == code] <-
      deptos_data$nombre[i]
    i <- i + 1
  }
  colnames(data_event_dptos)[colnames(data_event_dptos) == "id"] <-
    "nombre"
  data_event_dptos <- data_event_dptos[order(data_event_dptos$nombre,
                                             decreasing = FALSE), ]
  data_event_dptos <- data_event_dptos[5:nrow(data_event_dptos), ]
  return(data_event_dptos)
}

#' Obtener fila con mayor número de casos
#'
#' Función que obtiene la fila con el mayor número de casos
#' @param data_event Un data frame que contiene los datos de la
#' enfermedad o evento
#' @param col_name Un character (cadena de caracteres) con el
#' nombre de la columna de los datos de la enfermedad o evento que
#' contiene el número de casos
#' @param porcentaje Un boolean (TRUE/FALSE) que indica si se
#' requiere agregar un porcentaje de casos como columna
#' @return Un data frame que contiene la fila con mayor número de casos
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' casos_sex <- agrupar_sex(data_event,
#'                          porcentaje = TRUE)
#' obtener_fila_mas_casos(data_event = casos_sex,
#'                        col_name = "casos",
#'                        porcentaje = TRUE)
#' @export
obtener_fila_mas_casos <- function(data_event,
                                   col_name = "casos",
                                   porcentaje = TRUE) {
  data_mas_casos <- data_event[order(eval(parse(text =
                                                  paste0("data_event$",
                                                         col_name))),
                                     decreasing = TRUE), ]
  data_mas_casos <- data_mas_casos[1, ]
  if (porcentaje) {
    value_per <-
      data_mas_casos$casos[1] / sum(eval(parse(text =
                                                 paste0("data_event$",
                                                        col_name))))
    data_mas_casos$porcentaje <- round(value_per * 100, 2)
  }
  return(data_mas_casos)
}

#' Concatenar valores con separador o token
#'
#' Función que concatena valores con un separador o token específico
#' @param vals Un array (arreglo) de character (cadena de caracteres)
#' que contiene los valores que se desean concatenar
#' @param longitud Un numeric (numerico) que contiene la longitud de
#' los valores que se desean concatenar; su valor por defecto es 3
#' @param princ_token Un character (cadena de caracteres) que contiene el
#' separador o token principal; su valor por defecto es ", "
#' @param final_token Un character (cadena de caracteres) que contien el
#' separador o token final; su valor por defecto es "y "
#' @return Un character (cadena de caracteres) con el valor final concatenado
#' @examples
#' concatenar_vals_token(vals = c("enero", "febrero", "marzo"),
#'                       longitud = 3,
#'                       princ_token = ", ",
#'                       final_token = "y ")
#' @export
concatenar_vals_token <- function(vals,
                                  longitud = 3,
                                  princ_token = ", ",
                                  final_token = "y ") {
  final_val <- ""
  i <- 1
  for (value in vals) {
    if (i != longitud) {
      final_val <- paste0(final_val, value, princ_token)
    } else {
      final_val <- paste0(final_val, final_token, value)
    }
    i <- i + 1
  }
  return(final_val)
}

#' Obtener columnas de ocurrencia geográfica de los datos de la
#' enfermedad o evento
#'
#' Función que obtiene las columnas de ocurrencia geográfica de los
#' datos de la enfermedad o evento
#' @param cod_event Un numeric (numerico) que contiene el código de la
#' enfermedad o evento
#' @param nombre_event Un character (cadena de caracteres) con el nombre de
#' la enfermedad o evento
#' @return Un data frame con las columnas de ocurrencia geográfica de los
#' datos de la enfermedad
#' @examples
#' obtener_tip_ocurren_geo(cod_event = 210)
#' @export
obtener_tip_ocurren_geo <- function(cod_event = NULL, nombre_event = NULL) {
  geo_occurren <- config::get(file =
                                system.file("extdata",
                                            "config.yml",
                                            package = "sivirep"),
                              "occurrence_geo_diseases")
  col_ocurren <- c("cod_dpto_o", "cod_mun_o", "ocurrencia")
  param_busqueda <- cod_event
  if (!is.null(nombre_event)) {
    param_busqueda <- nombre_event
  }
  if (length(grep(param_busqueda, geo_occurren$cod_dpto_n)) == 1
      && grep(param_busqueda, geo_occurren$cod_dpto_n) > 0) {
    col_ocurren <- c("cod_dpto_n", "cod_mun_n", "notificacion")
  } else if (length(grep(param_busqueda, geo_occurren$cod_dpto_r)) == 1
             && grep(param_busqueda, geo_occurren$cod_dpto_r) > 0) {
    col_ocurren <- c("cod_dpto_r", "cod_mun_r", "residencia")
  } else {
    col_ocurren <- c("cod_dpto_o", "cod_mun_o", "ocurrencia")
  }
  return(col_ocurren)
}

#' Obtener información geográfica de los datos de la enfermedad o evento
#'
#' Función que obtiene la información geográfica de los datos de la enfermedad
#' o evento
#' @param dpto Un character (cadena de caracteres) que contiene el nombre
#' del departamento; su valor por defecto es NULL
#' @param munpio Un character (cadena de caracteres) que contiene los datos del
#' municipio; su valor por defecto es NULL
#' @return Un data frame con la información geográfica de los datos de
#' la enfermedad o evento
#' @examples
#' obtener_info_depts(dpto = "ANTIOQUIA")
#' @export
obtener_info_depts <- function(dpto = NULL, munpio = NULL) {
  data_geo <- import_geo_cods()
  list_dptos <- unique(data_geo$nombre_departamento)
  list_specific <-
    list_dptos[stringr::str_detect(list_dptos,
                                   toupper(dpto))]
  data_dpto <- dplyr::filter(data_geo, .data$nombre_departamento %in%
                               list_specific)
  if (!is.null(munpio)) {
    list_municipalities <- unique(data_geo$nombre_municipio)
    list_specific <-
      list_municipalities[stringr::str_detect(list_municipalities,
                                              toupper(munpio))]
    data_dpto <- dplyr::filter(data_geo, .data$nombre_municipio %in%
                                 list_specific)
  }
  return(data_dpto)
}

#' Establecer códigos geográficos de los datos de la enfermedad o evento
#'
#' Función que establece los códigos geográficos de los datos de la enfermedad
#' o evento
#' @param code_dept Un numeric (numerico) que contiene el código del
#' departamento
#' @param cod_mun Un numeric (numerico) que contiene el código del municipio
#' @return Un data frame con los códigos geográficos
#' @examples
#' modficar_cod_mun(code_dept = 01, cod_mun = "001")
#' @export
modficar_cod_mun <- function(code_dept, cod_mun) {
  cod_mun <- as.character(cod_mun)
  if (substr(code_dept, 1, 1) == "0") {
    code_dept <- substr(code_dept, 2, 2)
    cod_mun <- gsub(code_dept, "", cod_mun)
  }
  return(cod_mun)
}

#' Obtener departamentos de Colombia
#'
#' Función que obtiene los departamentos de Colombia
#' @return Un data frame con los departamentos de Colombia
#' @examples
#' obtener_dptos()
#' @export
obtener_dptos <- function() {
  dptos <- config::get(file =
                         system.file("extdata", "config.yml",
                                     package = "sivirep"),
                       "departments")
  return(dptos)
}

#' Obtener nombre del municipio en Colombia
#'
#' Función que obtiene el nombre del municipio
#' @param data_geo Un data frame que contiene los códigos
#' geográficos (departamentos y municipios de Colombia)
#' @param code_dept Un numeric (numerico) que contiene el código
#' del departamento
#' @param cod_mun Un numeric (numerico) que contiene el código
#' del municipio
#' @return Un character (cadena de caracteres) con el nombre del municipio
#' @examples
#' data_geo <- import_geo_cods()
#' obtener_nombres_muns(data_geo,
#'                      code_dept = "05",
#'                      cod_mun = "001")
#' @export
obtener_nombres_muns <- function(data_geo, code_dept, cod_mun) {
  if (substr(code_dept, 1, 1) == "0") {
    code_dept <- substr(code_dept, 2, 2)
    cod_mun <- paste0(code_dept, cod_mun)
  } else {
    cod_mun <- paste0(code_dept, cod_mun)
  }
  data_mun <- dplyr::filter(data_geo,
                            .data$codigo_municipio %in% as.integer(cod_mun))
  data_mun <- data_mun[1, ]
  return(data_mun$nombre_municipio)
}
