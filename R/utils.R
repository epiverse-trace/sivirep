#' Obtener los meses con mayor número de casos
#'
#' Función que obtiene los meses con el mayor número de casos
#' @param data_event Un `data.frame` con los datos de la enfermedad
#' o vento
#' @param col_fechas Un `array` (arreglo) de `character` (cadena de caracteres)
#' con los nombres de columna de los datos de la enfermedad o evento
#' que contienen las fechas
#' @param col_casos Un `character` (cadena de caracteres) con el nombre de la
#' columna de los datos de la enfermedad o evento que contiene el número
#' de casos; su valor por defecto es `"casos"`
#' @param top Un `numeric` (numerico) que contiene la cantidad máxima
#' de meses a retornar; su valor por defecto es `3`
#' @param concat_vals Un `boolean` (TRUE/FALSE) que indica si se requiere
#' concatenar los meses como una cadena; su valor por defecto es `TRUE`
#' @return Un `data.frame` que contiene los meses con mayor número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' casos_inisintomas <- agrupar_fecha_inisintomas(data_limpia)
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
  stopifnot("El parametro data_event es obligatorio" =
              !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_agrupada no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro col_fechas es obligatorio"
            = !missing(col_fechas),
            "El parametro col_fechas debe ser un cadena de caracteres"
            = is.character(col_fechas),
            "El parametro col_casos debe ser un cadena de caracteres"
            = is.character(col_casos),
            "El parametro top debe ser numerico"
            = is.numeric(top))
  data_mas_casos <-
    data_event[order(eval(parse(text = paste0("data_event$", col_casos))),
                     decreasing = TRUE), ]
  if (nrow(data_mas_casos) < top) {
    top <- nrow(data_mas_casos)
  }
  data_mas_casos <- data_mas_casos[1:top, ]
  mes <- strftime(data_mas_casos[[col_fechas]], "%m")
  mes <- as.numeric(mes)
  etiquetas <- config::get(file =
                             system.file("extdata",
                                         "config.yml",
                                         package = "sivirep"),
                           "months")
  etiquetas <- etiquetas[mes]
  data_mas_casos$meses <- etiquetas
  if (concat_vals && length(data_mas_casos$meses) >= 2) {
    meses_concat <-
      concatenar_vals_token(as.character(data_mas_casos$meses)[1:top])
    return(meses_concat)
  }
  return(data_mas_casos)
}

#' Obtener fila con mayor número de casos
#'
#' Función que obtiene la fila con el mayor número de casos
#' @param data_event Un `data.frame` que contiene los datos de la
#' enfermedad o evento
#' @param nomb_col Un `character` (cadena de caracteres) con el
#' nombre de la columna de los datos de la enfermedad o evento que
#' contiene el número de casos
#' @param porcentaje Un `boolean` (TRUE/FALSE) que indica si se
#' requiere agregar un porcentaje de casos como columna
#' @return Un `data.frame` que contiene la fila con mayor número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' casos_sex <- agrupar_sex(data_event = data_limpia,
#'                          porcentaje = TRUE)
#' obtener_fila_mas_casos(data_event = casos_sex,
#'                        nomb_col = "casos",
#'                        porcentaje = TRUE)
#' @export
obtener_fila_mas_casos <- function(data_event,
                                   nomb_col = "casos",
                                   porcentaje = TRUE) {
  stopifnot("El parametro data_event es obligatorio" =
              !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_agrupada no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro nomb_col debe ser un cadena de caracteres"
            = is.character(nomb_col),
            "El parametro porcentaje debe ser booleano"
            = is.logical(porcentaje))
  data_mas_casos <- data_event[order(eval(parse(text =
                                                  paste0("data_event$",
                                                         nomb_col))),
                                     decreasing = TRUE), ]
  data_mas_casos <- data_mas_casos[1, ]
  if (porcentaje) {
    value_per <-
      data_mas_casos$casos[1] / sum(eval(parse(text =
                                                 paste0("data_event$",
                                                        nomb_col))))
    data_mas_casos$porcentaje <- round(value_per * 100, 2)
  }
  return(data_mas_casos)
}

#' Concatenar valores con separador o token
#'
#' Función que concatena valores con un separador o token específico
#' @param vals Un `array` (arreglo) de character (cadena de caracteres)
#' que contiene los valores que se desean concatenar
#' @param longitud Un `numeric` (numerico) que contiene la longitud de
#' los valores que se desean concatenar; su valor por defecto es `3`
#' @param princ_token Un `character` (cadena de caracteres) que contiene el
#' separador o token principal; su valor por defecto es `", "`
#' @param final_token Un `character` (cadena de caracteres) que contien el
#' separador o token final; su valor por defecto es `"y "`
#' @return Un `character` (cadena de caracteres) con el valor final concatenado
#' @keywords internal
concatenar_vals_token <- function(vals,
                                  longitud = 3,
                                  princ_token = ", ",
                                  final_token = "y ") {
  stopifnot("El parametro vals es obligatorio" =
              !missing(vals),
            "El parametro vals debe ser una cadena de caracteres o
            un arreglo de cadenas de caracteres"
            = is.character(vals),
            "El parametro princ_token debe ser un cadena de caracteres"
            = is.character(princ_token),
            "El parametro final_token debe ser un cadena de caracteres"
            = is.character(final_token))
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
#' @param cod_event Un `numeric` (numerico) o `character`
#' (cadena de caracteres) que contiene el código de la
#' enfermedad o evento
#' @param nombre_event Un `character` (cadena de caracteres) con el nombre de
#' la enfermedad o evento
#' @return Un `data.frame` con las columnas de ocurrencia geográfica de los
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
  param_busqueda <- NULL
  if (!is.null(cod_event)) {
      param_busqueda <- cod_event
      stopifnot("El parametro cod_event es obligatorio" =
              !missing(cod_event),
            "El parametro cod_event debe ser una cadena de caracteres o
            numerico" =
              (is.numeric(cod_event) && !is.character(cod_event)) ||
              (!is.numeric(cod_event) && is.character(cod_event)))
  }
  if (!is.null(nombre_event)) {
    stopifnot("El parametro nombre_event debe ser un cadena de caracteres"
            = is.character(nombre_event))
    param_busqueda <- nombre_event
  }
  if (length(grep(param_busqueda, geo_occurren$cod_dpto_n)) == 1
      && grep(param_busqueda, geo_occurren$cod_dpto_n) > 0) {
    col_ocurren <- c("cod_dpto_n", "departamento_notificacion",
                     "cod_mun_n", "municipio_notificacion", "notificacion")
  } else if (length(grep(param_busqueda, geo_occurren$cod_dpto_r)) == 1
             && grep(param_busqueda, geo_occurren$cod_dpto_r) > 0) {
    col_ocurren <- c("cod_dpto_r", "departamento_residencia",
                     "cod_mun_r", "municipio_residencia", "residencia")
  } else {
    col_ocurren <- c("cod_dpto_o", "departamento_ocurrencia",
                     "cod_mun_o", "municipio_ocurrencia", "ocurrencia")
  }
  return(col_ocurren)
}

#' Obtener información geográfica de los datos de la enfermedad o evento
#'
#' Función que obtiene la información geográfica de los datos de la enfermedad
#' o evento
#' @param dpto Un `character` (cadena de caracteres) o `numeric` (numerico)
#' que contiene el nombre del departamento; su valor por defecto es `NULL`
#' @param mpio Un `character` (cadena de caracteres) o `numeric` (numerico)
#' que contiene los datos del municipio; su valor por defecto es `NULL`
#' @return Un `data.frame` con la información geográfica de los datos de
#' la enfermedad o evento
#' @examples
#' obtener_info_depts(dpto = "ANTIOQUIA")
#' obtener_info_depts(dpto = "ANTIOQUIA", mpio = "MEDELLIN")
#' obtener_info_depts(dpto = "05")
#' obtener_info_depts(dpto = "05", mpio = "05001")
#' obtener_info_depts(dpto = 05, mpio = 05001)
#' obtener_info_depts(dpto = 05, mpio = 001)
#' obtener_info_depts(dpto = "bogota dc", mpio = "bogota dc")
#' @export
obtener_info_depts <- function(dpto = NULL, mpio = NULL) {
  stopifnot("El parametro dpto es obligatorio" =
              !missing(dpto),
            "El parametro dpto debe ser una cadena de caracteres" =
              is.character(dpto) || is.numeric(dpto))
  data_geo <- import_geo_cods()
  data_dpto <- NULL
  dpto_busqueda <- dpto
  col_dpto <- "nombre_departamento"
  if (is.numeric(dpto_busqueda) ||
      !is.na(suppressWarnings(as.numeric(dpto_busqueda)))) {
    col_dpto <- "codigo_departamento"
    dpto_busqueda <- format_cod_geo(cod_geo = dpto_busqueda,
                                    etiqueta = "departamento",
                                    digitos = 2, tam = 2)
  }
  list_dptos <- unique(data_geo[[col_dpto]])
  dpto_busqueda <-  tolower(dpto_busqueda)
  dpto_busqueda <- epitrix::clean_labels(dpto_busqueda)
  list_specific <-
      list_dptos[stringr::str_detect(list_dptos, dpto_busqueda)]
  if (length(list_specific) > 1) {
    warning("Dos o mas departamentos coinciden con el nombre o ",
            "codigo ingresado: ",
            dpto, " se tomara el valor de la primera coincidencia ",
            "encontrada")
    data_dpto <- dplyr::filter(data_geo,
                               .data[[col_dpto]] == dpto_busqueda)
  } else {
    data_dpto <- dplyr::filter(data_geo, .data[[col_dpto]] %in%
                                 list_specific)
  }
  if (!is.null(mpio)) {
    stopifnot("El parametro mpio debe ser una cadena de caracteres o un
              numero"
              = is.character(mpio) || is.numeric(mpio))
    mpio_busqueda <- mpio
    col_mpio <- "nombre_municipio"
    if (is.numeric(mpio_busqueda) ||
        !is.na(suppressWarnings(as.numeric(mpio_busqueda)))) {
      col_mpio <- "codigo_municipio"
      mpio_busqueda <- format_cod_geo(cod_geo = mpio_busqueda,
                                      etiqueta = "municipio",
                                      digitos = 3, tam = 5)
    }
    if (nchar(mpio_busqueda) == 3) {
      mpio_busqueda <- paste0(dpto_busqueda, mpio_busqueda)
    }
    mpio_busqueda <- tolower(mpio_busqueda)
    mpio_busqueda <- epitrix::clean_labels(mpio_busqueda)
    data_dpto <-
      data_dpto[which(stringr::str_detect(data_dpto[[col_mpio]],
                                        mpio_busqueda)), ]
    data_mpio <- dplyr::filter(data_dpto,
                               data_dpto[[col_mpio]] == mpio_busqueda)
    if (nrow(data_mpio) == 1) {
        data_dpto <- data_mpio
    }
    if (nrow(data_dpto) > 1) {
      warning("Dos o mas municipios coinciden con el nombre o ",
              "codigo ingresado: ",
              mpio, " se tomara el valor de la primera coincidencia ",
              "encontrada")
      data_dpto <- data_dpto[1, ]
    }
  }
  return(data_dpto)
}

#' Obtener departamentos de Colombia
#'
#' Función que obtiene los departamentos de Colombia
#' @return Un `data.frame` con los departamentos de Colombia
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

#' Obtener el nombre de un departamento de Colombia
#'
#' Función que obtiene el nombre de un departamento de Colombia a
#' partir de su código geográfico
#' @param data_geo Un `data.frame` que contiene los códigos
#' geográficos (departamentos y municipios de Colombia)
#' @param cod_dpto Un `numeric` (numerico) o `character`
#' (cadena de caracteres) que contiene el código
#' del departamento
#' @return Un `character` (cadena de caracteres) con el nombre del
#' departamento
#' @examples
#' data_geo <- import_geo_cods()
#' obtener_nombre_dpto(data_geo,
#'                     cod_dpto = "05")
#' obtener_nombre_dpto(data_geo,
#'                     cod_dpto = 05)
#' obtener_nombre_dpto(data_geo,
#'                     cod_dpto = 5)
#' @export
obtener_nombre_dpto <- function(data_geo, cod_dpto) {
  stopifnot("El parametro data_geo es obligatorio" =
              !missing(data_geo),
            "El parametro data_geo debe ser un data.frame" =
              is.data.frame(data_geo),
            "El parametro data_geo no debe estar vacio" =
              nrow(data_geo) > 0,
            "El parametro cod_dpto es obligatorio" =
              !missing(cod_dpto),
            "El parametro cod_dpto debe ser una cadena de caracteres
             o numerico" =
              (is.numeric(cod_dpto) && !is.character(cod_dpto)) ||
              (!is.numeric(cod_dpto) && is.character(cod_dpto)))
  cod_dpto <- format_cod_geo(cod_geo = cod_dpto, etiqueta = "departamento",
                             digitos = 2, tam = 2)
  data_dpto <- dplyr::filter(data_geo,
                               .data$codigo_departamento %in% cod_dpto)
  data_dpto <- data_dpto[1, ]
  return(data_dpto$nombre_departamento)
}

#' Obtener el nombre de un departamento o municipio de Colombia
#'
#' Función que obtiene el nombre de un municipio de Colombi a
#' partir de su código geográfico
#' @param data_geo Un `data.frame` que contiene los códigos
#' geográficos (departamentos y municipios de Colombia)
#' @param cod_dpto Un `numeric` (numerico) o `character`
#' (cadena de caracteres) que contiene el código
#' del departamento
#' @param cod_mpio Un `numeric` (numerico) o `character`
#' (cadena de caracteres) que contiene el código
#' del municipio
#' @return Un `character` (cadena de caracteres) con el nombre del municipio
#' @examples
#' data_geo <- import_geo_cods()
#' obtener_nombre_mpio(data_geo,
#'                     cod_dpto = "05",
#'                     cod_mpio = "001")
#' obtener_nombre_mpio(data_geo,
#'                     cod_dpto = 05,
#'                     cod_mpio = 001)
#' obtener_nombre_mpio(data_geo,
#'                     cod_dpto = 5,
#'                     cod_mpio = 1)
#' @export
obtener_nombre_mpio <- function(data_geo, cod_dpto, cod_mpio) {
  stopifnot("El parametro data_geo es obligatorio" =
              !missing(data_geo),
            "El parametro data_geo debe ser un data.frame" =
              is.data.frame(data_geo),
            "El parametro data_geo no debe estar vacio" =
              nrow(data_geo) > 0,
            "El parametro cod_dpto es obligatorio" =
              !missing(cod_dpto),
            "El parametro cod_dpto debe ser una cadena de caracteres
             o numerico" =
            (is.numeric(cod_dpto) && !is.character(cod_dpto)) ||
            (!is.numeric(cod_dpto) && is.character(cod_dpto)),
            "El parametro cod_mpio es obligatorio" =
              !missing(cod_dpto),
            "El parametro cod_mpio debe ser una cadena de caracteres 
            o numerico" =
              (is.numeric(cod_mpio) && !is.character(cod_mpio)) ||
              (!is.numeric(cod_mpio) && is.character(cod_mpio)))
  cod_dpto <- format_cod_geo(cod_geo = cod_dpto, etiqueta = "departamento",
                             digitos = 2, tam = 2)
  cod_mpio <- format_cod_geo(cod_geo = cod_mpio, etiqueta = "municipio",
                             digitos = 3, tam = 5)
  cod_mpio <- paste0(cod_dpto, cod_mpio)
  data_mpio <- dplyr::filter(data_geo,
                               .data$codigo_municipio %in% cod_mpio)
  data_mpio <- data_mpio[1, ]
  return(data_mpio$nombre_municipio)
}

#' Formatear código geográfico
#'
#' Función que da el formato deseado a un código geográfico
#' @param cod_geo Un `numeric` (numerico) o `character`
#' (cadena de caracteres) que contiene el código
#' geográfico
#' @param etiqueta Un `character` (cadena de caracteres) con el nombre
#' de la etiqueta de la validación relacionada a la longitud máxima del
#' código geográfico; se refiere al tipo de división geográfica ("municipio",
#' "departamento")
#' @param digitos Un `numeric` (numerico) que contiende el número de digitos
#' que debe tener individualmente el código geográfico
#' @param tam Un `numeric` (numerico) que contiende el tamaño o la longitud
#' máxima que debe tener el código geográfico
#' @return Un `character` (cadena de caracteres) con el código geográfico
#' formateado
#' @keywords internal
format_cod_geo <- function(cod_geo, etiqueta, digitos, tam) {
  cod_format <- NULL
  if (is.numeric(cod_geo) ||
      !is.na(suppressWarnings(as.numeric(cod_geo)))) {
    cod_format <- formatC(cod_geo,
                          width = digitos,
                          format = "d",
                          flag = "0")
    etiqueta <- paste0("El codigo del ", etiqueta,
                       " debe tener maximo ", tam, " digitos")
    if (nchar(cod_geo) > tam) {
      stop(etiqueta)
    }
    if (nchar(cod_format) == tam - 1) {
      cod_format <- paste0("0", cod_format)
    }
  }
  return(cod_format)
}

#' Obtener los eventos relacionados
#'
#' Función que obtiene los eventos relacionados o tipos de un evento
#' principal
#' @param years Un `numeric` (numerico) el año  o años deseados para
#' la descarga de los datos
#' @param nombre_event Un `character` (cadena de caracteres) con el
#' nombre de la enfermedad o evento
#' @return Un `array` con los eventos relacionados por año desde
#' los microdatos de SIVIGILA
#' @keywords internal
obtener_eventos_relacionados <- function(nombre_event, years) {
  list_events <- list_events()
  grupo_events <-
    list_events[which(stringr::str_detect(list_events$enfermedad,
                                          substr(nombre_event,
                                                 1,
                                                 nchar(nombre_event) - 1))), ]
  list_events_relacionados <- config::get(file =
                                            system.file("extdata",
                                                        "config.yml",
                                                        package = "sivirep"),
                                          "related_diseases")
  list_events_relacionados <- lapply(list_events_relacionados,
                                     stringr::str_to_title)
  if (length(list_events_relacionados) > 0) {
    events_relacionados <- list_events_relacionados[[nombre_event]]
    for (year in years) {
      for (event in events_relacionados) {
        grupo_events_relacionados <-
          list_events[which(list_events$enfermedad == event), ]
        if (is.null(grupo_events) || nrow(grupo_events) == 0) {
          warning("La enfermedad o evento relacionado: ",
                  event,
                  "no esta disponible para su descarga", call. = FALSE)
        } else if (stringr::str_detect(grupo_events_relacionados$aa,
                                       as.character(year))) {
          warning("El year: ", year,
                  "de la enfermedad o evento relacionado: ",
                  event,
                  "no esta disponible para su descarga", call. = FALSE)
        } else {
          grupo_events <- rbind(grupo_events, grupo_events_relacionados)
        }
      }
    }
  }
  return(grupo_events)
}

#' Obtener las condiciones para calcular la incidencia de una
#' enfermedad o evento
#'
#' Función que obtiene las condiciones del numerador, denominador
#' y coeficiente de múltiplicación para calcular la incidencia de un
#' evento
#' @param cod_eve Un `numeric` (numerico) o `character` (cadena de
#' caracteres) que contiene el código de una enfermedad o evento
#' @return Un `data.frame` con las condiciones para calcular la
#' incidencia de una enfermedad o evento
#' @examples
#' obtener_cond_inciden_event(cod_eve = 210)
#' @export
obtener_cond_inciden_event <- function(cod_eve) {
  stopifnot("El parametro cod_eve es obligatorio" =
              !missing(cod_eve),
            "El parametro cod_eve debe ser una cadena de caracteres o
            un numerico" =
              (is.numeric(cod_eve) && !is.character(cod_eve)) ||
              (!is.numeric(cod_eve) && is.character(cod_eve)))
  ruta_base <- config::get(file =
                             system.file("extdata",
                                         "config.yml",
                                         package = "sivirep"),
                           "incidence_events_path")
  archivo_condiciones <-  system.file(ruta_base, package = "sivirep")
  incidencia_events <- readxl::read_excel(archivo_condiciones,
                                          col_types = c("numeric", "text",
                                                        "text", "text",
                                                        "text", "text"))
  vals_event <- incidencia_events[incidencia_events$cod_eve ==
                                    as.numeric(cod_eve), ]
  return(vals_event)
}

#' Obtener código de un departamento y municipio
#'
#' Función que obtiene los códigos geográficos de un departamento y municipio
#' dadas unas condiciones
#' @param data_agrupada Un `data.frame` que contiene los datos de la enfermedad
#' agrupados por departamento o municipio y número de casos
#' @param nomb_cols Un `character` (cadena de caracteres) o
#' `array (arreglo) de character` que contiene el nombre de la(s) columna(s) con
#' la información de los departamentos y municipios en los datos agrupados de la
#' enfermedad o evento
#' @param dpto Un `character` (cadena de caracteres) o `numeric` (numérico)
#' que contiene el código o nombre del departamento; su valor por
#' defecto es `NULL`
#' @param mpio Un `character` (cadena de caracteres) o `numeric` (numérico)
#' que contiene el código o nombre del municipio; su valor por defecto es `NULL`
#' @return Una `list` (lista) con el departamento y municipio con la siguiente
#' estructura `list(dpto = "05", mpio = "05001")`
#' @keywords internal
obtener_dpto_mpio <- function(data_agrupada, nomb_cols,
                              dpto = NULL, mpio = NULL) {
  unidades_geo <- NULL
  if (!is.null(dpto) && dpto != "01") {
    dept_data <- obtener_info_depts(dpto, mpio)
    if (nrow(dept_data) == 0) {
      warning("El departamento o municipio ingresado no existe, ",
              "dpto: ", dpto, " , mpio: ", mpio)
    }
    dept_data <- dept_data[1, ]
    dpto <- dept_data$codigo_departamento
    if (!is.null(mpio)) {
      mpio <- dept_data$codigo_municipio
    }
    unidades_geo <- list(dpto = dpto, mpio = mpio)
  } else if (nomb_cols[1] %in% colnames(data_agrupada) &&
      !is.na(unique(
        nomb_cols[data_agrupada[[nomb_cols[1]]]]))) {
    dpto <- data_agrupada[[nomb_cols[1]]][1]
    if (is.null(mpio) &&
        nomb_cols[3] %in% colnames(data_agrupada) &&
        !is.na(unique(
          nomb_cols[data_agrupada[[nomb_cols[3]]]]))) {
      mpio <- data_agrupada[[nomb_cols[3]]][1]
    }
    unidades_geo <- list(dpto = dpto, mpio = mpio)
  }
  return(unidades_geo)
}

#' Obtener la población para efectuar el cálculo de la incidencia
#'
#' Función que obtiene la población a riesgo de un evento o enfermedad
#' o las proyecciones poblacionales DANE desde el año 2005 hasta el 2035.
#' Si no hay población a riesgo disponible del evento o enfermedad para el año
#' seleccionado se obtendrá las proyecciones poblacionales DANE y se mostrarán
#' mensajes de advertencia al usuario dependendiendo del tipo de población
#' obtenida
#' @param data_incidencia Un `data.frame` que contiene la población a riesgo o
#' las proyecciones poblaciones DANE. Si este parámetro está vacío importará
#' la población a riesgo o las proyecciones dependiendo de la disponibilidad de
#' la información; su valor por defecto es `NULL`
#' @param poblacion Un `character` (cadena de caracteres) con el tipo de
#' población que se desea obtener. Indica si se desea obtener la población
#' a riesgo del evento `"riesgo"` o las proyecciones poblacionales DANE
#' `"proyecciones"`
#' @param event Un `character` (cadena de caracteres) o un `numeric` (numerico)
#' con el nombre o código de la enfermedad o evento. Es obligatorio para
#' obtener la población a riesgo
#' @param year Un `numeric` (numerico) con el año deseado de la población a
#' riesgo. Es obligatorio para obtener la población a riesgo
#' @return Un `data.frame` con la población a riesgo o las proyecciones
#' poblacionaldes DANE
#' @keywords internal
obtener_pob_incidencia <- function(data_incidencia = NULL,
                                   poblacion,
                                   event,
                                   year) {
  if (is.null(data_incidencia)) {
    data_incidencia <- import_pob_incidencia(poblacion = poblacion,
                                             event = event,
                                             year = year)
    if (poblacion == "riesgo") {
      if (!is.null(data_incidencia)) {
        message("Las incidencias se calcularan con la poblacion a ",
                "riesgo definida por el Ministerio de Salud para ",
                "el ", year)
      } else {
        poblacion <- "proyecciones"
        data_incidencia <- import_pob_incidencia(poblacion = poblacion)
        warning("Las incidencias se calcularan con las proyecciones ",
                "poblacionales DANE. Si usted cuenta con la ",
                "poblacion a riesgo definida por el Ministerio de ",
                "Salud para el ", year, " puede hacer uso de ella, ",
                "asignandola en el argumento data_incidencia de la funcion")
      }
    } else {
      message("Las incidencias que va a generar idealmente deberian estar ",
              "calculadas por poblacion a riesgo. Para esto, puede usar ",
              "el argumento poblacion = 'riesgo'")
    }
  } else {
    stopifnot("El parametro data_incidencia debe ser un data.frame" =
                is.data.frame(data_incidencia),
              "El parametro data_incidencia no debe estar vacio" =
                nrow(data_incidencia) > 0)
  }
  pop_data_incidencia <- list(data_incidencia = data_incidencia,
                              poblacion = poblacion)
  return(pop_data_incidencia)
}

#' Obtener el año de una enfermedad o evento
#'
#' Función que obtiene el año de los datos de una enfermedad o
#' evento
#' @param data_event Un `data.frame` que contiene los datos de la
#' enfermedad o evento
#' @keywords internal
obtener_year <- function(data_event) {
  year <- NULL
  nomb_col <- "ano"
  if (!is.null(data_event)) {
    if (nomb_col %in% colnames(data_event)) {
      year <- unique(data_event[[nomb_col]])
      stopifnot("Los datos del evento o enfermedad tienen informacion de mas
                de un year, no es posible inferir el year que debe tomar la
                funcion para su ejecucion. Por favor indique el valor en el
                parametro year" =
                  length(year) == 1)
    } else {
      stopifnot("Los datos del evento o enfermedad no contienen la variable
                o columna ano. Por favor indique el valor en el parametro year
                para ejecutar la funcion" =
                  length(year) == 1)
    }
  }
  return(year)
}
