#' Filtrar por enfermedad o evento
#'
#' Función que filtra por nombre de enfermedad o evento
#' en un conjunto de datos
#' @param nombre_event Un character (cadena de caracteres) que contiene
#' el nombre de la enfermedad o evento
#' @param data_sivigila Un data frame que contiene el conjunto de datos
#' del SIVIGILA
#' @return Un data frame con los datos filtrados por la enfermedad o
#' evento seleccionado
#' @examples
#' data_sivigila <- import_data_resumen_sivigila()
#' data_sivigila <- limpiar_encabezado(data_sivigila)
#' filtrar_event(nombre_event = "MALAR",
#'               data_sivigila = data_sivigila)
#' @export
filtrar_event <- function(nombre_event,
                          data_sivigila) {
  if ("conteo_casos" %in% names(data_sivigila)) {
    names(data_sivigila)[names(data_sivigila)
                         == "conteo_casos"] <- "casos"
  }
  list_events <- unique(data_sivigila$nombre)
  list_specific <- list_events[stringr::str_detect(list_events,
                                                   nombre_event) == TRUE]
  data_fil <- data_sivigila %>%
    dplyr::filter(.data$nombre %in% list_specific)
  return(data_fil)
}

#' Filtrar por departamentos y municipios
#'
#' Función que filtra los datos de una enfermedad o evento por departamentos
#' y municipios
#' @param data_event Un data frame con los datos de una enfermedad o evento
#' @param nombre_dpto Un character (cadena de caracteres) que contiene
#' el nombre del departamento; valor por defecto NULL
#' @param nombre_mun Un character (cadena de caracteres) que contiene el
#' nombre del municipio; su valor por defecto es NULL
#' @return Un data frame con los datos filtrados con la enfermedad,
#' departamentos y municipios seleccionados
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' geo_filtro(data_event, nombre_dpto = "ANTIOQUIA")
#' @export
geo_filtro <- function(data_event, nombre_dpto = NULL, nombre_mun = NULL) {
  data_dept_filt <- data.frame()
  dept_data <- data.frame()
  cols_ocurren <- c()
  if (!is.null(nombre_dpto)) {
    dept_data <- obtener_info_depts(nombre_dpto, nombre_mun)
    dept_data <- dept_data[1, ]
    cols_ocurren <- obtener_tip_ocurren_geo(data_event$cod_eve[1])
  }
  if (!is.null(dept_data)) {
    data_dept_filt <-
      dplyr::filter(data_event,
                    data_event[[cols_ocurren[1]]] %in%
                      dept_data$codigo_departamento)
  }
  if (!is.null(nombre_mun)) {
    code_mun <- modficar_cod_mun(dept_data$codigo_departamento,
                                 dept_data$codigo_municipio)
    data_dept_filt <-
      dplyr::filter(data_dept_filt,
                    data_dept_filt[[cols_ocurren[2]]] %in%
                      as.integer(code_mun))
  }
  return(data_dept_filt)
}

#' Obtener códigos de departamentos
#'
#' Función que obtiene la lista de departamentos de Colombia con su nombre
#' y código
#' @param geo_cods Un data frame que contiene los códigos geográficos
#' (departamentos y municipios de Colombia)
#' @return Un data frame con los datos de los departamentos con
#' código y nombre
#' @examples
#' geo_cods <- import_geo_cods()
#' obtener_cods_dpto(geo_cods = geo_cods)
#' @export
obtener_cods_dpto <- function(geo_cods) {
  data_deptos <- geo_cods %>%
    dplyr::group_by(cod_dep = .data$codigo_departamento,
                    name_dep = .data$nombre_departamento) %>%
    dplyr::select(.data$cod_dep, .data$name_dep) %>%
    dplyr::distinct()
  data_deptos <- data_deptos[1:33, ]
  return(data_deptos)
}

#' Obtener población especial y casos
#'
#' Función que obtiene los casos por tipo de población
#' especial de una enfermedad
#' @param data_event Un data frame que contiene los datos de una
#' enfermedad o evento
#' @return Un data frame con los casos por tipo de población especial
#' de una enfermedad o evento
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' obtener_casos_pob_especial(data_event = data_event)
#' @export
obtener_casos_pob_especial <- function(data_event) {
  pob_especial <- config::get(file = system.file("extdata",
                                                 "config.yml",
                                                 package = "sivirep"),
                              "special_populations_cols")
  pob_especial_noms <- config::get(file =
                                     system.file("extdata",
                                                 "config.yml",
                                                 package = "sivirep"),
                                   "special_populations_names")
  casos_especiales <- c()
  for (sp in pob_especial) {
    data_event[[sp]] <- as.numeric(data_event[[sp]])
    casos_especiales <- append(casos_especiales,
                               sum(data_event[[sp]]))
  }
  data_pob_especial <- data.frame(
    poblacion = pob_especial,
    casos = casos_especiales,
    nombre = pob_especial_noms
  )
  return(data_pob_especial)
}

#' Agrupar por semana y casos
#'
#' Función que agrupa los datos de una enfermedad o evento
#' por semana epidemiológica y número de casos
#' @param data_event Un data frame que contiene los datos de
#' una enfermedad o evento
#' @return Un data frame con los datos de una enfermedad o
#' evento agrupados por semana epidemiológica y número de casos
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' agrupar_casos_semanaepi(data_event = data_event)
#' @export
agrupar_casos_semanaepi <- function(data_event) {
  data_event_agrupada <- data_event %>%
    dplyr::group_by(.data$semana) %>%
    dplyr::summarise(casos = sum(.data$uni_med))
  data_event_agrupada <- data_event_agrupada[1:52, ]
  return(data_event_agrupada)
}

#' Agrupar por columnas y casos
#'
#' Función que agrupa los datos de una enfermedad o evento
#' por nombre de columna(s) y número de casos
#' @param data_event Un data frame que contiene los datos de
#' una enfermedad o evento
#' @param cols_nombres Un character (cadena de caracteres) o
#' array (arreglo) de character que contiene el nombre de
#' la(s) columna(s)
#' @param agr_porcentaje Un boolean (TRUE o FALSE) que indica si
#' es necesario agregar un porcentaje de casos como columna
#' @return Un data frame con los datos de una enfermedad
#' o evento agrupados por el nombre de la(s) columna(s) y el
#' número de casos; su valor por defecto es TRUE
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' agrupar_cols_casos(data_event = data_event,
#'                    cols_nombres = "sexo",
#'                    agr_porcentaje = TRUE)
#' agrupar_cols_casos(data_event = data_event,
#'                    cols_nombres = c("sexo", "semana"))
#' @export
agrupar_cols_casos <- function(data_event,
                               cols_nombres,
                               agr_porcentaje = FALSE) {
  cols_nombres <- append(cols_nombres, c("nombre_evento"))
  print(cols_nombres)
  data_event_agrupada <- data_event %>%
    dplyr::group_by_at(cols_nombres) %>%
    dplyr::summarise(casos = dplyr::n(), .groups = "drop")
  if (agr_porcentaje) {
    data_event_agrupada <-
      data_event_agrupada %>%
      dplyr::mutate(porcentaje =
                    round(data_event_agrupada$casos
                          / sum(data_event_agrupada$casos) * 100,
                          1))
  }
  return(data_event_agrupada)
}

#' Agrupar por rango de edad y casos
#'
#' Función que agrupa los datos de una enfermedad o evento por rango
#' de edad y número de casos
#' @param data_event Un data frame que contiene los datos de la
#' enfermedad o evento
#' @param col_nombre Un character (cadena de caracteres) con
#' el nombre de la columna de los datos de la enfermedad o evento
#' que contiene las edades
#' @param var_a Un character (cadena de caracteres) con
#' el nombre adicional de la columna de los datos de la enfermedad
#' o evento para agrupar con la edad; su valor por defecto es
#' NULL
#' @param min_val Un numeric (numerico) que contiene el valor mínimo
#' de las edades
#' @param max_val Un numeric (numerico) que contiene el valor máximo
#' de las edades
#' @param paso Un numeric (numerico) que contiene el valor del paso
#' para generar el rango de edades
#' @return Un data frame con los datos de la enfermedad o evento
#' agrupados por el rango de edad y número de casos
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' data_edad <- agrupar_cols_casos(data_event = data_event,
#'                                 c("edad", "semana"),
#'                                 agr_porcentaje = TRUE)
#' agrupar_rango_edad_casos(data_event = data_edad,
#'                          col_nombre = "edad",
#'                          min_val = 0,
#'                          max_val = max(data_edad$edad),
#'                          paso = 10)
#' @export
agrupar_rango_edad_casos <- function(data_event,
                                     col_nombre,
                                     var_a = NULL,
                                     min_val,
                                     max_val,
                                     paso) {
  data_vals_rango <- data.frame()
  if (!is.null(var_a) && length(var_a) > 0) {
    data_vals_rango <- data_event %>%
      dplyr::mutate(ranges = cut(
        .data$edad,
        seq(min_val, max_val, paso)
      )) %>%
      dplyr::group_by_at(c("ranges", var_a)) %>%
      dplyr::summarize(casos = sum(.data$casos), .groups = "drop") %>%
      as.data.frame()
    names(data_vals_rango)[names(data_vals_rango) == "ranges"] <- col_nombre
  } else {
    data_vals_rango <- data_event %>%
      dplyr::mutate(ranges = cut(
        .data$edad,
        seq(min_val, max_val, paso)
      )) %>%
      dplyr::group_by_at("ranges") %>%
      dplyr::summarize(casos = sum(.data$casos), .groups = "drop") %>%
      as.data.frame()
    names(data_vals_rango)[names(data_vals_rango) == "ranges"] <- col_nombre
  }
  return(data_vals_rango)
}

#' Agrupar por columnas y casos
#'
#' Función que agrupa los datos de una enfermedad o evento
#' por un nombre de columna(s) específico y número de casos
#' @param data_event Un data frame que contiene los datos
#' de la enfermedad o evento
#' @param cols_nombres Un character (cadena de caracteres) o
#' array (arreglo) de character que contiene el nombre de
#' la(s) columna(s) por la(s) que se desea agrupar los datos
#' @param agr_porcentaje Un boolean (TRUE o FALSE) que indica si
#' es necesario agregar un porcentaje de casos como una columna;
#' su valor por defecto es FALSE
#' @return Un data frame con los datos de una enfermedad
#' o evento agrupados por nombre de columna(s) y número de casos
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' agrupar_cols_casos(data_event = data_event,
#'                    cols_nombres = "sexo",
#'                    agr_porcentaje = TRUE)
#' agrupar_cols_casos(data_event = data_event,
#'                    cols_nombres = c("sexo", "semana"))
#' @export
agrupar_cols_casos <- function(data_event,
                               cols_nombres,
                               agr_porcentaje = FALSE) {
  cols_nombres <- append(cols_nombres, c("nombre_evento"))
  data_event_agrupada <- data_event %>%
    dplyr::group_by_at(cols_nombres) %>%
    dplyr::summarise(casos = dplyr::n(), .groups = "drop")
  if (agr_porcentaje) {
    data_event_agrupada <-
      data_event_agrupada %>%
      dplyr::mutate(porcentaje =
                    round(data_event_agrupada$casos
                          / sum(data_event_agrupada$casos) * 100,
                          1))
  }
  return(data_event_agrupada)
}

#' Agrupar por fecha de inicio de síntomas y casos
#'
#' Función que agrupa los datos de una enfermedad o evento por
#' fecha de inicio de síntomas y número de casos
#' @param data_event Un data frame que contiene los datos de
#' la enfermedad o evento
#' @param col_nombre Un character (cadena de caracteres) con el
#' nombre de la columna de los datos de la enfermedad o evento que contiene
#' las fechas de inicio de síntomas; su valor por defecto es ini_sin
#' @return Un data frame con los datos de la enfermedad o evento
#' agrupados por fecha de inicio de síntomas y número de casos
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' agrupar_fecha_inisintomas(data_event = data_event,
#'                           col_nombre = "ini_sin")
#' @export
agrupar_fecha_inisintomas <- function(data_event,
                                      col_nombre = "ini_sin") {
  fechas_cols_nombres <- config::get(file =
                                       system.file("extdata",
                                                   "config.yml",
                                                   package = "sivirep"),
                                     "dates_column_names")
  if (is.null(col_nombre)) {
    col_nombre <- fechas_cols_nombres[3]
  }
  col_nombre <- append(col_nombre, "semana")
  group_by_onset_symp <- agrupar_cols_casos(data_event,
                                            cols_nombres = col_nombre)
  return(group_by_onset_symp)
}

#' Agrupar por fecha de notificación y casos
#'
#' Función que agrupa los datos de una enfermedad o evento por fecha de
#' notificación y número de casos
#' @param data_event Un data frame que contiene los datos de la enfermedad
#' o evento
#' @param col_nombre Un character (cadena de caracteres) con el nombre de
#' la columna de los datos de la enfermedad o evento que contiene las
#' fechas de notificación; su valor por defecto es fec_not
#' @return Un data frame con los datos de enfermedades agrupados por fecha de
#' notificación y número de casos
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' agrupar_fecha_notifica(data_event = data_event,
#'                        col_nombre = "fec_not")
#' @export
agrupar_fecha_notifica <- function(data_event,
                                   col_nombre = "fec_not") {
  fechas_cols_nombres <- config::get(file =
                                       system.file("extdata",
                                                   "config.yml",
                                                   package = "sivirep"),
                                     "dates_column_names")
  if (is.null(col_nombre)) {
    col_nombre <- fechas_cols_nombres[2]
  }
  col_nombre <- append(col_nombre, "semana")
  data_agrupada_fecha_not <- agrupar_cols_casos(data_event,
                                                cols_nombres = col_nombre)
  return(data_agrupada_fecha_not)
}

#' Agrupar por sexo y casos
#'
#' Función que agrupa los datos de una enfermedad o evento
#' por sexo y número de casos
#' @param data_event Un data frame que contiene los datos de la enfermedad
#' o evento
#' @param col_nombre Un character (cadena de caracteres) con el nombre
#' de la columna de los datos de la enfermedad o evento que contiene el sexo;
#' su valor por defecto es sexo
#' @param porcentaje Un boolean (TRUE o FALSE) que indica si es necesario
#' agregar un porcentaje de casos como una columna; su valor por defecto es
#' TRUE
#' @return Un data frame con los datos de la enfermedad o evento
#' agrupados por sexo y número de casos
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' agrupar_sex(data_event = data_event,
#'             col_nombre = "sexo",
#'             porcentaje = TRUE)
#' @export
agrupar_sex <- function(data_event,
                        col_nombre = "sexo",
                        porcentaje = TRUE) {
  data_event_sex <- agrupar_cols_casos(data_event, col_nombre, porcentaje)
  return(data_event_sex)
}

#' Agrupar por sexo, semana epidemiológica y casos
#'
#' Función que agrupa los datos de enfermedades por sexo, semana
#' epidemiológica y número de casos
#' @param data_event Un data frame que contiene los datos de
#' la enfermedad o evento
#' @param col_nombres Un character (cadena de caracteres) o
#' array (arreglo) de character que contiene el nombre de
#' la(s) columna(s) de los datos de la enfermedad o evento
#' que contienen el sexo y las semanas epidemiológicas; su valor
#' por defecto es `c("sexo", "semana")`
#' @param porcentaje Un boolean (TRUE o FALSE) que indica si
#' es necesario agregar un porcentaje de casos como una columna; su
#' valor por defecto es `TRUE`
#' @return Un data frame con los datos de la enfermedad o evento
#' agrupados por sexo, semana epidemiológica y número de casos
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' agrupar_sex_semanaepi(data_event = data_event,
#'                       col_nombres = c("sexo", "semana"),
#'                       porcentaje = TRUE)
#' @export
agrupar_sex_semanaepi <- function(data_event,
                                  col_nombres = c("sexo", "semana"),
                                  porcentaje = TRUE) {
  data_event_sex_semanaepi <- agrupar_cols_casos(data_event,
                                                 col_nombres,
                                                 porcentaje)
  return(data_event_sex_semanaepi)
}

#' Agrupar por edad y casos
#'
#' Función que agrupa los datos de una enfermedad o evento por edad
#' y número de casos
#' @param data_event Un data frame que contiene los datos de la enfermedad
#' o evento
#' @param col_nombre Un character (cadena de caracteres) con el nombre
#' de la columna de los datos de la enfermedad o evento que contiene las edades;
#' su valor por defecto es edad
#' @param porcentaje Un boolean (TRUE o FALSE) que indica si
#' es necesario agregar un porcentaje de casos como una columna; su valor por
#' defecto es `FALSE`
#' @param interval_edad Un numeric (numerico) que contiene el intervalo del
#' rango de edades; su valor por defecto es 10
#' @return Un data frame con los datos de la enfermedad o evento agrupados
#' por edad y número de casos
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' agrupar_edad(data_event = data_event,
#'              col_nombre = "edad",
#'              porcentaje = FALSE)
#' @export
agrupar_edad <- function(data_event,
                         col_nombre = "edad",
                         porcentaje = FALSE,
                         interval_edad = 10) {
  data_event_edad <- agrupar_cols_casos(data_event,
                                        col_nombre,
                                        porcentaje)
  data_event_edad <-
    agrupar_rango_edad_casos(data_event_edad,
                             col_nombre,
                             min_val = 0,
                             max_val =
                             max(data_event_edad[[col_nombre]]),
                             paso = interval_edad)
  return(data_event_edad)
}

#' Agrupar por edades, sexo y casos
#'
#' Función que agrupa los datos de una enfermedad o evento por edades,
#' sexo y número de casos
#' @param data_event Un data frame que contiene los datos de la enfermedad
#' o evento
#' @param col_nombres Un character (cadena de caracteres) o
#' array (arreglo) de character que contiene el nombre de
#' la(s) columna(s) de los datos de la enfermedad o evento que contienen
#' las edades y el sexo; su valor por defecto es c("edad", "sexo")
#' @param porcentaje Un boolean (TRUE o FALSE) que indica si
#' es necesario agregar un porcentaje de casos como una columna; su valor
#' por defecto es `TRUE`
#' @param interval_edad Un numeric (numerico) que contiene el intervalo del
#' rango de edades; su valor por defeccto es `10`
#' @return Un data frame con los datos de enfermedades agrupados
#' por edades, sexo y número de casos
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' agrupar_edad_sex(data_event = data_event,
#'                  col_nombres = c("edad", "sexo"),
#'                  porcentaje = TRUE)
#' @export
agrupar_edad_sex <- function(data_event,
                             col_nombres = c("edad", "sexo"),
                             porcentaje = TRUE,
                             interval_edad = 10) {
  data_event_edad_sex <- agrupar_cols_casos(data_event,
                                            col_nombres,
                                            porcentaje)
  data_event_edad_sex <- agrupar_rango_edad_casos(
    data_event_edad_sex,
    col_nombres[1],
    col_nombres[2],
    min_val = 0,
    max_val =
      max(data_event_edad_sex[[col_nombres[1]]]),
    paso = interval_edad
  )
  return(data_event_edad_sex)
}

#' Agrupar por población especial y casos
#'
#' Función que agrupa los datos de la enfermedad o evento por población
#' especial y casos
#' @param data_event Un data frame que contiene los datos de la enfermedad
#' o evento
#' @param col_nombre Un character (cadena de caracteres) con el nombre de la
#' columna de los datos de la enfermedad o evento que contiene las poblaciones
#' especiales; su valor por defecto es poblacion
#' @param porcentaje Un boolean (TRUE o FALSE) que indica si
#' es necesario agregar un porcentaje de casos como una columna; su valor
#' por defecto es `TRUE`
#' @return Un data frame con los datos de la enfermedad o evento agrupados
#' por poblaciones especiales y casos
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' agrupar_pob_especial(data_event = data_event,
#'                      col_nombre = "poblacion",
#'                      porcentaje = TRUE)
#' @export
agrupar_pob_especial <- function(data_event,
                                 col_nombre = "poblacion",
                                 porcentaje = TRUE) {
  data_event_especial <- obtener_casos_pob_especial(data_event)
  data_event_especial_agrupada <- data.frame(poblacion =
                                               data_event_especial$poblacion,
                                             casos =
                                               data_event_especial$casos)
  return(data_event_especial_agrupada)
}

#' Agrupar por departamento y casos
#'
#' Función que agrupa los datos por códigos de departamento y
#' número de casos
#' @param data_event Un data frame que contiene los datos de la
#' enfermedad o evento
#' @param col_nombre Un character (cadena de caracteres) con el nombre
#' de la columna en los datos de la enfermedad o evento que contiene los
#' códigos de departamento; su valor por defecto es cod_dpto_o
#' @param porcentaje Un boolean (TRUE o FALSE) que indica si
#' es necesario agregar un porcentaje de casos como una columna; su valor
#' por defecto es FALSE
#' @return Un data frame con los datos de la enfermedad o evento agrupados
#' por códigos de departamento y número de casos
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' agrupar_dpto(data_event = data_event,
#'              col_nombre = "cod_dpto_o",
#'              porcentaje = FALSE)
#' @export
agrupar_dpto <- function(data_event,
                         col_nombre = "cod_dpto_o",
                         porcentaje = FALSE) {
  data_event_cods_dpto <- data_event
  col_nombre <- obtener_tip_ocurren_geo(data_event_cods_dpto$cod_eve[1])
  data_event_cods_dpto <- agrupar_cols_casos(data_event_cods_dpto,
                                             cols_nombres = col_nombre[1])
  colnames(data_event_cods_dpto)[colnames(data_event_cods_dpto) ==
                                   col_nombre[1]] <- "id"
  data_event_cods_dpto$id <- sapply(data_event_cods_dpto$id,
                                    as.character)
  return(data_event_cods_dpto)
}


#' Agrupar por municipios y casos
#'
#' Función que agrupa los datos de una enfermedad o evento por código
#' de municipios y número de casos
#' @param data_event Un data frame que contiene los datos de la
#' enfermedad o evento
#' @param dept_nombre Un character (cadena de caracteres) que contiene
#' el nombre del departamento; su valor por defecto es NULL
#' @param col_nombre Un character (cadena de caracteres) con el nombre de
#' la columna en los datos de la enfermedad o evento que contiene los códigos
#' de municipios; su valor por defecto es cod_mun_o
#' @param porcentaje Un boolean (TRUE o FALSE) que indica si es necesario
#' agregar un porcentaje de casos como una columna; su valor por
#' defecto es FALSE
#' @return Un data frame con los datos de la enfermedad o evento agrupados
#' por códigos de municipios y número de casos
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' agrupar_mun(data_event = data_event,
#'             dept_nombre = "Antioquia",
#'             col_nombre = "cod_mun_o",
#'             porcentaje = FALSE)
#' @export
agrupar_mun <- function(data_event,
                        dept_nombre = NULL,
                        col_nombre = "cod_mun_o",
                        porcentaje = FALSE) {
  cols_geo_ocurrencia <- data.frame()
  cod_events <- unique(data_event$cod_eve)
  for (cod in cod_events) {
    cols_geo_ocurrencia <- append(cols_geo_ocurrencia,
                                  obtener_tip_ocurren_geo(cod))
  }
  col_nombre <- obtener_tip_ocurren_geo(data_event$cod_eve[1])
  data_event_muns <- data_event
  data_event_muns <- agrupar_cols_casos(data_event_muns,
                                        cols_nombres = col_nombre[2])
  colnames(data_event_muns)[colnames(data_event_muns) ==
                              col_nombre[2]] <- "id"
  data_event_muns$id <- sapply(data_event_muns$id,
                               as.character)
  dept_data <- obtener_info_depts(dept_nombre)
  dept_data <- dept_data[1, ]
  nombres_muns <- c()
  geo_data <- import_geo_cods()
  for (id in data_event_muns$id) {
    nombres_muns <- append(nombres_muns,
                           obtener_nombres_muns(geo_data,
                                                dept_data$codigo_departamento,
                                                id))
  }
  data_event_muns$nombre <- nombres_muns
  data_event_muns <-  dplyr::arrange(data_event_muns,
                                     dplyr::desc(.data$casos))
  return(data_event_muns)
}
