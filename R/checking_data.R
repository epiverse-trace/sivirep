#' Filtrar por departamentos y municipios
#'
#' Función que filtra los datos de una enfermedad o evento por departamentos
#' y municipios
#' @param data_event Un `data.frame` con los datos de una enfermedad o evento
#' @param dpto Un `character` (cadena de caracteres) que contiene
#' el nombre o código del departamento; valor por defecto `NULL`
#' @param mpio Un `character` (cadena de caracteres) que contiene el
#' nombre o código del municipio; su valor por defecto es `NULL`
#' @return Un `data.frame` con los datos filtrados con la enfermedad,
#' departamentos y municipios seleccionados
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' geo_filtro(data_event = data_limpia, dpto = "ANTIOQUIA")
#' geo_filtro(data_event = data_limpia, dpto = "ANTIOQUIA", mpio = "ENVIGADO")
#' @export
geo_filtro <- function(data_event, dpto = NULL, mpio = NULL) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0)
  data_dept_filt <- data.frame()
  dept_data <- data.frame()
  cols_ocurren <- NULL
  if (!is.null(dpto)) {
    dept_data <- obtener_info_depts(dpto, mpio)
    stopifnot("El departamento o municipio ingresado no existe"
              = nrow(dept_data) > 0)
    dept_data <- dept_data[1, ]
    cols_ocurren <- obtener_tip_ocurren_geo(data_event$cod_eve[1])
  }
  if (!is.null(dept_data)) {
    data_dept_filt <-
      dplyr::filter(data_event,
                    data_event[[cols_ocurren[1]]] %in%
                      dept_data$codigo_departamento)
  }
  if (!is.null(mpio)) {
    stopifnot("El parametro mpio debe ser una cadena de caracteres"
              = is.character(mpio))
    tam <- nchar(data_dept_filt[[cols_ocurren[3]]][1])
    cod_mun <- modficar_cod_mun(dept_data$codigo_departamento,
                                dept_data$codigo_municipio,
                                tam)
    data_dept_filt[[cols_ocurren[3]]] <-
      as.character(data_dept_filt[[cols_ocurren[3]]])
    data_dept_filt <-
      dplyr::filter(data_dept_filt,
                    data_dept_filt[[cols_ocurren[3]]] %in%
                      as.character(cod_mun))
  }
  return(data_dept_filt)
}

#' Obtener códigos de departamentos
#'
#' Función que obtiene la lista de departamentos de Colombia con su nombre
#' y código
#' @param geo_cods Un `data.frame` que contiene los códigos geográficos
#' (departamentos y municipios de Colombia)
#' @return Un `data.frame` con los datos de los departamentos con
#' código y nombre
#' @examples
#' geo_cods <- import_geo_cods()
#' obtener_cods_dpto(geo_cods = geo_cods)
#' @export
obtener_cods_dpto <- function(geo_cods) {
  stopifnot("El parametro geo_cods es obligatorio" = !missing(geo_cods),
            "El parametro geo_cods debe ser un data.frame" =
              is.data.frame(geo_cods),
            "El parametro geo_cods no debe estar vacio" =
              nrow(geo_cods) > 0)
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
#' especial de una enfermedad o evento
#' @param data_event Un `data.frame` que contiene los datos de una
#' enfermedad o evento
#' @return Un `data.frame` con los casos por tipo de población especial
#' de una enfermedad o evento
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' obtener_casos_pob_especial(data_event = data_limpia)
#' @export
obtener_casos_pob_especial <- function(data_event) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0)
  pob_especial <- config::get(file = system.file("extdata",
                                                 "config.yml",
                                                 package = "sivirep"),
                              "special_populations_cols")
  pob_especial_noms <- config::get(file =
                                     system.file("extdata",
                                                 "config.yml",
                                                 package = "sivirep"),
                                   "special_populations_names")
  casos_especiales <- NULL
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
#' @param data_event Un `data.frame` que contiene los datos de
#' una enfermedad o evento
#' @return Un `data.frame` con los datos de una enfermedad o
#' evento agrupados por semana epidemiológica y número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_casos_semanaepi(data_event = data_limpia)
#' @export
agrupar_casos_semanaepi <- function(data_event) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0)
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
#' @param data_event Un `data.frame` que contiene los datos de
#' una enfermedad o evento
#' @param nomb_cols Un `character` (cadena de caracteres) o
#' `array (arreglo) de character` que contiene el nombre de
#' la(s) columna(s)
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si
#' es necesario agregar un porcentaje de casos como columna
#' @return Un `data.frame` con los datos de una enfermedad
#' o evento agrupados por el nombre de la(s) columna(s) y el
#' número de casos; su valor por defecto es `TRUE`
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_cols_casos(data_event = data_limpia,
#'                    nomb_cols = "sexo",
#'                    porcentaje = TRUE)
#' agrupar_cols_casos(data_event = data_limpia,
#'                    nomb_cols = c("sexo", "semana"))
#' @export
agrupar_cols_casos <- function(data_event,
                               nomb_cols,
                               porcentaje = FALSE) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro nomb_cols es obligatorio"
            = !missing(nomb_cols),
            "El parametro nomb_cols debe ser una cadena de caracteres 
            o un arreglo de cadenas de caracteres "
            = (is.character(nomb_cols) && !is.array(nomb_cols)) ||
              (!is.character(nomb_cols) && is.array(nomb_cols)),
            "El parametro porcentaje debe ser un booleano (TRUE o FALSE)" =
              is.logical(porcentaje))
  nomb_cols <- append(nomb_cols, "nombre_evento")
  data_event_agrupada <- data_event %>%
    dplyr::group_by_at(nomb_cols) %>%
    dplyr::summarise(casos = dplyr::n(), .groups = "drop")
  if (porcentaje) {
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
#' @param data_event Un `data.frame` que contiene los datos de la
#' enfermedad o evento
#' @param nomb_col Un `character` (cadena de caracteres) con
#' el nombre de la columna de los datos de la enfermedad o evento
#' que contiene las edades
#' @param col_adicional Un `character` (cadena de caracteres) con
#' el nombre adicional de la columna de los datos de la enfermedad
#' o evento para agrupar con la edad; su valor por defecto es
#' `NULL`
#' @param min_val Un `numeric` (numerico) que contiene el valor mínimo
#' de las edades
#' @param max_val Un `numeric` (numerico) que contiene el valor máximo
#' de las edades
#' @param paso Un `numeric` (numerico) que contiene el valor del paso
#' para generar el rango de edades
#' @return Un `data.frame` con los datos de la enfermedad o evento
#' agrupados por el rango de edad y número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' data_edad <- agrupar_cols_casos(data_event = data_limpia,
#'                                 c("edad", "semana"),
#'                                 porcentaje = TRUE)
#' agrupar_rango_edad_casos(data_event = data_edad,
#'                          nomb_col = "edad",
#'                          min_val = 0,
#'                          max_val = max(data_edad$edad),
#'                          paso = 10)
#' @export
agrupar_rango_edad_casos <- function(data_event,
                                     nomb_col,
                                     col_adicional = NULL,
                                     min_val,
                                     max_val,
                                     paso) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0)
  data_vals_rango <- data.frame()
  if (is.null(nomb_col) || length(nomb_col) > 0) {
    nomb_col <- "edad"
  }
  stopifnot("El parametro nomb_col debe ser una cadena de caracteres"
            = is.character(nomb_col))
  data_vals_rango <- data_event %>%
    dplyr::mutate(ranges = cut(
      data_event[[nomb_col]],
      seq(min_val, max_val, paso)
    )) %>%
    dplyr::group_by_at(c("ranges", col_adicional)) %>%
    dplyr::summarize(casos = sum(.data$casos), .groups = "drop") %>%
    as.data.frame()
  names(data_vals_rango)[names(data_vals_rango) == "ranges"] <- nomb_col
  return(data_vals_rango)
}

#' Agrupar por fecha de inicio de síntomas y casos
#'
#' Función que agrupa los datos de una enfermedad o evento por
#' fecha de inicio de síntomas y número de casos
#' @param data_event Un `data.frame` que contiene los datos de
#' la enfermedad o evento
#' @param nomb_col Un `character` (cadena de caracteres) con el
#' nombre de la columna de los datos de la enfermedad o evento que contiene
#' las fechas de inicio de síntomas; su valor por defecto es `"ini_sin"`
#' @return Un `data.frame` con los datos de la enfermedad o evento
#' agrupados por fecha de inicio de síntomas y número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_fecha_inisintomas(data_event = data_limpia,
#'                           nomb_col = "ini_sin")
#' @export
agrupar_fecha_inisintomas <- function(data_event,
                                      nomb_col = "ini_sin") {
  fechas_cols_nombres <- config::get(file =
                                       system.file("extdata",
                                                   "config.yml",
                                                   package = "sivirep"),
                                     "dates_column_names")
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0)
  if (is.null(nomb_col)) {
    nomb_col <- fechas_cols_nombres[3]
  }
  stopifnot("El parametro nomb_col debe ser una cadena de caracteres"
            = is.character(nomb_col))
  nomb_col <- append(nomb_col, "semana")
  group_by_onset_symp <- agrupar_cols_casos(data_event,
                                            nomb_cols = nomb_col)
  return(group_by_onset_symp)
}

#' Agrupar por fecha de notificación y casos
#'
#' Función que agrupa los datos de una enfermedad o evento por fecha de
#' notificación y número de casos
#' @param data_event Un `data.frame` que contiene los datos de la enfermedad
#' o evento
#' @param nomb_col Un `character` (cadena de caracteres) con el nombre de
#' la columna de los datos de la enfermedad o evento que contiene las
#' fechas de notificación; su valor por defecto es `"fec_not"`
#' @return Un `data.frame` con los datos de enfermedades agrupados por fecha de
#' notificación y número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_fecha_notifica(data_event = data_limpia,
#'                        nomb_col = "fec_not")
#' @export
agrupar_fecha_notifica <- function(data_event,
                                   nomb_col = "fec_not") {
  fechas_cols_nombres <- config::get(file =
                                       system.file("extdata",
                                                   "config.yml",
                                                   package = "sivirep"),
                                     "dates_column_names")
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0)
  if (is.null(nomb_col)) {
    nomb_col <- fechas_cols_nombres[2]
  }
  stopifnot("El parametro nomb_col debe ser una cadena de caracteres"
            = is.character(nomb_col))
  nomb_col <- append(nomb_col, "semana")
  data_agrupada_fecha_not <- agrupar_cols_casos(data_event,
                                                nomb_cols = nomb_col)
  return(data_agrupada_fecha_not)
}

#' Agrupar por sexo y casos
#'
#' Función que agrupa los datos de una enfermedad o evento
#' por sexo y número de casos
#' @param data_event Un `data.frame` que contiene los datos de la enfermedad
#' o evento
#' @param nomb_col Un `character` (cadena de caracteres) con el nombre
#' de la columna de los datos de la enfermedad o evento que contiene el sexo;
#' su valor por defecto es `"sexo"`
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si es necesario
#' agregar un porcentaje de casos como una columna; su valor por defecto es
#' `TRUE`
#' @return Un data.frame con los datos de la enfermedad o evento
#' agrupados por sexo y número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_sex(data_event = data_limpia,
#'             nomb_col = "sexo",
#'             porcentaje = TRUE)
#' @export
agrupar_sex <- function(data_event,
                        nomb_col = "sexo",
                        porcentaje = TRUE) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro nomb_col debe ser una cadena de caracteres"
            = is.character(nomb_col))
  data_event_sex <- agrupar_cols_casos(data_event, nomb_col, porcentaje)
  return(data_event_sex)
}

#' Agrupar por sexo, semana epidemiológica y casos
#'
#' Función que agrupa los datos de enfermedades por sexo, semana
#' epidemiológica y número de casos
#' @param data_event Un `data.frame` que contiene los datos de
#' la enfermedad o evento
#' @param nomb_cols Un `character` (cadena de caracteres) o
#' `array (arreglo) de character` que contiene el nombre de
#' la(s) columna(s) de los datos de la enfermedad o evento
#' que contienen el sexo y las semanas epidemiológicas; su valor
#' por defecto es `c("sexo", "semana")`
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si
#' es necesario agregar un porcentaje de casos como una columna; su
#' valor por defecto es `TRUE`
#' @return Un `data.frame` con los datos de la enfermedad o evento
#' agrupados por sexo, semana epidemiológica y número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_sex_semanaepi(data_event = data_limpia,
#'                       nomb_cols = c("sexo", "semana"),
#'                       porcentaje = TRUE)
#' @export
agrupar_sex_semanaepi <- function(data_event,
                                  nomb_cols = c("sexo", "semana"),
                                  porcentaje = TRUE) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro nomb_cols debe ser una cadena de caracteres 
            o un arreglo de cadenas de caracteres "
            = (is.character(nomb_cols) && !is.array(nomb_cols)) ||
              (!is.character(nomb_cols) && is.array(nomb_cols)))
  data_event_sex_semanaepi <- agrupar_cols_casos(data_event,
                                                 nomb_cols,
                                                 porcentaje)
  return(data_event_sex_semanaepi)
}

#' Agrupar por edad y casos
#'
#' Función que agrupa los datos de una enfermedad o evento por edad
#' y número de casos
#' @param data_event Un `data.frame` que contiene los datos de la enfermedad
#' o evento
#' @param nomb_col Un `character` (cadena de caracteres) con el nombre
#' de la columna de los datos de la enfermedad o evento que contiene las edades;
#' su valor por defecto es `"edad"`
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si
#' es necesario agregar un porcentaje de casos como una columna; su valor por
#' defecto es `FALSE`
#' @param interval_edad Un `numeric` (numerico) que contiene el intervalo del
#' rango de edades; su valor por defecto es `10`
#' @return Un `data.frame` con los datos de la enfermedad o evento agrupados
#' por edad y número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_edad(data_event = data_limpia,
#'              nomb_col = "edad",
#'              porcentaje = FALSE)
#' @export
agrupar_edad <- function(data_event,
                         nomb_col = "edad",
                         porcentaje = FALSE,
                         interval_edad = 10) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro nomb_col debe ser una cadena de caracteres"
            = is.character(nomb_col),
            "El parametro interval_edad debe ser un numero"
            = is.numeric(interval_edad))
  data_event_edad <- agrupar_cols_casos(data_event,
                                        nomb_col,
                                        porcentaje)
  data_event_edad <-
    agrupar_rango_edad_casos(data_event_edad,
                             nomb_col,
                             min_val = 0,
                             max_val =
                             max(data_event_edad[[nomb_col]]),
                             paso = interval_edad)
  return(data_event_edad)
}

#' Agrupar por edades, sexo y casos
#'
#' Función que agrupa los datos de una enfermedad o evento por edades,
#' sexo y número de casos
#' @param data_event Un `data.frame` que contiene los datos de la enfermedad
#' o evento
#' @param nomb_cols Un `character` (cadena de caracteres) o
#' `array (arreglo) de character` que contiene el nombre de
#' la(s) columna(s) de los datos de la enfermedad o evento que contienen
#' las edades y el sexo; su valor por defecto es `c("edad", "sexo")`
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si
#' es necesario agregar un porcentaje de casos como una columna; su valor
#' por defecto es `TRUE`
#' @param interval_edad Un `numeric` (numerico) que contiene el intervalo del
#' rango de edades; su valor por defeccto es `10`
#' @return Un `data.frame` con los datos de enfermedades agrupados
#' por edades, sexo y número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_edad_sex(data_event = data_limpia,
#'                  nomb_cols = c("edad", "sexo"),
#'                  porcentaje = TRUE)
#' @export
agrupar_edad_sex <- function(data_event,
                             nomb_cols = c("edad", "sexo"),
                             porcentaje = TRUE,
                             interval_edad = 10) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro nomb_cols debe ser una cadena de caracteres
            o un arreglo de cadenas de caracteres"
            = (is.character(nomb_cols) || is.array(nomb_cols)),
            "El parametro porcentaje debe ser un booleano (TRUE o FALSE)" =
              is.logical(porcentaje))
  data_event_edad_sex <- agrupar_cols_casos(data_event,
                                            nomb_cols,
                                            porcentaje)
  data_event_edad_sex <- agrupar_rango_edad_casos(
    data_event_edad_sex,
    nomb_cols[1],
    nomb_cols[2],
    min_val = 0,
    max_val =
      max(data_event_edad_sex[[nomb_cols[1]]]),
    paso = interval_edad
  )
  return(data_event_edad_sex)
}

#' Agrupar por población especial y casos
#'
#' Función que agrupa los datos de la enfermedad o evento por población
#' especial y casos
#' @param data_event Un `data.frame` que contiene los datos de la enfermedad
#' o evento
#' @param nomb_col Un `character` (cadena de caracteres) con el nombre de la
#' columna de los datos de la enfermedad o evento que contiene las poblaciones
#' especiales; su valor por defecto es `"poblacion"`
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si
#' es necesario agregar un porcentaje de casos como una columna; su valor
#' por defecto es `TRUE`
#' @return Un `data.frame` con los datos de la enfermedad o evento agrupados
#' por poblaciones especiales y casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_pob_especial(data_event = data_limpia,
#'                      nomb_col = "poblacion",
#'                      porcentaje = TRUE)
#' @export
agrupar_pob_especial <- function(data_event,
                                 nomb_col = "poblacion",
                                 porcentaje = TRUE) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro nomb_col debe ser una cadena de caracteres"
            = is.character(nomb_col),
            "El parametro porcentaje debe ser un booleano (TRUE o FALSE)" =
              is.logical(porcentaje))
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
#' @param data_event Un `data.frame` que contiene los datos de la
#' enfermedad o evento
#' @param nomb_col Un `character` (cadena de caracteres) con el nombre
#' de la columna en los datos de la enfermedad o evento que contiene los
#' códigos de departamento; su valor por defecto es `"cod_dpto_o"`
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si
#' es necesario agregar un porcentaje de casos como una columna; su valor
#' por defecto es `FALSE`
#' @return Un `data.frame` con los datos de la enfermedad o evento agrupados
#' por códigos de departamento y número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_dpto(data_event = data_limpia,
#'              nomb_col = "cod_dpto_o",
#'              porcentaje = FALSE)
#' @export
agrupar_dpto <- function(data_event,
                         nomb_col = "cod_dpto_o",
                         porcentaje = FALSE) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro nomb_col debe ser una cadena de caracteres"
            = is.character(nomb_col),
            "El parametro porcentaje debe ser un booleano (TRUE o FALSE)" =
              is.logical(porcentaje))
  data_event_cods_dpto <- data_event
  nomb_col <- obtener_tip_ocurren_geo(data_event_cods_dpto$cod_eve[1])
  data_event_cods_dpto <- agrupar_cols_casos(data_event_cods_dpto,
                                             nomb_cols = nomb_col[1:2])
  data_event_cods_dpto[[nomb_col[1]]] <-
    as.character(data_event_cods_dpto[[nomb_col[1]]])
  return(data_event_cods_dpto)
}


#' Agrupar por municipios y casos
#'
#' Función que agrupa los datos de una enfermedad o evento por código
#' de municipios y número de casos
#' @param data_event Un `data.frame` que contiene los datos de la
#' enfermedad o evento
#' @param dpto Un `character` (cadena de caracteres) que contiene
#' el nombre del departamento; su valor por defecto es `NULL`
#' @param nomb_col Un `character` (cadena de caracteres) con el nombre de
#' la columna en los datos de la enfermedad o evento que contiene los códigos
#' de municipios; su valor por defecto es `"cod_mun_o"`
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si es necesario
#' agregar un porcentaje de casos como una columna; su valor por
#' defecto es `FALSE`
#' @return Un `data.frame` con los datos de la enfermedad o evento agrupados
#' por códigos de municipios y número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_mpio(data_event = data_limpia,
#'              dpto = "Antioquia",
#'              nomb_col = "cod_mun_o",
#'              porcentaje = FALSE)
#' @export
agrupar_mpio <- function(data_event,
                         dpto = NULL,
                         nomb_col = "cod_mun_o",
                         porcentaje = FALSE) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro nomb_col debe ser una cadena de caracteres"
            = is.character(nomb_col),
            "El parametro porcentaje debe ser un booleano (TRUE o FALSE)" =
              is.logical(porcentaje))
  cols_geo_ocurrencia <- data.frame()
  cod_events <- unique(data_event$cod_eve)
  for (cod in cod_events) {
    cols_geo_ocurrencia <- append(cols_geo_ocurrencia,
                                  obtener_tip_ocurren_geo(cod))
  }
  nomb_col <- obtener_tip_ocurren_geo(data_event$cod_eve[1])
  data_event_muns <- data_event
  dept_data <- NULL
  if (!is.null(dpto)) {
    aux_dpto <- unique(data_event_muns[[nomb_col[2]]])
    if (length(aux_dpto) > 1) {
      data_event_muns <- geo_filtro(data_event, dpto)
    }
  } else {
    dpto <- unique(data_event_muns[[nomb_col[2]]])
    if (length(dpto) != 1) {
      stopifnot("Debe ingresar el nombre o codigo del departamento" =
                length(dpto) == 1)
    }
  }
  dept_data <- obtener_info_depts(dpto)
  data_event_muns <- agrupar_cols_casos(data_event_muns,
                                        nomb_cols = nomb_col[1:4])
  data_event_muns[[nomb_col[1]]] <- as.character(data_event_muns[[nomb_col[1]]])
  data_event_muns[[nomb_col[3]]] <- as.character(data_event_muns[[nomb_col[3]]])
  dept_data <- dept_data[1, ]
  data_event_muns <-  dplyr::arrange(data_event_muns,
                                     dplyr::desc(.data$casos))
  return(data_event_muns)
}
