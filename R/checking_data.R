#' Filtrar por departamentos y municipios
#'
#' Función que filtra los datos de una enfermedad o evento por departamentos
#' y municipios
#' @param data_event Un `data.frame` con los datos de una enfermedad o evento
#' @param dpto Un `character` (cadena de caracteres) o `numeric` (numerico)
#' que contiene el nombre o código del departamento; valor por defecto `NULL`
#' @param mpio Un `character` (cadena de caracteres) o `numeric` (numerico)
#' que contiene el nombre o código del municipio; su valor por defecto
#' es `NULL`
#' @return Un `data.frame` con los datos filtrados con la enfermedad,
#' departamentos y municipios seleccionados
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' geo_filtro(data_event = data_limpia, dpto = "ANTIOQUIA")
#' geo_filtro(data_event = data_limpia, dpto = "ANTIOQUIA", mpio = "MEDELLIN")
#' geo_filtro(data_event = data_limpia, dpto = "05")
#' geo_filtro(data_event = data_limpia, dpto = "05", mpio = "05001")
#' geo_filtro(data_event = data_limpia, dpto = 05, mpio = 05001)
#' geo_filtro(data_event = data_limpia, dpto = 05, mpio = 001)
#' geo_filtro(data_event = data_limpia, dpto = "bogota dc", mpio = "bogota dc")
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
    stopifnot("El parametro mpio debe ser una cadena de caracteres o un
              numero"
              = is.character(mpio) | is.numeric(mpio))
    data_dept_filt[[cols_ocurren[3]]] <-
      as.character(data_dept_filt[[cols_ocurren[3]]])
    data_dept_filt <-
      dplyr::filter(data_dept_filt,
                    data_dept_filt[[cols_ocurren[3]]] %in%
                      as.character(dept_data$codigo_municipio))
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

#' Agrupar por semana epidemiológica y casos
#'
#' Función que agrupa los datos de una enfermedad o evento
#' por semana epidemiológica y número de casos
#' @param data_event Un `data.frame` que contiene los datos de
#' una enfermedad o evento
#' @param col_semanaepi Un `character` (cadena de caracteres)
#' con el nombre de la columna que contiene las semanas
#' epidemiológicas en los datos de la enfermedad o evento;
#' su valor por defecto es `"semana"`
#' @return Un `data.frame` con los datos de una enfermedad o
#' evento agrupados por semana epidemiológica y número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_semanaepi(data_event = data_limpia,
#'                   col_semanaepi = "semana")
#' @export
agrupar_semanaepi <- function(data_event,
                              col_semanaepi = "semana") {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0)
  data_event_agrupada <- data_event %>%
    dplyr::group_by_at(col_semanaepi) %>%
    dplyr::summarise(casos = sum(.data$uni_med))
  data_event_agrupada <- data_event_agrupada[1:53, ]
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
#' la(s) columna(s) en los datos de la enfermedad o evento
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica
#' si se debe agregar una columna con el porcentaje de casos;
#' su valor por defecto es `FALSE`
#' @return Un `data.frame` con los datos de una enfermedad
#' o evento agrupados por el nombre de la(s) columna(s) y el
#' número de casos
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
#' @param col_edad Un `character` (cadena de caracteres) con
#' el nombre de la columna que contiene las edades en los datos de
#' la enfermedad o evento
#' @param col_adicional Un `character` (cadena de caracteres) con
#' el nombre de la columna adicional para agrupar con las edades en
#' los datos de la enfermedad o evento; su valor por defecto es `NULL`
#' @param min_val Un `numeric` (numerico) que contiene la edad mínima
#' con la que debe iniciar el rango de edades
#' @param max_val Un `numeric` (numerico) que contiene la edad máxima
#' con la que debe finalizar el rango de edades
#' @param paso Un `numeric` (numerico) que contiene el valor del paso
#' para generar el rango de edades
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si se debe
#' agregar una columna con el porcentaje de casos; su valor por
#' defecto es `TRUE`
#' @return Un `data.frame` con los datos de la enfermedad o evento
#' agrupados por el rango de edad y número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' data_edad <- agrupar_cols_casos(data_event = data_limpia,
#'                                 c("edad", "semana"),
#'                                 porcentaje = TRUE)
#' agrupar_rango_edad(data_event = data_edad,
#'                    col_edad = "edad",
#'                    min_val = 0,
#'                    max_val = max(data_edad$edad),
#'                    paso = 10,
#'                    porcentaje = TRUE)
#' @export
agrupar_rango_edad <- function(data_event,
                               col_edad,
                               col_adicional = NULL,
                               min_val,
                               max_val,
                               paso,
                               porcentaje = TRUE) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0)
  data_vals_rango <- data.frame()
  if (is.null(col_edad) || length(col_edad) > 0) {
    col_edad <- "edad"
  }
  stopifnot("El parametro col_edad debe ser una cadena de caracteres"
            = is.character(col_edad))
  total_casos <- sum(data_event$casos)
  data_vals_rango <- data_event %>%
    dplyr::mutate(ranges = cut(
      data_event[[col_edad]],
      seq(min_val, max_val, paso)
    )) %>%
    dplyr::group_by_at(c("ranges", col_adicional)) %>%
    dplyr::summarize(casos = sum(.data$casos),
                     .groups = "drop") %>%
    as.data.frame()
  if (porcentaje) {
    data_vals_rango <- data_vals_rango %>%
      mutate(porcentaje =  round(.data$casos / total_casos * 100, 3))
  }
  names(data_vals_rango)[names(data_vals_rango) == "ranges"] <- col_edad
  return(data_vals_rango)
}

#' Agrupar por fecha de inicio de síntomas y casos
#'
#' Función que agrupa los datos de una enfermedad o evento por
#' fecha de inicio de síntomas y número de casos
#' @param data_event Un `data.frame` que contiene los datos de
#' la enfermedad o evento
#' @param col_fecha Un `character` (cadena de caracteres) con el
#' nombre de la columna de los datos de la enfermedad o evento que contiene
#' las fechas de inicio de síntomas; su valor por defecto es `"ini_sin"`
#' @return Un `data.frame` con los datos de la enfermedad o evento
#' agrupados por fecha de inicio de síntomas y número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_fecha_inisintomas(data_event = data_limpia,
#'                           col_fecha = "ini_sin")
#' @export
agrupar_fecha_inisintomas <- function(data_event,
                                      col_fecha = "ini_sin") {
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
  if (is.null(col_fecha)) {
    col_fecha <- fechas_cols_nombres[3]
  }
  stopifnot("El parametro col_fecha debe ser una cadena de caracteres"
            = is.character(col_fecha))
  nomb_cols <- append(col_fecha, "semana")
  data_agrupada_fecha_ini <- agrupar_cols_casos(data_event,
                                                nomb_cols = nomb_cols)
  return(data_agrupada_fecha_ini)
}

#' Agrupar por fecha de notificación y casos
#'
#' Función que agrupa los datos de una enfermedad o evento por fecha de
#' notificación y número de casos
#' @param data_event Un `data.frame` que contiene los datos de la enfermedad
#' o evento
#' @param col_fecha Un `character` (cadena de caracteres) con el nombre de
#' la columna que contiene las fechas de notificación en los datos de la
#' enfermedad o evento; su valor por defecto es `"fec_not"`
#' @return Un `data.frame` con los datos de enfermedades agrupados por fecha de
#' notificación y número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_fecha_notifica(data_event = data_limpia,
#'                        col_fecha = "fec_not")
#' @export
agrupar_fecha_notifica <- function(data_event,
                                   col_fecha = "fec_not") {
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
  if (is.null(col_fecha)) {
    col_fecha <- fechas_cols_nombres[2]
  }
  stopifnot("El parametro col_fecha debe ser una cadena de caracteres"
            = is.character(col_fecha))
  nomb_cols <- append(col_fecha, "semana")
  data_agrupada_fecha_not <- agrupar_cols_casos(data_event,
                                                nomb_cols = nomb_cols)
  return(data_agrupada_fecha_not)
}

#' Agrupar por sexo y casos
#'
#' Función que agrupa los datos de una enfermedad o evento
#' por sexo y número de casos
#' @param data_event Un `data.frame` que contiene los datos de la enfermedad
#' o evento
#' @param col_sex Un `character` (cadena de caracteres) con el nombre
#' de la columna que contiene el sexo en los datos de la enfermedad o evento;
#' su valor por defecto es `"sexo"`
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si se debe
#' agregar una columna con el porcentaje de casos; su valor por
#' defecto es `TRUE`
#' @return Un data.frame con los datos de la enfermedad o evento
#' agrupados por sexo y número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_sex(data_event = data_limpia,
#'             col_sex = "sexo",
#'             porcentaje = TRUE)
#' @export
agrupar_sex <- function(data_event,
                        col_sex = "sexo",
                        porcentaje = TRUE) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro col_sex debe ser una cadena de caracteres"
            = is.character(col_sex))
  data_event_sex <- agrupar_cols_casos(data_event, col_sex, porcentaje)
  return(data_event_sex)
}

#' Agrupar por sexo, semana epidemiológica y casos
#'
#' Función que agrupa los datos de enfermedades por sexo, semana
#' epidemiológica y número de casos
#' @param data_event Un `data.frame` que contiene los datos de
#' la enfermedad o evento
#' @param cols_sex Un `character` (cadena de caracteres) o
#' `array (arreglo) de character` con el nombre de la(s)
#' columna(s) que contienen el sexo y las semanas
#' epidemiológicas; su valor por defecto es `c("sexo", "semana")`
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si se debe
#' agregar una columna con el porcentaje de casos; su valor por
#' defecto es `TRUE`
#' @return Un `data.frame` con los datos de la enfermedad o evento
#' agrupados por sexo, semana epidemiológica y número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_sex_semanaepi(data_event = data_limpia,
#'                       cols_sex = c("sexo", "semana"),
#'                       porcentaje = TRUE)
#' @export
agrupar_sex_semanaepi <- function(data_event,
                                  cols_sex = c("sexo", "semana"),
                                  porcentaje = TRUE) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro cols_sex debe ser una cadena de caracteres 
            o un arreglo de cadenas de caracteres "
            = (is.character(cols_sex) && !is.array(cols_sex)) ||
              (!is.character(cols_sex) && is.array(cols_sex)))
  data_event_sex_semanaepi <- agrupar_cols_casos(data_event,
                                                 cols_sex,
                                                 porcentaje)
  return(data_event_sex_semanaepi)
}

#' Agrupar por edad y casos
#'
#' Función que agrupa los datos de una enfermedad o evento por edad
#' y número de casos
#' @param data_event Un `data.frame` que contiene los datos de la enfermedad
#' o evento
#' @param col_edad Un `character` (cadena de caracteres) con el nombre
#' de la columna que contiene las edades en los datos de la enfermedad o
#' evento; su valor por defecto es `"edad"`
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si se debe
#' agregar una columna con el porcentaje de casos; su valor por
#' defecto es `FALSE`
#' @param interval_edad Un `numeric` (numerico) que contiene el intervalo del
#' rango de edades; su valor por defecto es `10`
#' @return Un `data.frame` con los datos de la enfermedad o evento agrupados
#' por edad y número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_edad(data_event = data_limpia,
#'              col_edad = "edad",
#'              porcentaje = FALSE)
#' @export
agrupar_edad <- function(data_event,
                         col_edad = "edad",
                         interval_edad = 10,
                         porcentaje = FALSE) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro col_edad debe ser una cadena de caracteres"
            = is.character(col_edad),
            "El parametro interval_edad debe ser un numero"
            = is.numeric(interval_edad))
  data_event_edad <- agrupar_cols_casos(data_event,
                                        col_edad)
  data_event_edad <-
    agrupar_rango_edad(data_event_edad,
                       col_edad,
                       min_val = 0,
                       max_val =
                         max(data_event_edad[[col_edad]]),
                       paso = interval_edad,
                       porcentaje = porcentaje)
  return(data_event_edad)
}

#' Agrupar por edades, sexo y casos
#'
#' Función que agrupa los datos de una enfermedad o evento por edades,
#' sexo y número de casos
#' @param data_event Un `data.frame` que contiene los datos de la enfermedad
#' o evento
#' @param col_edad Un `character` (cadena de caracteres) con el nombre de la
#' columna que contiene las edades en los datos de la enfermdedad o evento;
#' su valor por defecto es `"edad`
#' @param col_sex Un `character` (cadena de caracteres) con el nombre de la
#' columna que contiene el sexo en los datos de la enfermdedad o evento;
#' su valor por defecto es `"sexo`
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si se debe
#' agregar una columna con el porcentaje de casos; su valor por
#' defecto es `TRUE`
#' @param interval_edad Un `numeric` (numerico) que contiene el intervalo del
#' rango de edades; su valor por defeccto es `10`
#' @return Un `data.frame` con los datos de enfermedades agrupados
#' por edades, sexo y número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_edad_sex(data_event = data_limpia,
#'                  col_edad = "edad",
#'                  col_sex = "sexo",
#'                  porcentaje = TRUE)
#' @export
agrupar_edad_sex <- function(data_event,
                             col_edad = "edad",
                             col_sex = "sexo",
                             porcentaje = TRUE,
                             interval_edad = 10) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro col_edad debe ser una cadena de caracteres"
            = is.character(col_edad),
            "El parametro col_sex debe ser una cadena de caracteres"
            = is.character(col_sex),
            "El parametro porcentaje debe ser un booleano (TRUE o FALSE)" =
              is.logical(porcentaje))
  nomb_cols <- c(col_edad, col_sex)
  data_event_edad_sex <- agrupar_cols_casos(data_event,
                                            nomb_cols,
                                            porcentaje)
  data_event_edad_sex <- agrupar_rango_edad(
    data_event_edad_sex,
    nomb_cols[1],
    nomb_cols[2],
    min_val = 0,
    max_val =
      max(data_event_edad_sex[[nomb_cols[1]]]),
    paso = interval_edad,
    porcentaje = porcentaje
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
#' @param col_dpto Un `character` (cadena de caracteres) con el nombre
#' de la columna que contiene los códigos de los departamentos en los
#' datos de la enfermedad o evento; su valor por defecto es `"cod_dpto_o"`
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si se debe
#' agregar una columna con el porcentaje de casos; su valor por
#' defecto es `FALSE`
#' @return Un `data.frame` con los datos de la enfermedad o evento agrupados
#' por códigos de departamento y número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_dpto(data_event = data_limpia,
#'              col_dpto = "cod_dpto_o",
#'              porcentaje = FALSE)
#' @export
agrupar_dpto <- function(data_event,
                         col_dpto = "cod_dpto_o",
                         porcentaje = FALSE) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro col_dpto debe ser una cadena de caracteres"
            = is.character(col_dpto),
            "El parametro porcentaje debe ser un booleano (TRUE o FALSE)" =
              is.logical(porcentaje))
  data_event_cods_dpto <- data_event
  nomb_cols <- obtener_tip_ocurren_geo(data_event_cods_dpto$cod_eve[1])
  data_event_cods_dpto <- agrupar_cols_casos(data_event_cods_dpto,
                                             nomb_cols = nomb_cols[1:2],
                                             porcentaje = porcentaje)
  data_event_cods_dpto[[nomb_cols[1]]] <-
    as.character(data_event_cods_dpto[[nomb_cols[1]]])
  return(data_event_cods_dpto)
}

#' Agrupar por municipios y casos
#'
#' Función que agrupa los datos de una enfermedad o evento por código
#' de municipios y número de casos
#' @param data_event Un `data.frame` que contiene los datos de la
#' enfermedad o evento
#' @param dpto Un `character` (cadena de caracteres) o `numeric` (numerico)
#' que contiene el nombre del departamento; su valor por defecto es `NULL`
#' @param col_mpio Un `character` (cadena de caracteres) con el nombre de
#' la columna que contiene los códigos de los municipios en los datos de la
#' enfermedad o evento; su valor por defecto es `"cod_mun_o"`
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si se debe
#' agregar una columna con el porcentaje de casos; su valor por
#' defecto es `FALSE`
#' @return Un `data.frame` con los datos de la enfermedad o evento agrupados
#' por códigos de municipios y número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_mpio(data_event = data_limpia,
#'              dpto = "ANTIOQUIA",
#'              col_mpio = "cod_mun_o",
#'              porcentaje = FALSE)
#' agrupar_mpio(data_event = data_limpia,
#'              dpto = "05",
#'              col_mpio = "cod_mun_o",
#'              porcentaje = FALSE)
#' agrupar_mpio(data_event = data_limpia,
#'              dpto = 05,
#'              col_mpio = "cod_mun_o",
#'              porcentaje = TRUE)
#' @export
agrupar_mpio <- function(data_event,
                         dpto = NULL,
                         col_mpio = "cod_mun_o",
                         porcentaje = FALSE) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro col_mpio debe ser una cadena de caracteres"
            = is.character(col_mpio),
            "El parametro porcentaje debe ser un booleano (TRUE o FALSE)" =
              is.logical(porcentaje))
  cols_geo_ocurrencia <- data.frame()
  cod_events <- unique(data_event$cod_eve)
  for (cod in cod_events) {
    cols_geo_ocurrencia <- append(cols_geo_ocurrencia,
                                  obtener_tip_ocurren_geo(cod))
  }
  nomb_cols <- obtener_tip_ocurren_geo(data_event$cod_eve[1])
  data_event_muns <- data_event
  dept_data <- NULL
  if (!is.null(dpto)) {
    aux_dpto <- unique(data_event_muns[[nomb_cols[1]]])
    if (length(aux_dpto) > 1) {
      data_event_muns <- geo_filtro(data_event, dpto)
    }
  } else {
    dpto <- unique(data_event_muns[[nomb_cols[1]]])
    if (length(dpto) != 1) {
      stopifnot("Debe ingresar el nombre o codigo del departamento" =
                length(dpto) == 1)
    }
  }
  dept_data <- obtener_info_depts(dpto)
  data_event_muns <- agrupar_cols_casos(data_event_muns,
                                        nomb_cols = nomb_cols[1:4],
                                        porcentaje = porcentaje)
  data_event_muns[[nomb_cols[1]]] <-
    as.character(data_event_muns[[nomb_cols[1]]])
  data_event_muns[[nomb_cols[3]]] <-
    as.character(data_event_muns[[nomb_cols[3]]])
  dept_data <- dept_data[1, ]
  data_event_muns <-  dplyr::arrange(data_event_muns,
                                     dplyr::desc(.data$casos))
  return(data_event_muns)
}

#' Agrupar por área geográfica
#'
#' Función que agrupa los datos de una enfermedad o evento por área
#' geográfica a nivel departamental o municipal
#' @param data_event Un `data.frame` que contiene los datos de la
#' enfermedad o evento
#' @param dpto Un `character` (cadena de caracteres) que contiene
#' el nombre del departamento; su valor por defecto es `NULL`; si se ingresa
#' un valor en este parámetro se procederá agrupar los datos por los
#' municipios del departamento y sus áreas geográficas; si no se
#' ingresa un valor en este parámetro validará si los datos ya están
#' filtrados por algún departamento, si no lo están generará la agrupación
#' por departamento
#' @param col_area Un `character` (cadena de caracteres) con el nombre de
#' la columna que contiene las áreas geográficas en los datos de la enfermedad
#' o evento; su valor por defecto es `"cod_mun_o"`
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si se debe
#' agregar una columna con el porcentaje de casos; su valor por
#' defecto es `FALSE`
#' @return Un `data.frame` con los datos de la enfermedad o evento agrupados
#' por códigos de municipios y número de casos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_area_geo(data_event = data_limpia,
#'                  dpto = "Antioquia",
#'                  col_area = "area",
#'                  porcentaje = FALSE)
#' @export
agrupar_area_geo <- function(data_event,
                             dpto = NULL,
                             col_area = "area",
                             porcentaje = FALSE) {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro col_area debe ser una cadena de caracteres"
            = is.character(col_area),
            "El parametro porcentaje debe ser un booleano (TRUE o FALSE)" =
              is.logical(porcentaje))
  nomb_cols <- append(col_area,
                     obtener_tip_ocurren_geo(data_event$cod_eve[1])[1:4])
  data_event_area <- data_event
  if (!is.null(dpto)) {
      aux_dpto <- unique(data_event_area[[nomb_cols[2]]])
      if (length(aux_dpto) > 1) {
        data_event_area <- geo_filtro(data_event, dpto)
      }
      data_event_area <- agrupar_cols_casos(data_event_area,
                                            nomb_cols)
  } else {
    dpto <- unique(data_event_area[[nomb_cols[3]]])
    if (length(dpto) != 1) {
      nomb_cols <- nomb_cols[1:3]
    }
    data_event_area <- agrupar_cols_casos(data_event_area,
                                          nomb_cols)
  }
  data_event_area <- dplyr::arrange(data_event_area,
                                    dplyr::desc(.data$casos))
  return(data_event_area)
}

#' Agrupar por tipo de enfermedad o evento
#'
#' Función que agrupa los casos por tipo de enfermedad o evento
#' @param data_event Un `data.frame` que contiene los datos de la
#' enfermedad o evento
#' @param col_event Un `character` (cadena de caracteres) con el nombre de
#' la columna que contiene los códigos de los eventos o de las enfermedades
#' en los datos; su valor por defecto es `"cod_eve"`
#' @return Un `data.frame` con los datos de la enfermedad o evento agrupados
#' por sus tipos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_eventos(data_event = data_limpia,
#'                 col_event = "cod_eve")
#' @export
agrupar_eventos <- function(data_event, col_event = "cod_eve") {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro col_event debe ser una cadena de caracteres"
            = is.character(col_event))
  data_event_tipos <- agrupar_cols_casos(data_event, nomb_cols = col_event)
  return(data_event_tipos)
}

#' Agrupar por los años de una enfermedad o evento
#'
#' Función que agrupa los casos por los años de una enfermedad o evento
#' @param data_event Un `data.frame` que contiene los datos de la
#' enfermedad o evento
#' @param col_year Un `character` (cadena de caracteres) con el nombre de
#' la columna que contiene los años en los datos de la enfermedad o evento;
#' su valor por defecto es `"ano"`
#' @return Un `data.frame` con los datos de la enfermedad o evento agrupados
#' por sus años
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_years(data_event = data_limpia,
#'               col_year = "ano")
#' @export
agrupar_years <- function(data_event, col_year = "ano") {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro col_year debe ser una cadena de caracteres"
            = is.character(col_year))
  data_event_year <- agrupar_cols_casos(data_event,
                                        nomb_cols = c(col_year,
                                                      "cod_eve"))
  return(data_event_year)
}

#' Agrupar por la clasificación inicial del caso
#'
#' Función que agrupa los casos por la clasificación inicial del caso
#' @param data_event Un `data.frame` que contiene los datos de la
#' enfermedad o evento
#' @param cols_tipo Un `character` (cadena de caracteres) con el nombre de
#' las columna(s) que contiene la clasificación inicial del caso en los datos
#' de la enfermedad o evento; su valor por defecto es `"tip_cas"`
#' @return Un `data.frame` con los datos de la enfermedad o evento agrupados
#' por la clasificación inicial del caso y/u otras variables como los años
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_tipo_caso(data_event = data_limpia,
#'                   cols_tipo = "tip_cas")
#' @export
agrupar_tipo_caso <- function(data_event, cols_tipo = "tip_cas") {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro cols_tipo debe ser una cadena de caracteres"
            = is.character(cols_tipo))
  if (length(cols_tipo) == 1) {
   cols_tipo <- c(cols_tipo, "cod_eve")
  }
  etiquetas <- config::get(file =
                             system.file("extdata",
                                         "config.yml",
                                         package = "sivirep"),
                           "labels_cas_tip")
  data_event_tipo <- agrupar_cols_casos(data_event,
                                        nomb_cols = cols_tipo)
  data_event_tipo <- data_event_tipo %>%
    dplyr::mutate(nombre_tip_cas =
                    etiquetas[as.numeric(.data[[cols_tipo[1]]])])
  return(data_event_tipo)
}

#' Agrupar por la pertenencia étnica
#'
#' Función que agrupa los casos por la pertenencia étnica
#' @param data_event Un `data.frame` que contiene los datos de la
#' enfermedad o evento
#' @param cols_etn Un `character` (cadena de caracteres) con el nombre de
#' las columna(s) que contiene(n) la pertenencia étnica en los datos de la
#' enfermedad o evento; su valor por defecto es `"per_etn"`
#' @return Un `data.frame` con los datos de la enfermedad o evento agrupados
#' por la pertenencia étnica
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_per_etn(data_event = data_limpia,
#'                 cols_etn = "per_etn")
#' @export
agrupar_per_etn <- function(data_event, cols_etn = "per_etn") {
  stopifnot("El parametro data_event es obligatorio" = !missing(data_event),
            "El parametro data_event debe ser un data.frame" =
              is.data.frame(data_event),
            "El parametro data_event no debe estar vacio" =
              nrow(data_event) > 0,
            "El parametro cols_etn debe ser una cadena de caracteres"
            = is.character(cols_etn))
  if (length(cols_etn) == 1) {
    cols_etn <- c(cols_etn, "cod_eve")
  }
  etiquetas <- config::get(file =
                             system.file("extdata",
                                         "config.yml",
                                         package = "sivirep"),
                           "labels_cas_tip")
  etiquetas <- unlist(etiquetas)
  data_event_tipo <- agrupar_cols_casos(data_event,
                                        nomb_cols = cols_etn)
  data_event_tipo <- data_event_tipo %>%
    dplyr::mutate(nombre_per_etn =
                    etiquetas[as.character(.data[[cols_etn[1]]])])
  return(data_event_tipo)
}
