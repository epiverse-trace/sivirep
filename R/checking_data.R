#' @title Filtrar por departamentos y municipios
#' @description Función que filtra los datos de una enfermedad o evento por
#' departamentos y municipios.
#' @param data_event Un `data.frame` con los datos de una enfermedad o evento.
#' @param dpto Un `character` (cadena de caracteres) o `numeric` (numérico)
#' que contiene el nombre o código del departamento; valor por defecto `NULL`.
#' @param mpio Un `character` (cadena de caracteres) o `numeric` (numérico)
#' que contiene el nombre o código del municipio; su valor por defecto
#' es `NULL`.
#' @return Un `data.frame` con los datos filtrados con la enfermedad,
#' departamentos y municipios seleccionados.
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
  validar_data_event(data_event)
  data_dept_filt <- data.frame()
  dept_data <- data.frame()
  cols_ocurren <- NULL
  if (!is.null(dpto)) {
    dept_data <- obtener_info_depts(dpto, mpio)
    stopifnot(
      "El departamento o municipio ingresado no existe" = nrow(dept_data) > 0
    )
    dept_data <- dept_data[1, ]
    cols_ocurren <- obtener_tip_ocurren_geo(data_event$cod_eve[1])
  }
  if (!is.null(dept_data)) {
    data_dept_filt <-
      dplyr::filter(
        data_event,
        data_event[[cols_ocurren[1]]] %in% dept_data$codigo_departamento
      )
  }
  if (!is.null(mpio)) {
    stopifnot(
      "El parametro mpio debe ser una cadena de caracteres o un
              numero" = is.character(mpio) | is.numeric(mpio)
    )
    data_dept_filt[[cols_ocurren[3]]] <-
      as.character(data_dept_filt[[cols_ocurren[3]]])
    data_dept_filt <-
      dplyr::filter(
        data_dept_filt,
        data_dept_filt[[cols_ocurren[3]]] %in%
          as.character(dept_data$codigo_municipio)
      )
  }
  return(data_dept_filt)
}

#' @title Agrupar por semana epidemiológica y casos
#' @description Función que agrupa los datos de una enfermedad o evento
#' por semana epidemiológica y número de casos.
#' @param data_event Un `data.frame` que contiene los datos de
#' una enfermedad o evento.
#' @param col_semanaepi Un `character` (cadena de caracteres)
#' con el nombre de la columna que contiene las semanas
#' epidemiológicas en los datos de la enfermedad o evento;
#' su valor por defecto es `"semana"`.
#' @return Un `data.frame` con los datos de una enfermedad o
#' evento agrupados por semana epidemiológica y número de casos.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_semanaepi(
#'   data_event = data_limpia,
#'   col_semanaepi = "semana"
#' )
#' @export
agrupar_semanaepi <- function(data_event,
                              col_semanaepi = "semana") {
  validar_data_event(data_event)
  stopifnot(
    "El parametro col_semanaepi debe ser una cadena de caracteres" =
      is.character(col_semanaepi)
  )
  data_event_agrupada <- dplyr::group_by(
    data_event,
    dplyr::across(dplyr::all_of(col_semanaepi))
  )
  data_event_agrupada <- dplyr::summarise(data_event_agrupada,
    casos = sum(.data$uni_med)
  )
  data_event_agrupada <- data_event_agrupada[1:53, ]
  return(data_event_agrupada)
}

#' @title Agrupar por columnas y casos
#' @description Función que agrupa los datos de una enfermedad o evento
#' por nombre de columna(s) y número de casos.
#' @param data_event Un `data.frame` que contiene los datos de
#' una enfermedad o evento.
#' @param nomb_cols Un `character` (cadena de caracteres) o
#' `array (arreglo) de character` que contiene el nombre de
#' la(s) columna(s) en los datos de la enfermedad o evento.
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica
#' si se debe agregar una columna con el porcentaje de casos;
#' su valor por defecto es `FALSE`.
#' @param estandar Un `boolean` (TRUE o FALSE) que indica
#' si se debe utilizar el estándar de agrupación de los datos
#' del evento o enfermedad propuesto por el paquete, es decir,
#' que se incluyan estas columnas o variables como parte del
#' resultado `c("cod_eve", "nombre_evento", "ano")`; su valor
#' por defecto es `TRUE`, si su valor es `FALSE` agrupará los
#' datos solamente por las columnas o variables enviadas en el
#' párametro `nomb_cols`.
#' @return Un `data.frame` con los datos de una enfermedad
#' o evento agrupados por el nombre de la(s) columna(s) y el
#' número de casos.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_cols_casos(
#'   data_event = data_limpia,
#'   nomb_cols = "sexo",
#'   porcentaje = TRUE
#' )
#' agrupar_cols_casos(
#'   data_event = data_limpia,
#'   nomb_cols = c("sexo", "semana")
#' )
#' @export
agrupar_cols_casos <- function(data_event,
                               nomb_cols,
                               porcentaje = FALSE,
                               estandar = TRUE) {
  validar_data_event(data_event)
  validar_nomb_cols(data_event, nomb_cols)
  validar_porcentaje(porcentaje)
  if (estandar) {
    nomb_cols <- c(nomb_cols, c("cod_eve", "nombre_evento", "ano"))
    data_event_agrupada <- dplyr::group_by(
      data_event,
      dplyr::across(dplyr::all_of(nomb_cols))
    )
    data_event_agrupada <- dplyr::summarise(data_event_agrupada,
      casos = dplyr::n(),
      .groups = "drop"
    )
  } else {
    data_event_agrupada <- dplyr::group_by(
      data_event,
      dplyr::across(dplyr::all_of(nomb_cols))
    )
    data_event_agrupada <- dplyr::summarise(data_event_agrupada,
      casos = sum(.data[["casos"]]),
      .groups = "drop"
    )
  }
  if (porcentaje) {
    data_event_agrupada <-
      dplyr::mutate(data_event_agrupada,
        porcentaje =
          round(
            data_event_agrupada$casos
              / sum(data_event_agrupada$casos) * 100,
            2
          )
      )
  }
  return(data_event_agrupada)
}

#' @title Agrupar por rango de edad y casos
#' @description Función que agrupa los datos de una enfermedad o evento por
#' rango de edad y número de casos.
#' @param data_event Un `data.frame` que contiene los datos de la
#' enfermedad o evento.
#' @param col_edad Un `character` (cadena de caracteres) con
#' el nombre de la columna que contiene las edades en los datos de
#' la enfermedad o evento.
#' @param col_adicional Un `character` (cadena de caracteres) con
#' el nombre de la columna adicional para agrupar con las edades en
#' los datos de la enfermedad o evento; su valor por defecto
#' es `NULL`.
#' @param min_val Un `numeric` (numérico) que contiene la edad mínima
#' con la que debe iniciar el rango de edades.
#' @param max_val Un `numeric` (numérico) que contiene la edad máxima
#' con la que debe finalizar el rango de edades.
#' @param paso Un `numeric` (numérico) que contiene el valor del paso
#' para generar el rango de edades.
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si se debe
#' agregar una columna con el porcentaje de casos; su valor por
#' defecto es `TRUE`.
#' @return Un `data.frame` con los datos de la enfermedad o evento
#' agrupados por el rango de edad y número de casos.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' data_edad <- agrupar_cols_casos(
#'   data_event = data_limpia,
#'   c("edad", "semana"),
#'   porcentaje = TRUE
#' )
#' agrupar_rango_edad(
#'   data_event = data_edad,
#'   col_edad = "edad",
#'   min_val = 0,
#'   max_val = max(data_edad$edad, na.rm = TRUE),
#'   paso = 10,
#'   porcentaje = TRUE
#' )
#' @export
agrupar_rango_edad <- function(data_event,
                               col_edad = "edad",
                               col_adicional = NULL,
                               min_val,
                               max_val,
                               paso,
                               porcentaje = TRUE) {
  validar_data_event(data_event)
  validar_edad(data_event, col_edad)
  total_casos <- sum(data_event$casos)
  data_vals_rango <-
    dplyr::mutate(data_event, ranges = cut(
      data_event[[col_edad]],
      seq(min_val, max_val, paso)
    ))
  data_vals_rango <- dplyr::group_by(
    data_vals_rango,
    dplyr::across(dplyr::all_of(c("ranges", col_adicional)))
  )
  data_vals_rango <- dplyr::summarize(data_vals_rango,
    casos = sum(.data$casos),
    .groups = "drop"
  )
  data_vals_rango <- as.data.frame(data_vals_rango)
  if (porcentaje) {
    data_vals_rango <- data_vals_rango %>%
      mutate(porcentaje = round(.data$casos / total_casos * 100, 3))
  }
  names(data_vals_rango)[names(data_vals_rango) == "ranges"] <- col_edad
  return(data_vals_rango)
}

#' @title Agrupar por fecha de inicio de síntomas y casos
#' @description Función que agrupa los datos de una enfermedad o evento por
#' fecha de inicio de síntomas y número de casos.
#' @param data_event Un `data.frame` que contiene los datos de
#' la enfermedad o evento.
#' @param col_fecha Un `character` (cadena de caracteres) con el
#' nombre de la columna de los datos de la enfermedad o evento que contiene
#' las fechas de inicio de síntomas; su valor por defecto es `"ini_sin"`.
#' @return Un `data.frame` con los datos de la enfermedad o evento
#' agrupados por fecha de inicio de síntomas y número de casos.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_fecha_inisintomas(
#'   data_event = data_limpia,
#'   col_fecha = "ini_sin"
#' )
#' @export
agrupar_fecha_inisintomas <- function(data_event,
                                      col_fecha = "ini_sin") {
  fechas_cols_nombres <- obtener_val_config("dates_column_names")
  validar_data_event(data_event)
  if (is.null(col_fecha)) {
    col_fecha <- fechas_cols_nombres[3]
  }
  stopifnot(
    "El parametro col_fecha debe ser una cadena de caracteres" =
      is.character(col_fecha)
  )
  nomb_cols <- c(col_fecha, "semana")
  data_agrupada_fecha_ini <- agrupar_cols_casos(data_event,
    nomb_cols = nomb_cols
  )
  return(data_agrupada_fecha_ini)
}

#' @title Agrupar por sexo y casos
#' @description Función que agrupa los datos de una enfermedad o evento
#' por sexo y número de casos.
#' @param data_event Un `data.frame` que contiene los datos de la enfermedad
#' o evento.
#' @param col_sex Un `character` (cadena de caracteres) con el nombre
#' de la columna que contiene el sexo en los datos de la enfermedad o evento;
#' su valor por defecto es `"sexo"`.
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si se debe
#' agregar una columna con el porcentaje de casos; su valor por
#' defecto es `TRUE`.
#' @return Un `data.frame` con los datos de la enfermedad o evento
#' agrupados por sexo y número de casos.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_sex(
#'   data_event = data_limpia,
#'   col_sex = "sexo",
#'   porcentaje = TRUE
#' )
#' @export
agrupar_sex <- function(data_event,
                        col_sex = "sexo",
                        porcentaje = TRUE) {
  validar_data_event(data_event)
  validar_sex(data_event, col_sex)
  validar_porcentaje(porcentaje)
  data_event_sex <- agrupar_cols_casos(data_event, col_sex, porcentaje)
  return(data_event_sex)
}

#' @title Agrupar por sexo, semana epidemiológica y casos
#' @description Función que agrupa los datos de enfermedades por sexo,
#' semana epidemiológica y número de casos.
#' @param data_event Un `data.frame` que contiene los datos de
#' la enfermedad o evento.
#' @param cols_sex Un `character` (cadena de caracteres) o
#' `array` (arreglo) de `character` con el nombre de la(s)
#' columna(s) que contienen el sexo y las semanas
#' epidemiológicas; su valor por defecto es `c("sexo", "semana")`.
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si se debe
#' agregar una columna con el porcentaje de casos; su valor por
#' defecto es `TRUE`.
#' @return Un `data.frame` con los datos de la enfermedad o evento
#' agrupados por sexo, semana epidemiológica y número de casos.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_sex_semanaepi(
#'   data_event = data_limpia,
#'   cols_sex = c("sexo", "semana"),
#'   porcentaje = TRUE
#' )
#' @export
agrupar_sex_semanaepi <- function(data_event,
                                  cols_sex = c("sexo", "semana"),
                                  porcentaje = TRUE) {
  validar_data_event(data_event)
  stopifnot(
    "El parametro cols_sex debe ser una cadena de caracteres
            o un arreglo de cadenas de caracteres " =
      (is.character(cols_sex) && !is.array(cols_sex)) ||
        (!is.character(cols_sex) && is.array(cols_sex))
  )
  data_event_sex_semanaepi <- agrupar_cols_casos(
    data_event,
    cols_sex,
    porcentaje
  )
  return(data_event_sex_semanaepi)
}

#' @title Agrupar por edad y casos
#' @description Función que agrupa los datos de una enfermedad o evento por
#' edad y número de casos.
#' @param data_event Un `data.frame` que contiene los datos de la enfermedad
#' o evento.
#' @param col_edad Un `character` (cadena de caracteres) con el nombre
#' de la columna que contiene las edades en los datos de la enfermedad o
#' evento; su valor por defecto es `"edad"`.
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si se debe
#' agregar una columna con el porcentaje de casos; su valor por
#' defecto es `FALSE`.
#' @param interval_edad Un `numeric` (numérico) que contiene el intervalo del
#' rango de edades; su valor por defecto es `10`.
#' @return Un `data.frame` con los datos de la enfermedad o evento agrupados
#' por edad y número de casos.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_edad(
#'   data_event = data_limpia,
#'   col_edad = "edad",
#'   porcentaje = FALSE
#' )
#' @export
agrupar_edad <- function(data_event,
                         col_edad = "edad",
                         interval_edad = 10,
                         porcentaje = FALSE) {
  validar_data_event(data_event)
  validar_edad(data_event, col_edad)
  validar_porcentaje(porcentaje)
  stopifnot(
    "El parametro interval_edad debe ser un numero" = is.numeric(interval_edad)
  )
  data_event_edad <- agrupar_cols_casos(
    data_event,
    col_edad
  )
  data_event_edad <-
    agrupar_rango_edad(data_event_edad,
      col_edad,
      min_val = 0,
      max_val =
        max(data_event_edad[[col_edad]]),
      paso = interval_edad,
      porcentaje = porcentaje
    )
  return(data_event_edad)
}

#' @title Agrupar por edades, sexo y casos
#' @description Función que agrupa los datos de una enfermedad o evento por
#' edades, sexo y número de casos.
#' @param data_event Un `data.frame` que contiene los datos de la enfermedad
#' o evento.
#' @param col_edad Un `character` (cadena de caracteres) con el nombre de la
#' columna que contiene las edades en los datos de la enfermedad o evento;
#' su valor por defecto es `"edad"`.
#' @param col_sex Un `character` (cadena de caracteres) con el nombre de la
#' columna que contiene el sexo en los datos de la enfermedad o evento;
#' su valor por defecto es `"sexo`.
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si se debe
#' agregar una columna con el porcentaje de casos; su valor por
#' defecto es `TRUE`.
#' @param interval_edad Un `numeric` (numérico) que contiene el intervalo del
#' rango de edades; su valor por defecto es `10`.
#' @return Un `data.frame` con los datos de enfermedades agrupados
#' por edades, sexo y número de casos.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_edad_sex(
#'   data_event = data_limpia,
#'   col_edad = "edad",
#'   col_sex = "sexo",
#'   porcentaje = TRUE
#' )
#' @export
agrupar_edad_sex <- function(data_event,
                             col_edad = "edad",
                             col_sex = "sexo",
                             porcentaje = TRUE,
                             interval_edad = 10) {
  validar_data_event(data_event)
  validar_edad(data_event, col_edad)
  validar_sex(data_event, col_sex)
  validar_porcentaje(porcentaje)
  nomb_cols <- c(col_edad, col_sex)
  data_event_edad_sex <- agrupar_cols_casos(
    data_event,
    nomb_cols,
    porcentaje
  )
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

#' @title Agrupar por departamento y casos
#' @description Función que agrupa los datos por códigos de departamento y
#' número de casos.
#' @param data_event Un `data.frame` que contiene los datos de la
#' enfermedad o evento.
#' @param col_dpto Un `character` (cadena de caracteres) con el nombre
#' de la columna que contiene los códigos de los departamentos en los
#' datos de la enfermedad o evento; su valor por defecto es `"cod_dpto_o"`.
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si se debe
#' agregar una columna con el porcentaje de casos; su valor por
#' defecto es `FALSE`.
#' @return Un `data.frame` con los datos de la enfermedad o evento agrupados
#' por códigos de departamento y número de casos.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_dpto(
#'   data_event = data_limpia,
#'   col_dpto = "cod_dpto_o",
#'   porcentaje = FALSE
#' )
#' @export
agrupar_dpto <- function(data_event,
                         col_dpto = "cod_dpto_o",
                         porcentaje = FALSE) {
  validar_data_event(data_event)
  stopifnot(
    "El parametro col_dpto debe ser una cadena de caracteres" =
      is.character(col_dpto),
    "El parametro porcentaje debe ser un booleano (TRUE o FALSE)" =
      is.logical(porcentaje)
  )
  data_event_cods_dpto <- data_event
  nomb_cols <- obtener_tip_ocurren_geo(data_event_cods_dpto$cod_eve[1])
  data_event_cods_dpto <- agrupar_cols_casos(data_event_cods_dpto,
    nomb_cols = nomb_cols[1:2],
    porcentaje = porcentaje
  )
  data_event_cods_dpto[[nomb_cols[1]]] <-
    as.character(data_event_cods_dpto[[nomb_cols[1]]])
  return(data_event_cods_dpto)
}

#' @title Agrupar por municipios y casos
#' @description Función que agrupa los datos de una enfermedad o evento por
#' código de municipios y número de casos.
#' @param data_event Un `data.frame` que contiene los datos de la
#' enfermedad o evento.
#' @param dpto Un `character` (cadena de caracteres) o `numeric` (numérico)
#' que contiene el nombre del departamento; su valor por defecto es `NULL`.
#' @param col_mpio Un `character` (cadena de caracteres) con el nombre de
#' la columna que contiene los códigos de los municipios en los datos de la
#' enfermedad o evento; su valor por defecto es `"cod_mun_o"`.
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si se debe
#' agregar una columna con el porcentaje de casos; su valor por
#' defecto es `FALSE`.
#' @return Un `data.frame` con los datos de la enfermedad o evento agrupados
#' por códigos de municipios y número de casos.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_mpio(
#'   data_event = data_limpia,
#'   dpto = "ANTIOQUIA",
#'   col_mpio = "cod_mun_o",
#'   porcentaje = FALSE
#' )
#' agrupar_mpio(
#'   data_event = data_limpia,
#'   dpto = "05",
#'   col_mpio = "cod_mun_o",
#'   porcentaje = FALSE
#' )
#' agrupar_mpio(
#'   data_event = data_limpia,
#'   dpto = 05,
#'   col_mpio = "cod_mun_o",
#'   porcentaje = TRUE
#' )
#' @export
agrupar_mpio <- function(data_event,
                         dpto = NULL,
                         col_mpio = "cod_mun_o",
                         porcentaje = FALSE) {
  validar_data_event(data_event)
  stopifnot(
    "El parametro col_mpio debe ser una cadena de caracteres" =
      is.character(col_mpio),
    "El parametro porcentaje debe ser un booleano (TRUE o FALSE)" =
      is.logical(porcentaje)
  )
  cols_geo_ocurrencia <- data.frame()
  cod_events <- unique(data_event$cod_eve)
  for (cod in cod_events) {
    cols_geo_ocurrencia <- c(
      cols_geo_ocurrencia,
      obtener_tip_ocurren_geo(cod)
    )
  }
  nomb_cols <- obtener_tip_ocurren_geo(data_event$cod_eve[1])
  data_event_muns <- data_event
  if (!is.null(dpto)) {
    aux_dpto <- unique(data_event_muns[[nomb_cols[1]]])
    if (length(aux_dpto) > 1) {
      data_event_muns <- geo_filtro(data_event, dpto)
    }
  } else {
    dpto <- unique(data_event_muns[[nomb_cols[1]]])
    if (length(dpto) != 1) {
      stopifnot(
        "Debe ingresar el nombre o codigo del departamento" =
          length(dpto) == 1
      )
    }
  }
  dept_data <- obtener_info_depts(dpto)
  data_event_muns <- agrupar_cols_casos(data_event_muns,
    nomb_cols = nomb_cols[1:4],
    porcentaje = porcentaje
  )
  data_event_muns[[nomb_cols[1]]] <-
    as.character(data_event_muns[[nomb_cols[1]]])
  data_event_muns[[nomb_cols[3]]] <-
    as.character(data_event_muns[[nomb_cols[3]]])
  dept_data <- dept_data[1, ]
  data_event_muns <- dplyr::arrange(
    data_event_muns,
    dplyr::desc(.data$casos)
  )
  return(data_event_muns)
}

#' @title Agrupar por área geográfica
#' @description Función que agrupa los datos de una enfermedad o evento por
#' área geográfica.
#' @param data_event Un `data.frame` que contiene los datos de la
#' enfermedad o evento.
#' @param col_area Un `character` (cadena de caracteres) con el nombre de
#' la columna que contiene las áreas geográficas en los datos de la enfermedad
#' o evento; su valor por defecto es `"area"`.
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si se debe
#' agregar una columna con el porcentaje de casos; su valor por
#' defecto es `FALSE`.
#' @return Un `data.frame` con los datos de la enfermedad o evento agrupados
#' por área geográfica.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_area_geo(
#'   data_event = data_limpia,
#'   col_area = "area",
#'   porcentaje = FALSE
#' )
#' @export
agrupar_area_geo <- function(data_event,
                             col_area = "area",
                             porcentaje = FALSE) {
  validar_data_event(data_event)
  validar_area_geo(data_event, col_area)
  validar_porcentaje(porcentaje)
  data_event_area <- agrupar_cols_casos(
    data_event,
    col_area
  )
  data_event_area <- dplyr::arrange(
    data_event_area,
    dplyr::desc(.data$casos)
  )
  return(data_event_area)
}

#' @title Agrupar por área geográfica a nivel departamental o municipal
#' @description Función que agrupa los datos de una enfermedad o evento por
#' área geográfica a nivel departamental o municipal.
#' @param data_event Un `data.frame` que contiene los datos de la
#' enfermedad o evento.
#' @param dpto Un `character` (cadena de caracteres) que contiene
#' el nombre del departamento; su valor por defecto es `NULL`. Si se ingresa
#' un valor en este parámetro se procederá agrupar los datos por los
#' municipios del departamento y sus áreas geográficas. Si no se
#' ingresa un valor en este parámetro validará si los datos ya están
#' filtrados por algún departamento; si no lo están generará la agrupación
#' por departamento.
#' @param col_area Un `character` (cadena de caracteres) con el nombre de
#' la columna que contiene las áreas geográficas en los datos de la enfermedad
#' o evento; su valor por defecto es `"cod_mun_o"`.
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica si se debe
#' agregar una columna con el porcentaje de casos; su valor por
#' defecto es `FALSE`.
#' @param top Un `numeric` (numerico) que indica la cantidad de departamentos o
#' municipios con mayor número de casos que se deben retornar; su valor
#' por defecto es `10`.
#' @return Un `data.frame` con los datos de la enfermedad o evento agrupados
#' por códigos de municipios y número de casos.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_top_area_geo(
#'   data_event = data_limpia,
#'   dpto = "Antioquia",
#'   col_area = "area",
#'   porcentaje = FALSE,
#'   top = 10
#' )
#' @export
agrupar_top_area_geo <- function(data_event,
                                 dpto = NULL,
                                 col_area = "area",
                                 porcentaje = FALSE,
                                 top = 10) {
  validar_data_event(data_event)
  validar_area_geo(data_event, col_area)
  validar_porcentaje(porcentaje)
  nomb_cols <- c(
    col_area,
    obtener_tip_ocurren_geo(data_event$cod_eve[1])[1:4]
  )
  data_event_area <- data_event
  if (!is.null(dpto)) {
    aux_dpto <- unique(data_event_area[[nomb_cols[2]]])
    if (length(aux_dpto) > 1) {
      data_event_area <- geo_filtro(data_event, dpto)
    }
  } else {
    dpto <- unique(data_event_area[[nomb_cols[3]]])
    if (length(dpto) != 1) {
      nomb_cols <- nomb_cols[1:3]
    }
  }
  data_event_area <- agrupar_cols_casos(
    data_event_area,
    nomb_cols
  )
  data_event_area <- dplyr::arrange(
    data_event_area,
    dplyr::desc(.data$casos)
  )
  if (top <= nrow(data_event_area)) {
    data_event_area <- data_event_area[1:top, ]
  }
  return(data_event_area)
}

#' @title Agrupar por tipo de enfermedad o evento
#' @description Función que agrupa los casos por tipo de enfermedad o evento.
#' @param data_event Un `data.frame` que contiene los datos de la
#' enfermedad o evento.
#' @param col_event Un `character` (cadena de caracteres) con el nombre de
#' la columna que contiene los códigos de los eventos o de las enfermedades
#' en los datos; su valor por defecto es `"cod_eve"`.
#' @return Un `data.frame` con los datos de la enfermedad o evento agrupados
#' por sus tipos.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_eventos(
#'   data_event = data_limpia,
#'   col_event = "cod_eve"
#' )
#' @export
agrupar_eventos <- function(data_event, col_event = "cod_eve") {
  validar_data_event(data_event)
  stopifnot(
    "El parametro col_event debe ser una cadena de caracteres" =
      is.character(col_event)
  )
  data_event_tipos <- agrupar_cols_casos(data_event, nomb_cols = col_event)
  return(data_event_tipos)
}

#' @title Agrupar por años de una enfermedad o evento
#' @description Función que agrupa los casos por los años de una enfermedad
#' o evento.
#' @param data_event Un `data.frame` que contiene los datos de la
#' enfermedad o evento.
#' @param col_year Un `character` (cadena de caracteres) con el nombre de
#' la columna que contiene los años en los datos de la enfermedad o evento;
#' su valor por defecto es `"ano"`.
#' @return Un `data.frame` con los datos de la enfermedad o evento agrupados
#' por año.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_years(
#'   data_event = data_limpia,
#'   col_year = "ano"
#' )
#' @export
agrupar_years <- function(data_event, col_year = "ano") {
  validar_data_event(data_event)
  validar_years(data_event, col_year)
  data_event_year <- agrupar_cols_casos(data_event,
    nomb_cols = c(
      col_year,
      "cod_eve"
    )
  )
  return(data_event_year)
}

#' @title Agrupar por la clasificación inicial del caso
#' @description Función que agrupa los casos por la clasificación inicial
#' del caso.
#' @param data_event Un `data.frame` que contiene los datos de la
#' enfermedad o evento.
#' @param cols_tipo Un `character` (cadena de caracteres) con el nombre de
#' las columna(s) que contiene la clasificación inicial del caso en los datos
#' de la enfermedad o evento; su valor por defecto es `"tip_cas"`.
#' @return Un `data.frame` con los datos de la enfermedad o evento agrupados
#' por la clasificación inicial del caso y/u otras variables como los años.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_tipo_caso(
#'   data_event = data_limpia,
#'   cols_tipo = "tip_cas"
#' )
#' @export
agrupar_tipo_caso <- function(data_event, cols_tipo = "tip_cas") {
  validar_data_event(data_event)
  stopifnot(
    "El parametro cols_tipo debe ser una cadena de caracteres" =
      is.character(cols_tipo)
  )
  if (length(cols_tipo) == 1) {
    cols_tipo <- c(cols_tipo, "cod_eve")
  }
  etiquetas <- obtener_val_config("labels_cas_tip")
  etiquetas <- as.character(etiquetas)
  data_event_tipo <- agrupar_cols_casos(data_event,
    nomb_cols = cols_tipo
  )
  data_event_tipo <-
    dplyr::mutate(data_event_tipo,
      nombre_tip_cas =
        etiquetas[as.numeric(data_event_tipo[[cols_tipo[1]]])]
    )
  return(data_event_tipo)
}

#' @title Agrupar por la pertenencia étnica
#' @description Función que agrupa los casos por la pertenencia étnica.
#' @param data_event Un `data.frame` que contiene los datos de la
#' enfermedad o evento.
#' @param cols_etn Un `character` (cadena de caracteres) con el nombre de
#' las columna(s) que contiene(n) la pertenencia étnica en los datos de la
#' enfermedad o evento; su valor por defecto es `"per_etn"`
#' @param porcentaje Un `boolean` (TRUE o FALSE) que indica.
#' si se debe agregar una columna con el porcentaje de casos;
#' su valor por defecto es `TRUE`.
#' @return Un `data.frame` con los datos de la enfermedad o evento agrupados
#' por la pertenencia étnica.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' agrupar_per_etn(
#'   data_event = data_limpia,
#'   cols_etn = "per_etn"
#' )
#' @export
agrupar_per_etn <- function(data_event, cols_etn = "per_etn",
                            porcentaje = TRUE) {
  validar_data_event(data_event)
  validar_per_etn(data_event, cols_etn)
  validar_porcentaje(porcentaje)
  if (length(cols_etn) == 1) {
    cols_etn <- c(cols_etn, "cod_eve")
  }
  etiquetas <- obtener_val_config("labels_per_etn")
  etiquetas <- unlist(etiquetas)
  data_event_tipo <- agrupar_cols_casos(data_event,
    nomb_cols = cols_etn,
    porcentaje = porcentaje
  )
  data_event_tipo <-
    dplyr::mutate(data_event_tipo,
      nombre_per_etn =
        unname(etiquetas[
          as.character(data_event_tipo[[cols_etn[1]]])
        ])
    )
  return(data_event_tipo)
}

#' @title Calcular incidencia
#' @description Función que calcula la incidencia de una enfermedad o evento
#' para todo Colombia, departamento o municipio.
#' @param data_incidencia Un `data.frame` que contiene la población a riesgo o
#' las proyecciones poblaciones DANE. Si este parámetro está vacío importará
#' la población a riesgo o las proyecciones dependiendo de la disponibilidad de
#' la información; su valor por defecto es `NULL`.
#' @param data_agrupada Un `data.frame` que contiene los datos de la enfermedad
#' agrupados por departamento o municipio y número de casos.
#' @param poblacion Un `character` (cadena de caracteres) con el tipo de
#' población para efectuar el calculo de la incidencia. Indica si se
#' va a utilizar la población a riesgo del evento `"riesgo"` o las
#' proyecciones poblacionales DANE `"proyecciones"`; su valor por defecto
#' es `NULL`.
#' @param year Un `numeric` (numerico) con el año que se debe tomar de las
#' proyecciones poblacionales; su valor por defecto es `NULL`.
#' @param dpto Un `character` (cadena de caracteres) o `numeric` (numérico)
#' que contiene el código o nombre del departamento; su valor por
#' defecto es `NULL`.
#' @param mpio Un `character` (cadena de caracteres) o `numeric` (numérico)
#' que contiene el código o nombre del municipio; su valor por defecto
#' es `NULL`.
#' @param sex Un `character` (cadena de caracteres) que contiene el sexo`"F"`
#' para Femenino y `"M"` Masculino; su valor por defecto es `NULL`.
#' @return Un `numeric` con el calculo de la incidencia para todo Colombia, un
#' departamento, municipio o sexo especifico.
#' @examples
#' \donttest{
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' # Cálculo de la incidencia con proyecciones poblacionales por departamento
#' data_agrupada_mpios <- agrupar_mpio(data_limpia, dpto = "Antioquia")
#' calcular_incidencia(
#'   data_agrupada = data_agrupada_mpios,
#'   poblacion = "proyecciones",
#'   dpto = "05",
#'   year = 2020
#' )
#' # Cálculo de la incidencia con proyecciones poblacionales por municipio
#' calcular_incidencia(
#'   data_agrupada = data_agrupada_mpios,
#'   poblacion = "proyecciones",
#'   dpto = "Antioquia",
#'   mpio = "05001",
#'   year = 2020
#' )
#' # Cálculo de la incidencia con población a riesgo para Colombia
#' data_agrupada_dptos <- agrupar_dpto(data_limpia)
#' calcular_incidencia(
#'   poblacion = "riesgo",
#'   data_agrupada = data_agrupada_dptos,
#'   year = 2020
#' )
#' }
#' @export
calcular_incidencia <- function(data_incidencia = NULL, data_agrupada,
                                poblacion = NULL, year = NULL,
                                dpto = NULL, mpio = NULL,
                                sex = NULL) {
  validar_data_agrupada(data_agrupada)
  nombre_evento <- data_agrupada$nombre_evento[1]
  vals_event <- obtener_cond_inciden_event(cod_eve = data_agrupada$cod_eve[1])
  coeficiente <- as.integer(vals_event$coeficiente)
  if (is.null(year)) {
    year <- as.numeric(obtener_year(data_agrupada))
  }
  if (is.null(poblacion)) {
    poblacion <- vals_event$denominador
  }
  pop_incidencia <-
    obtener_pob_incidencia(
      data_incidencia = data_incidencia,
      poblacion = poblacion,
      event = nombre_evento,
      year = year
    )
  data_incidencia <- pop_incidencia$data_incidencia
  poblacion <- pop_incidencia$poblacion
  poblacion_incidencia <- data_incidencia
  total_poblacion <- NULL
  incidencia <- 0.00
  nomb_cols <- obtener_tip_ocurren_geo(data_agrupada$nombre_evento[1])
  unidades_geo <- obtener_dpto_mpio(
    data_agrupada = data_agrupada,
    nomb_cols = nomb_cols,
    dpto = dpto, mpio = mpio
  )
  dpto <- unidades_geo$dpto
  mpio <- unidades_geo$mpio
  if (!is.null(dpto)) {
    if (poblacion == "proyecciones") {
      poblacion_incidencia <-
        dplyr::filter(
          poblacion_incidencia,
          .data$area_geografica == "Total",
          .data$dp == dpto, .data$ano == year
        )
    } else {
      poblacion_incidencia <- dplyr::filter(
        data_incidencia,
        .data$cod_dpto == dpto
      )
      total_poblacion <-
        sum(poblacion_incidencia[[paste0("poblacion_riesgo_", year)]])
    }
    if (!is.null(mpio)) {
      if (poblacion == "proyecciones") {
        poblacion_incidencia <-
          poblacion_incidencia[poblacion_incidencia$mpio == mpio, ]
      } else {
        poblacion_incidencia <-
          poblacion_incidencia[poblacion_incidencia$cod_mpio == mpio, ]
        total_poblacion <-
          sum(poblacion_incidencia[[paste0("poblacion_riesgo_", year)]])
      }
      if (is.null(sex)) {
        data_agrupada <- data_agrupada[data_agrupada[[nomb_cols[3]]] == mpio, ]
      }
    } else if (is.null(sex)) {
      data_agrupada <- data_agrupada[data_agrupada[[nomb_cols[1]]] == dpto, ]
    }
  } else {
    if (poblacion == "proyecciones") {
      poblacion_incidencia <-
        dplyr::filter(
          data_incidencia,
          .data$area_geografica == "Total",
          .data$ano == year
        )
    } else {
      total_poblacion <-
        sum(poblacion_incidencia[[paste0("poblacion_riesgo_", year)]])
    }
  }
  if (!is.null(sex)) {
    if (sex == "F") {
      total_poblacion <- sum(poblacion_incidencia$mujeres)
    } else {
      total_poblacion <- sum(poblacion_incidencia$hombres)
    }
  } else if (poblacion == "proyecciones") {
    total_poblacion <- sum(poblacion_incidencia$total)
  }
  total_casos <- sum(data_agrupada$casos)
  if (total_poblacion > 0) {
    incidencia <- round(
      (total_casos / total_poblacion) *
        coeficiente,
      2
    )
  }
  return(incidencia)
}

#' @title Calcular incidencia según distribución geográfica
#' @description Función que calcula la incidencia de una enfermedad o evento
#' para todos los departamentos de Colombia o los municipios de un departamento.
#' @param data_incidencia Un `data.frame` que contiene las proyecciones
#' poblacionales del DANE; su valor por defecto es `NULL`.
#' @param data_agrupada Un `data.frame` que contiene los datos de la enfermedad
#' agrupados por departamento o municipio y número de casos.
#' @param poblacion Un `character` (cadena de caracteres) con el tipo de
#' población para efectuar el calculo de la incidencia. Indica si se
#' desea utilizar la población a riesgo del evento `"riesgo"` o las
#' proyecciones poblacionales DANE `"proyecciones"`; su valor por defecto
#' es `NULL`.
#' @param year Un `numeric` (numerico) con el año que se debe tomar de las
#' proyecciones poblacionales.
#' @return Un `data.frame` con el calculo de la incidencia para todos los
#' departamentos de Colombia o los municipios de un departamento.
#' @examples
#' \donttest{
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' data_agrupada_mpios <- agrupar_mpio(data_limpia, dpto = "Antioquia")
#' # Cálculo de la incidencia con población a riesgo por departamento
#' calcular_incidencia_geo(
#'   poblacion = "riesgo",
#'   data_agrupada = data_agrupada_mpios,
#'   year = 2020
#' )
#' data_agrupada_dptos <- agrupar_dpto(data_limpia)
#' # Cálculo de la incidencia con proyecciones poblacionales para Colombia
#' calcular_incidencia_geo(
#'   poblacion = "proyecciones",
#'   data_agrupada = data_agrupada_dptos,
#'   year = 2020
#' )
#' }
#' @export
calcular_incidencia_geo <- function(data_incidencia = NULL,
                                    data_agrupada,
                                    poblacion = NULL,
                                    year = NULL) {
  validar_data_agrupada(data_agrupada)
  data_geo_incidencia <- NULL
  nombre_evento <- data_agrupada$nombre_evento[1]
  cod_evento <- data_agrupada$cod_eve[1]
  if (is.null(year)) {
    year <- as.numeric(obtener_year(data_agrupada))
  }
  if (is.null(poblacion)) {
    vals_event <-
      obtener_cond_inciden_event(cod_eve = cod_evento)
    poblacion <- vals_event$denominador
  }
  pop_incidencia <-
    obtener_pob_incidencia(
      data_incidencia = data_incidencia,
      poblacion = poblacion,
      event = nombre_evento,
      year = year
    )
  data_incidencia <- pop_incidencia$data_incidencia
  poblacion <- pop_incidencia$poblacion
  nomb_cols <- obtener_tip_ocurren_geo(nombre_evento)
  if (nomb_cols[1] %in% colnames(data_agrupada) &&
    !(nomb_cols[3] %in% colnames(data_agrupada))) {
    data_agrupada <- group_by(
      data_agrupada,
      dplyr::across(dplyr::all_of(nomb_cols[1:2]))
    )
    data_agrupada <- dplyr::summarise(data_agrupada,
      casos =
        sum(.data[["casos"]]),
      .groups = "drop"
    )
    data_agrupada$nombre_evento <- nombre_evento
    data_agrupada$cod_eve <- cod_evento

    geo_incidencia <- rep_len(NA_real_, nrow(data_agrupada))
    for (fila in seq_along(geo_incidencia)) {
      dpto_fila <- data_agrupada[fila, ]
      incidencia <- calcular_incidencia(
        data_incidencia = data_incidencia,
        data_agrupada = dpto_fila,
        poblacion = poblacion,
        dpto = dpto_fila[[nomb_cols[1]]],
        year = year
      )
      geo_incidencia[fila] <- incidencia
    }
    data_geo_incidencia <- cbind(data_agrupada, geo_incidencia)
  } else if (nomb_cols[3] %in% colnames(data_agrupada)) {
    data_agrupada <- group_by(
      data_agrupada,
      dplyr::across(dplyr::all_of(nomb_cols[1:4]))
    )
    data_agrupada <- dplyr::summarise(data_agrupada,
      casos =
        sum(.data[["casos"]]),
      .groups = "drop"
    )
    data_agrupada$nombre_evento <- nombre_evento
    data_agrupada$cod_eve <- cod_evento

    geo_incidencia <- rep_len(NA_real_, nrow(data_agrupada))
    for (fila in seq_along(geo_incidencia)) {
      mpio_fila <- data_agrupada[fila, ]
      incidencia <- calcular_incidencia(
        data_incidencia = data_incidencia,
        data_agrupada = mpio_fila,
        poblacion = poblacion,
        dpto = mpio_fila[[nomb_cols[1]]],
        mpio = mpio_fila[[nomb_cols[3]]],
        year = year
      )
      geo_incidencia[fila] <- incidencia
    }
    data_geo_incidencia <- cbind(data_agrupada, geo_incidencia)
  }
  return(data_geo_incidencia)
}

#' @title Calcular incidencia por sexo
#' @description Función que calcula la incidencia de una enfermedad o evento
#' para todos los departamentos de Colombia o los municipios de un departamento
#' por cada sexo.
#' @param data_incidencia Un `data.frame` que contiene la proyecciones
#' poblacionales del DANE; su valor por defecto es `NULL`.
#' @param data_agrupada Un `data.frame` que contiene los datos de la enfermedad
#' agrupados por departamento o municipio y número de casos.
#' @param year Un `numeric` (numerico) con el año que se debe tomar de las
#' proyecciones poblacionales; valor por defecto es `NULL`.
#' @param dpto Un `character` (cadena de caracteres) o `numeric` (numérico)
#' que contiene el código o nombre del departamento; su valor por
#' defecto es `NULL`.
#' @param mpio Un `character` (cadena de caracteres) o `numeric` (numérico)
#' que contiene el código o nombre del municipio; su valor por defecto
#' es `NULL`.
#' @return Un `data.frame` con el calculo de la incidencia para todos los
#' departamentos de Colombia o los municipios de un departamento.
#' @examples
#' \donttest{
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' # Cálculo de la incidencia con proyecciones poblacionales por sexo y
#' # departamento
#' data_filtrada <- geo_filtro(
#'   data_event = data_limpia,
#'   dpto = "05"
#' )
#' data_agrupada <- agrupar_sex(data_filtrada)
#' calcular_incidencia_sex(
#'   data_agrupada = data_agrupada,
#'   dpto = "05",
#'   year = 2020
#' )
#' #' Cálculo de la incidencia con proyecciones poblacionales por sexo y
#' # municipio
#' data_filtrada <- geo_filtro(
#'   data_event = data_limpia,
#'   dpto = "05",
#'   mpio = "Medellin"
#' )
#' calcular_incidencia_sex(
#'   data_agrupada = data_agrupada,
#'   dpto = "05",
#'   mpio = "Medellin"
#' )
#' }
#' @export
calcular_incidencia_sex <- function(data_incidencia = NULL,
                                    data_agrupada,
                                    year = NULL, dpto = NULL,
                                    mpio = NULL) {
  validar_data_agrupada(data_agrupada)
  dept_data <- NULL
  nombre_evento <- data_agrupada$nombre_evento[1]
  if (is.null(year)) {
    year <- as.numeric(obtener_year(data_agrupada))
  }
  if (is.null(data_incidencia)) {
    data_incidencia <-
      import_pob_incidencia(poblacion = "proyecciones", year = year)
    message(
      "Las incidencias se calcularon con las proyecciones ",
      "poblacionales DANE. Si usted cuenta con la poblacion ",
      "a riesgo definida por el Ministerio de Salud para el ",
      year, " puede hacer uso de ella, asignandola en el ",
      "argumento data_incidencia de la funcion"
    )
  } else {
    validar_data_incidencia(data_incidencia)
  }
  if (!is.null(dpto)) {
    stopifnot(
      "El parametro dpto debe ser una cadena de caracteres
               o numerico" =
        (is.numeric(dpto) && !is.character(dpto)) ||
          (!is.numeric(dpto) && is.character(dpto))
    )
    dept_data <- obtener_info_depts(dpto, mpio)
    stopifnot(
      "El departamento o municipio ingresado no existe" =
        seq_len(nrow(dept_data)) > 0
    )
    dept_data <- dept_data[1, ]
    dpto <- dept_data$codigo_departamento
    if (!is.null(mpio)) {
      mpio <- dept_data$codigo_municipio
    }
  }
  cod_eve <- data_agrupada$cod_eve[1]
  data_agrupada <- group_by(
    data_agrupada,
    "sexo"
  )
  data_agrupada <- dplyr::summarise(data_agrupada,
    casos = sum(.data[["casos"]]),
    .groups = "drop"
  )
  data_agrupada$cod_eve <- cod_eve
  data_agrupada$nombre_evento <- nombre_evento

  incidencia <- rep_len(NA_real_, nrow(data_agrupada))
  for (fila in seq_along(incidencia)) {
    sex_fila <- data_agrupada[fila, ]
    incidencia_sex <- calcular_incidencia(
      data_incidencia = data_incidencia,
      data_agrupada = sex_fila,
      poblacion = "proyecciones",
      dpto = dpto,
      mpio = mpio,
      sex = sex_fila[["sexo"]],
      year = year
    )
    incidencia[fila] <- incidencia_sex
  }
  data_incidencia_sex <- cbind(data_agrupada, incidencia)
  if (!is.null(dpto) && is.null(data_incidencia_sex)) {
    data_incidencia_sex$nombre_departamento <- dept_data$nombre_departamento[1]
    data_incidencia_sex$codigo_departamento <- dpto
    if (!is.null(mpio)) {
      data_incidencia_sex$nombre_municipio <- dept_data$nombre_municipio[1]
      data_incidencia_sex$codigo_municipio <- mpio
    }
  }
  return(data_incidencia_sex)
}
