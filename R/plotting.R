#' Plot de las semanas epidemiológicas
#'
#' Función que genera el gráfico de las semanas epidemiológicas
#' @param data Un data frame con los datos de la enfermedad
#' o evento
#' @param col_semana Un data frame con el nombre de la columna
#' de los datos de la enfermedad o evento que contiene las semanas
#' epidemiológicas
#' @param col_casos Un character (cadena de caracteres) que contiene
#' el nombre de la columna de los datos de la enfermedad o evento
#' que contiene el número de casos
#' @param year Un numeric (numerico) que contiene el año de los datos
#' de la enfermedad o evento
#' @param tipo Un character (cadena de caracteres) que contiene la unidad de
#' tiempo para el eje x ("week": semana y "date": fecha)
#' @param etiqueta_x Un character (cadena de caracteres) que contiene la
#' etiqueta del eje x
#' @param etiqueta_y Un character (cadena de caracteres) que contiene la
#' etiqueta del eje y
#' @return Un plot o gráfico de las semanas epidemiológicas
#' @examples
#' data_resumida_sivigila <- import_data_resumen_sivigila()
#' data_resumida_sivigila <- limpiar_encabezado(data_resumida_sivigila)
#' data_filtrada <- filtrar_event("MALAR", data_resumida_sivigila)
#' plot_semanaepi(data = data_filtrada,
#'                col_semana = "semana",
#'                col_casos = "casos",
#'                year = 2019,
#'                tipo = "date",
#'                etiqueta_x = "Fecha de semana epidemiologica")
#' @export
plot_semanaepi <- function(data,
                           col_semana,
                           col_casos,
                           year,
                           tipo = "week",
                           etiqueta_x = "Semana epidemiologica",
                           etiqueta_y = "Numero de casos por semana") {
  data$epiweek <- data[, col_semana]
  data$numero_casos <- data[, col_casos]
  data_plot <- data %>%
    dplyr::group_by(.data$epiweek, .data$nombre) %>%
    dplyr::summarise(casos = sum(.data$numero_casos), .groups = "drop")
  if (tipo == "week") {
    plot <- ggplot2::ggplot(data_plot) +
      ggplot2::geom_col(ggplot2::aes(x = .data$epiweek,
                                     y = .data$casos,
                                     fill = .data$nombre), alpha = 0.9) +
      ggplot2::theme_classic() +
      ggplot2::xlab(etiqueta_x) +
      ggplot2::ylab(etiqueta_y) +
      ggplot2::scale_fill_discrete(name = "") +
      ggplot2::theme(legend.position = "bottom")
  }
  if (tipo == "date") {
    data_plot$date_week <- as.Date(paste(year,
                                         data_plot$epiweek, 1, sep = "-"),
                                   "%Y-%U-%u")
    plot <- ggplot2::ggplot(data_plot) +
      ggplot2::geom_col(ggplot2::aes(x = .data$date_week,
                                     y = .data$casos,
                                     fill = .data$nombre), alpha = 0.9) +
      ggplot2::theme_classic() +
      ggplot2::xlab(etiqueta_x) +
      ggplot2::ylab(etiqueta_y) +
      ggplot2::scale_fill_discrete(name = "") +
      ggplot2::theme(legend.position = "bottom")
  }
  return(plot)
}

#' Generar mapa por departamento
#'
#' Función que genera el mapa por departamento con el número de casos
#' de una enfermedad o evento
#' @param data_agrupada Un data frame que contiene los datos de la enfermedad
#' o evento agrupados por departamento y número de casos
#' @param col_nombre_lj Un character (cadena de caracteres) que contiene el
#' nombre de la columna para unir con el archivo de forma (shape file)
#' @param fuente_data Un character (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos de la enfermedad o evento
#' @return Un plot con el mapa por departamento con el número de casos de
#' la enfermedad o evento
#' @examples
#' data_event <- import_data_event(2019, "DENGUE")
#' data_event <- limpiar_encabezado(data_event)
#' data_event <- estandarizar_geo_cods(data_event)
#' data_dpto <- agrupar_dpto(data_event)
#' plot_map_dpto(data_dpto,
#'    col_nombre_lj = "id",
#'    fuente_data = "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia")
#' @export
plot_map_dpto <- function(data_agrupada,
                          col_nombre_lj = "id",
                          fuente_data = NULL) {
  if (is.null(fuente_data)) {
    fuente_data <- "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  dsn <-  system.file("extdata/depto_adm_shp", "depto.shp",
                      package = "sivirep")
  shp <- sf::st_read(dsn = dsn)
  colnames(shp)[colnames(shp) == "DPTO"] <- "id"
  shp <- ggplot2::fortify(shp, region = "id")
  shp <- shp %>%
    dplyr::left_join(data_agrupada, by = col_nombre_lj)
  shp <- cbind(shp, sf::st_coordinates(sf::st_centroid(shp$geometry)))
  map <- ggplot2::ggplot() +
    ggplot2::geom_sf() +
    ggplot2::geom_sf(data = shp, ggplot2::aes(fill = .data$casos)) +
    ggplot2::scale_fill_gradient(low = "white", high = "darkred") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::labs(caption = fuente_data)
  return(map)
}

#' Generar mapa por departamento
#'
#' Función que genera el mapa por departamentos o municipios con el número de
#' casos de una enfermedad o evento
#' @param data_agrupada Un data frame que contiene los datos de la enfermedad
#' agrupados por departamento y número de casos
#' @param col_nombre_lj Un character (cadena de caracteres) que contiene el
#' nombre de la columna para unir con el archivo de forma (shape file)
#' @param fuente_data Un character (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos de la enfermedad o evento
#' @param dpto Un character (cadena de caracteres) que contiene el
#' nombre del departamento
#' @param munpio Un character (cadena de caracteres) que contiene el
#' nombre del municipio
#' @return El plot o mapa por departamentos o municipios con el número de
#' casos de una enfermedad específica
#' @examples
#' data_event <- import_data_event(2019, "DENGUE")
#' data_event <- limpiar_encabezado(data_event)
#' data_espacial_dpto <- estandarizar_geo_cods(data_event)
#' data_espacial_dpto <- geo_filtro(data_event = data_event,
#'                                  nombre_dpto = "Antioquia")
#' data_espacial_dpto <- agrupar_mun(data_event, dept_nombre = "Antioquia")
#' plot_map(data_agrupada = data_espacial_dpto,
#'    col_nombre_lj = "id",
#'    fuente_data = "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia",
#'    dpto = "Antioquia",
#'    munpio = "Envigado")
#' @export
plot_map <- function(data_agrupada,
                     col_nombre_lj = "id",
                     fuente_data = NULL,
                     dpto = NULL,
                     munpio = NULL) {
  if (is.null(fuente_data)) {
    fuente_data <- "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  dsn <-  system.file("extdata/depto_adm_shp", "MGN_ANM_MPIOS.shp",
                      package = "sivirep")
  shp <- sf::st_read(dsn = dsn)
  data_dept <- obtener_info_depts(dpto, munpio)
  data_dept <- data_dept[1, ]
  polygon_seleccionado <- shp
  if (!is.null(dpto)) {
    polygon_seleccionado <- shp[shp$DPTO_CCDGO ==
                                  data_dept$codigo_departamento, ]
    if (!is.null(munpio)) {
      code_mun <- modficar_cod_mun(data_dept$codigo_departamento,
                                   data_dept$codigo_municipio)
      polygon_seleccionado <-
        polygon_seleccionado[polygon_seleccionado$MPIO_CCDGO == code_mun, ]
    }
    colnames(polygon_seleccionado)[colnames(polygon_seleccionado) ==
                                     "MPIO_CCDGO"] <- "id"
  } else {
    colnames(polygon_seleccionado)[colnames(polygon_seleccionado) ==
                                     "DPTO_CCDGO"] <- "id"
  }
  polygon_seleccionado <- ggplot2::fortify(polygon_seleccionado, region = "id")
  polygon_seleccionado <- polygon_seleccionado %>%
    dplyr::left_join(data_agrupada, by = col_nombre_lj)
  polygon_seleccionado <-
    cbind(polygon_seleccionado,
          sf::st_coordinates(sf::st_centroid(polygon_seleccionado$geometry)))
  map <- ggplot2::ggplot() +
    ggplot2::geom_sf() +
    ggplot2::geom_sf(data = polygon_seleccionado,
                     ggplot2::aes(fill = .data$casos)) +
    ggplot2::scale_fill_gradient(low = "white", high = "darkred") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::labs(caption = fuente_data, fill = "Casos")
  return(map)
}

#' Generar gráfico por variable(s) o columna(s)
#'
#' Función que genera un gráfico por cualquier tipo de variable o columna de un
#' data frame
#' @param data Un data frame con los datos a graficar
#' @param var_x Un character (cadena de caracteres) que contiene el nombre de
#' la variable para el eje x
#' @param var_y Un character (cadena de caracteres) que contiene el nombre de
#' la variable para el eje y
#' @param var_por Un character (cadena de caracteres) que contiene el nombre
#' de la variable si los datos tienen porcentajes; su valor por defecto es NULL
#' @param var_fill Un character (cadena de caracteres) que contiene la
#' variable de llenado; su valor por defecto es NULL
#' @param agr_per Un boolean (TRUE/FALSE) que indica si los datos
#' tienen porcentajes; su valor por defecto es TRUE
#' @param etiqueta_x Un character (cadena de caracteres) que contiene
#' la etiqueta para el eje x
#' @param etiqueta_y Un character (cadena de caracteres) que contiene
#' la etiqueta para el eje y
#' @param nombre_escala Un character (cadena de caracteres) que contiene
#' el nombre de la escala; su valor por defecto es NULL
#' @param etiquetas_escala Un array (arreglo) de character (cadena de
#' caracteres) que contiene las etiquetas de la escala
#' @param tit_diagram Un character (cadena de caracteres) que contiene
#' el título del diagrama; su valor por defecto es NULL
#' @param pos_leyenda Un character (cadena de caracteres) que contiene la
#' posición de la leyenda
#' @param ancho_barra Un numeric (numerico) que contiene el ancho de las
#' barras; su valor por defecto es 1
#' @param tam_text Un numeric (numerico) que contiene el tamaño del texto;
#' su valor por defecto es 3
#' @param most_val Un boolean (TRUE/FALSE) que indica si las barras deben
#' mostrar los valores; su valor por defecto es TRUE
#' @param fuente_data Un character (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es NULL
#' @return Un plot o gráfico por variable(s) o columna(s)
#' @examples
#' data_event <- import_data_event(2019, "DENGUE")
#' data_event <- limpiar_encabezado(data_event)
#' cases_sex <- agrupar_sex(data_event,
#'                          porcentaje = TRUE)
#' plot_variable(data = cases_sex,
#'   var_x = "sexo",
#'   var_y = "casos",
#'   var_fill = "sexo",
#'   var_por = "porcentaje",
#'   etiqueta_x = "Sexo",
#'   etiqueta_y = "Numero de casos",
#'   nombre_escala = "Sexo",
#'   etiquetas_escala = c("Femenino", "Masculino"),
#'   pos_leyenda = "right",
#'   ancho_barra = 0.5,
#'   tam_text = 3,
#'   most_val = TRUE
#'   )
#' @export
plot_variable <- function(data, var_x, var_y, var_por = NULL,
                          var_fill = NULL, agr_per = TRUE, etiqueta_x,
                          etiqueta_y, nombre_escala = NULL,
                          etiquetas_escala = NULL,
                          tit_diagram = NULL, pos_leyenda, ancho_barra = 1,
                          tam_text = 3, most_val = TRUE, fuente_data = NULL) {
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  ggplot2::ggplot(data, {
    if (is.null(var_fill)) ggplot2::aes_string(x = var_x, y = var_y)
    else ggplot2::aes_string(x = var_x, y = var_y, fill = var_fill)
  }) + {
    if (is.null(var_fill)) {
      ggplot2::geom_bar(width = ancho_barra, stat = "identity",
                        position = ggplot2::position_dodge(),
                        fill = "#90C73D")
    } else {
      ggplot2::geom_bar(width = ancho_barra,
                        stat = "identity",
                        position = ggplot2::position_dodge())
    }
  } +
    ggplot2::labs(x = etiqueta_x, y = etiqueta_y, caption = fuente_data) +
    ggplot2::labs(fill = "") + {
    if (var_y == "casos") {
      ggplot2::scale_y_continuous(limits = c(0, max(data$casos)))
    } else {
      ggplot2::scale_x_continuous(limits = c(0, max(data$casos)))
    }
  } +
    ggplot2::theme_classic() + {
    if (tam_text > 3) {
      ggplot2::theme(text = ggplot2::element_text(size = tam_text * 2))
    }
  } +
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 8)) + {
    if (most_val) {
      ggplot2::geom_text({
        if (!is.null(var_por)) {
          eval(parse(text = paste0("ggplot2::aes(label = paste0(",
                                   var_y, ", '\n (' ,",
                                   var_por, ", '%', ')'", "))")))
        } else {
          eval(parse(text = paste0("ggplot2::aes(label = ", var_y, ")")))
        }
      },
      vjust = 1.3,
      color = "black",
      hjust = 0.5,
      position = ggplot2::position_dodge(0.9),
      angle = 0,
      size = tam_text,
      )
    }
  } +
    ggplot2::theme(legend.position = pos_leyenda) + {
    if (ncol(data) == 3 || (!is.null(var_fill) && var_fill == "sexo"))
      ggplot2::scale_fill_manual(values = c("#56B4E9", "#E69F00"))
    else ggplot2::theme(legend.position = pos_leyenda)
  } + {
    if (length(unique(data$nombre_evento)) > 1)
      ggplot2::facet_grid(~nombre_evento, scales = "free")
  }
}

#' Generar gráfico de distribución de casos por fecha de inicio de síntomas
#'
#' Función que genera el gráfico de distribución de casos
#' por fecha de inicio de síntomas
#' @param data_agrupada Un data frame que contiene los datos de la enfermedad
#' o evento agrupados
#' @param uni_marca Un character (cadena de caracteres) que contiene la unidad
#' de las marcas del gráfico ("day": día, "month": mes y "year": año);
#' su valor por defecto es "month"
#' @param col_nombre Un character (cadena de caracteres) que contiene el
#' nombre de la columna en los datos de la enfermedad o evento
#' agrupados que contiene las fechas de inicio de síntomas; su valor por
#' defecto es "ini_sin"
#' @return Un plot o gráfico de la distribución de casos por fecha de inicio
#' de síntomas
#' @examples
#' data_event <- import_data_event(2020, "DENGUE")
#' data_event <- limpiar_encabezado(data_event)
#' data_agrupada <- agrupar_fecha_inisintomas(
#'                                      data_event,
#'                                      col_nombre = "ini_sin",
#'                                      tipo = "month")
#' plot_fecha_inisintomas(data_agrupada = data_agrupada,
#'                        col_nombre = "ini_sin",
#'                        uni_marca = "month")
#' @export
plot_fecha_inisintomas <- function(data_agrupada,
                                   col_nombre = "ini_sin",
                                   uni_marca = "month") {
  fechas_column_nombres <- config::get(file = system.file("extdata",
                                                          "config.yml",
                                                          package = "sivirep"),
                                       "dates_column_names")
  if (is.null(col_nombre)) {
    col_nombre <- fechas_column_nombres[3]
  }
  plot_casos_inisintomas <- plot_variable(data_agrupada,
                                          var_x = col_nombre,
                                          var_y = "casos",
                                          etiqueta_x =
                                            "\nFecha de inicio de sintomas\n",
                                          etiqueta_y =
                                            "Numero de casos por dia\n",
                                          pos_leyenda = "right",
                                          most_val = FALSE) +
    ggplot2::scale_x_date(
      date_breaks = paste0("1 ", uni_marca),
      date_labels = "%b"
    )
  return(plot_casos_inisintomas)
}

#' Generar gráfico de distribución de casos por fecha de notificación
#'
#' Función que genera el gráfico de distribución de casos por
#' fecha de notificación
#' @param data_agrupada Un data frame que contiene los datos de la
#' enfermedad o evento agrupados
#' @param uni_marca Un character (cadena de caracteres) que contiene la unidad
#' de las marcas del gráfico ("day": día, "month": mes y "year": año); su
#' valor por defecto es "month"
#' @param col_nombre Un character (cadena de caracteres) que contiene el
#' nombre de la columna en los datos de la enfermedad o evento agrupados que
#' contiene las fechas de notificación; su valor por defecto es "fec_not"
#' @return Un plot o gráfico de distribución de casos por fecha de notificación
#' @examples
#' data_event <- import_data_event(2020, "DENGUE")
#' data_event <- limpiar_encabezado(data_event)
#' data_agrupada <- agrupar_fecha_notifica(data_event,
#'                                         col_nombre = "fec_not",
#'                                         tipo = "month")
#' plot_fecha_notifica(data_agrupada = data_agrupada,
#'                     col_nombre = "fec_not",
#'                     uni_marca = "month")
#' @export
plot_fecha_notifica <- function(data_agrupada,
                                col_nombre = "fec_not",
                                uni_marca = "month") {
  fechas_column_nombres <- config::get(file =
                                         system.file("extdata",
                                                     "config.yml",
                                                     package = "sivirep"),
                                       "dates_column_names")
  if (is.null(col_nombre)) {
    col_nombre <- fechas_column_nombres[2]
  }
  plot_casos_fecha_notifica <- plot_variable(data_agrupada,
                                             var_x = col_nombre,
                                             var_y = "casos",
                                             etiqueta_x =
                                               "\nFecha de notificacion\n",
                                             etiqueta_y =
                                               "Numero de casos por dia\n  ",
                                             pos_leyenda = "right",
                                             most_val = FALSE) +
    ggplot2::scale_x_date(
      date_breaks = paste0("1 ", uni_marca),
      date_labels = "%b"
    )
  return(plot_casos_fecha_notifica)
}

#' Generar gráfico de distribución de casos por sexo
#'
#' Función que genera el gráfico de distribución de casos por sexo
#' @param data_agrupada Un data fram que contiene los datos de la
#' enfermedad o evento agrupados
#' @param col_nombre Un character (cadena de caracteres) con el
#' nombre de la columna de los datos agrupados de la enfermedad
#' o evento que contiene el sexo; su valor por defecto es "sexo"
#' @param porcentaje Un boolean (TRUE/FALSE) que indica si los datos
#' tienen porcentajes; su valor por defecto es TRUE
#' @return Un plot o gráfico de distribución de casos por sexo
#' @examples
#' data_event <- import_data_event(2020, "DENGUE")
#' data_event <- limpiar_encabezado(data_event)
#' data_agrupada <- agrupar_sex(data_event,
#'                              col_nombre = "sexo",
#'                              porcentaje = TRUE)
#' plot_sex(data_agrupada = data_agrupada,
#'          col_nombre = "sexo",
#'          porcentaje = TRUE)
#' @export
plot_sex <- function(data_agrupada,
                     col_nombre = "sexo",
                     porcentaje = TRUE) {
  var_fill <- col_nombre
  plot_casos_sex <- plot_variable(data_agrupada,
                                  var_x = col_nombre,
                                  var_y = "casos",
                                  var_fill = var_fill,
                                  var_por = "porcentaje",
                                  etiqueta_x = "\nSexo\n",
                                  etiqueta_y = "Numero de casos\n",
                                  nombre_escala = "Sexo",
                                  etiquetas_escala = c("Femenino", "Masculino"),
                                  pos_leyenda = "right",
                                  ancho_barra = 0.5,
                                  tam_text = 3,
                                  most_val = porcentaje)
  return(plot_casos_sex)
}

#' Generar gráfico de distribución de casos por sexo y semana epidemiológica
#'
#' Función que genera el gráfico de distribución de casos por sexo
#' y semana epidemiológica
#' @param data_agrupada Un data frame que contiene los datos de la enfermedad
#' o evento agrupados
#' @param col_nombres Un array (arreglo) de character (cadena de caracteres) con
#' los nombres de columna de los datos agrupados de la enfermedad o evento que
#' contienen el sexo y las semanas epidemiológicas; su valor por defecto es
#' c("sexo", "semana")
#' @param porcentaje Un boolean (TRUE/FALSE) que indica si los datos tienen
#' porcentajes; su valor por defecto es TRUE
#' @return Un plot o gráfico de distribución de casos por sexo y semana
#' epidemiológica
#' @examples
#' data_event <- import_data_event(2020, "DENGUE")
#' data_event <- limpiar_encabezado(data_event)
#' data_agrupada <- agrupar_sex_semanaepi(data_event,
#'                                        col_nombres = c("sexo", "semana"),
#'                                        porcentaje = TRUE)
#' plot_sex_semanaepi(data_agrupada = data_agrupada,
#'                    col_nombres = c("sexo", "semana"),
#'                    porcentaje = FALSE)
#' @export
plot_sex_semanaepi <- function(data_agrupada,
                               col_nombres = c("sexo", "semana"),
                               porcentaje = FALSE) {
  plot_casos_sex_semanaepi <- plot_variable(data_agrupada,
                                            var_x = col_nombres[2],
                                            var_y = "casos",
                                            var_fill = col_nombres[1],
                                            var_por = "porcentaje",
                                            etiqueta_x =
                                              "\nSemana epidemiologica\n",
                                            etiqueta_y =
                                              "Numero de casos\n",
                                            nombre_escala = "Sexo",
                                            etiquetas_escala =
                                              c("Femenino", "Masculino"),
                                            pos_leyenda = "right",
                                            ancho_barra = 0.5,
                                            tam_text = 3,
                                            most_val = porcentaje)
  return(plot_casos_sex_semanaepi)
}

#' Generar gráfico de distribución de casos por edad
#'
#' Función que genera el gráfico de distribución de casos por edad
#' @param data_agrupada Un data frame que contiene los datos de la enfermedad
#' o evento agrupados
#' @param col_nombre Un character (cadena de carácteres) con el nombre de
#' la columna de los datos agrupados de la enfermedad o evento que contiene
#' las edades; su valor por defecto es "edad"
#' @param porcentaje Un boolean (TRUE/FALSE) que indica si los datos tienen
#' porcentajes; su valor por defecto es FALSE
#' @return Un plot o gráfico de distribución de casos por edad
#' @examples
#' data_event <- import_data_event(2020, "DENGUE")
#' data_event <- limpiar_encabezado(data_event)
#' data_agrupada <- agrupar_edad(data_event,
#'                               col_nombre = "edad",
#'                               porcentaje = FALSE)
#' plot_edad(data_agrupada = data_agrupada,
#'           col_nombre = "edad",
#'           porcentaje = FALSE)
#' @export
plot_edad <- function(data_agrupada,
                      col_nombre = "edad",
                      porcentaje = FALSE) {
  plot_casos_edad <- plot_variable(data_agrupada,
                                   var_x = col_nombre,
                                   var_y = "casos",
                                   etiqueta_x = "\nEdad\n",
                                   etiqueta_y = "Numero de casos\n",
                                   nombre_escala = "Edad",
                                   pos_leyenda = "right",
                                   ancho_barra = 0.7,
                                   tam_text = 2.5,
                                   most_val = porcentaje)
  return(plot_casos_edad)
}

#' Generar gráfico de distribución de casos por edad y sexo
#'
#' Función que genera el gráfico de distribución de casos por edad y sexo
#' @param data_agrupada Un data frame que contiene los datos de la
#' enfermedad o evento agrupados
#' @param col_nombres Un array (arreglo) de character (cadena de caracteres) con
#' los nombres de columna de los datos agrupados de la enfermedad o evento que
#' contiene las edades y las semanas epidemiológicas; su valor por defecto
#' es c("edad", "sexo")
#' @param porcentaje Un boolean (TRUE/FALSE) que indica si los datos tienen
#' porcentajes; su valor por defecto es FALSE
#' @return Un plot o gráfico de distribución de casos por edad y sexo
#' @examples
#' data_event <- import_data_event(2020, "DENGUE")
#' data_event <- limpiar_encabezado(data_event)
#' data_agrupada <- agrupar_edad_sex(data_event,
#'                                   col_nombres = c("edad", "sexo"),
#'                                   porcentaje = FALSE)
#' plot_edad_sex(data_agrupada = data_agrupada,
#'               col_nombres = c("edad", "sexo"),
#'               porcentaje = FALSE)
#' @export
plot_edad_sex <- function(data_agrupada,
                          col_nombres = c("edad", "sexo"),
                          porcentaje = FALSE) {
  plot_casos_edad_sexo <- plot_variable(data_agrupada,
                                        var_x = col_nombres[1],
                                        var_y = "casos",
                                        var_fill = col_nombres[2],
                                        etiqueta_x = "\nEdad\n",
                                        etiqueta_y = "Numero de casos\n",
                                        nombre_escala = "Edad",
                                        pos_leyenda = "right",
                                        ancho_barra = 0.7,
                                        tam_text = 3,
                                        most_val = porcentaje)
  return(plot_casos_edad_sexo)
}

#' Generar gráfico de distribución de casos por población especial
#'
#' Función que genera el gráfico de distribución de casos por población especial
#' @param data_agrupada Un data frame que contiene los datos de la
#' enfermedad o evento agrupados
#' @param col_nombre Un character (cadena de carácteres) con el nombre de
#' la columna de los datos agrupados de la enfermedad o evento que contiene
#' las poblaciones especiales; su valor por defecto es "poblacion"
#' @param porcentaje Un boolean (TRUE/FALSE) que indica si los datos tienen
#' porcentajes; su valor por defecto es FALSE
#' @return Un plot o gráfico de distribución de casos por población especial
#' @examples
#' data_event <- import_data_event(2020, "DENGUE")
#' data_event <- limpiar_encabezado(data_event)
#' data_agrupada <- agrupar_pob_especial(data_event,
#'                          col_nombre = "poblacion",
#'                          porcentaje = TRUE)
#' plot_pob_especial(data_agrupada = data_agrupada,
#'                   col_nombre = "poblacion",
#'                   porcentaje = FALSE)
#' @export
plot_pob_especial <- function(data_agrupada,
                              col_nombre = "poblacion",
                              porcentaje = FALSE) {
  plot_casos_especial_pob <- plot_variable(data_agrupada,
                                           var_x = col_nombre,
                                           var_y = "casos",
                                           var_fill = col_nombre,
                                           etiqueta_x = "Poblacion",
                                           etiqueta_y = "casos",
                                           nombre_escala = "Poblacion",
                                           pos_leyenda = "right",
                                           ancho_barra = 0.5,
                                           tam_text = 3,
                                           most_val = porcentaje) +
    ggplot2::theme(legend.position = "bottom")
  return(plot_casos_especial_pob)
}

#' Generar gráfico de distribución de casos por municipios
#'
#' Función que genera el gráfico de distribución de casos por municipios
#' @param data_agrupada Un data frame que contiene los datos de la
#' enfermedad o evento agrupados
#' @param col_nombre Un character (cadena de carácteres) con el nombre de
#' la columna de los datos agrupados de la enfermedad o evento que contiene
#' los municipios; su valor por defecto es "nombre"
#' @param porcentaje Un boolean (TRUE/FALSE) que indica si los datos tienen
#' porcentajes; su valor por defecto es FALSE
#' @return Un plot o gráfico de distribución de casos por municipios
#' @examples
#' data_event <- import_data_event(2020, "DENGUE")
#' data_event <- limpiar_encabezado(data_event)
#' data_event <- estandarizar_geo_cods(data_event)
#' data_agrupada <- agrupar_mun(data_event,
#'                              dept_nombre = "Antioquia")
#' plot_muns(data_agrupada,
#'           col_nombre = "nombre",
#'           porcentaje = FALSE)
#' @export
plot_muns <- function(data_agrupada,
                      col_nombre = "nombre",
                      porcentaje = FALSE) {
  plot_casos_muns <- plot_variable(data_agrupada,
                                   var_x = col_nombre,
                                   var_y = "casos",
                                   etiqueta_x = "Municipios",
                                   etiqueta_y = "casos",
                                   nombre_escala = "Municipios",
                                   pos_leyenda = "right",
                                   ancho_barra = 1,
                                   tam_text = 3,
                                   most_val = porcentaje) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::coord_flip()
  return(plot_casos_muns)
}
