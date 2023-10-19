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
#' \dontrun{
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' data_event <- estandarizar_geo_cods(data_event)
#' data_dpto <- agrupar_dpto(data_event)
#' plot_map_dpto(data_dpto,
#'    col_nombre_lj = "id",
#'    fuente_data = "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia")
#' }
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
#' @param col_codigos Un character (cadena de caracteres) que contiene el
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
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' data_espacial_dpto <- estandarizar_geo_cods(data_event)
#' data_espacial_dpto <- geo_filtro(data_event = data_event,
#'                                  nombre_dpto = "Antioquia")
#' data_espacial_dpto <- agrupar_mun(data_event, dept_nombre = "Antioquia")
#' plot_map(data_agrupada = data_espacial_dpto,
#'    col_codigos = "id",
#'    fuente_data = "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia",
#'    dpto = "Antioquia",
#'    munpio = "Envigado")
#' @export
plot_map <- function(data_agrupada,
                     col_codigos = "id",
                     fuente_data = NULL,
                     dpto = NULL,
                     munpio = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada))
  stopifnot("El parametro col_codigos debe ser un cadena de caracteres"
            = is.character(col_codigos))
  titulo <- paste0("Departamento de ", dpto)
  subtitulo <- "Analisis efectuado por geografia de "
  cols_geo_ocurrencia <- c()
  data_tabla <- data.frame()
  if (is.null(fuente_data)) {
    fuente_data <- "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  if (!is.null(munpio)) {
    titulo <- paste0(titulo, " , ", munpio)
  }
  nombre_events <- unique(data_agrupada$nombre_evento)[1]
  cols_geo_ocurrencia <- obtener_tip_ocurren_geo(nombre_event = nombre_events)
  if (length(cols_geo_ocurrencia) > 1) {
    subtitulo <- paste0(subtitulo, cols_geo_ocurrencia[3])
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
  polygon_seleccionado$indice <- seq_len(nrow(polygon_seleccionado))
  polygon_seleccionado <- polygon_seleccionado %>%
    dplyr::left_join(data_agrupada, by = col_codigos)
  polygon_seleccionado <-
    cbind(polygon_seleccionado,
          sf::st_coordinates(sf::st_centroid(polygon_seleccionado$geometry)))
  data_tabla <-
    data.frame(Indice = polygon_seleccionado$indice,
               Codigo = polygon_seleccionado$id,
               Municipio = stringr::str_to_title(polygon_seleccionado
                                                 $MPIO_CNMBR))
  data_tabla <- data_tabla[order(data_tabla$Indice), ]
  sysfonts::font_add_google("Montserrat", "Montserrat")
  showtext::showtext_auto()
  map <- ggplot2::ggplot(polygon_seleccionado) +
    ggplot2::geom_sf(data = polygon_seleccionado,
                     ggplot2::aes(fill = .data$casos)) +
    ggplot2::geom_sf_text(ggplot2::aes(label = .data$indice),
                          size = ggplot2::unit(3, "cm"),
                          fontface = "bold") +
    ggplot2::scale_fill_continuous(low = "#fcebfc", high = "#be0000",
                                   guide = "colorbar", na.value = "white") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5),
                   text = ggplot2::element_text(family = "Montserrat",
                                                size = 14)) +
    ggplot2::labs(caption = fuente_data, fill = "Casos")
  tema_tabla <- gridExtra::ttheme_minimal(base_size = 14,
                                          base_family = "Montserrat",
                                          padding = ggplot2::unit(c(1, 1),
                                                                  "mm"))
  tabla <- ggplot2::ggplot() + ggplot2::theme_void() +
    ggplot2::annotation_custom(gridExtra::tableGrob(data_tabla,
                                                    theme = tema_tabla,
                                                    rows = NULL))
  mapa_tabla <- cowplot::plot_grid(map,
                                   tabla,
                                   align = "h",
                                   rel_widths = c(2, 1),
                                   nrow = 1)
  return(mapa_tabla)
}

#' Generar gráfico de distribución de casos por fecha de inicio de síntomas
#'
#' Función que genera el gráfico de distribución de casos
#' por fecha de inicio de síntomas
#' @param data_agrupada Un data frame que contiene los datos de la enfermedad
#' o evento agrupados
#' @param uni_marca Un character (cadena de caracteres) que contiene la unidad
#' de las marcas del gráfico (dia, mes o año);
#' su valor por defecto es "meses"
#' @param col_nombre Un character (cadena de caracteres) que contiene el
#' nombre de la columna en los datos de la enfermedad o evento
#' agrupados que contiene las fechas de inicio de síntomas; su valor por
#' defecto es "ini_sin"
#' @param tipo Un character (cadena de caracteres) que contiene el tipo de
#' grafico (barras o tendencia); su valor por defecto es "barras"
#' @param fuente_data Un character (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es NULL
#' @return Un plot o gráfico de la distribución de casos por fecha de inicio
#' de síntomas
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' data_agrupada <- agrupar_fecha_inisintomas(
#'                                      data_event,
#'                                      col_nombre = "ini_sin")
#' plot_fecha_inisintomas(data_agrupada = data_agrupada,
#'                        col_nombre = "ini_sin",
#'                        uni_marca = "semanaepi")
#' @export
plot_fecha_inisintomas <- function(data_agrupada,
                                   col_nombre = "ini_sin",
                                   uni_marca = "mes",
                                   tipo = "barras",
                                   fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada))
  stopifnot("El parametro col_nombre debe ser una cadena de caracteres" =
              is.character(col_nombre))
  stopifnot("El parametro uni_marca debe ser una cadena de caracteres" =
              is.character(uni_marca))
  stopifnot("Valor invalido para el parametro uni_marca" =
              uni_marca %in% c("mes", "dia", "semanaepi"))
  fechas_column_nombres <- config::get(file = system.file("extdata",
                                                          "config.yml",
                                                          package = "sivirep"),
                                       "dates_column_names")
  var_x <- col_nombre
  num_eventos <- length(unique(data_agrupada[["nombre_evento"]]))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  uni_marca <- switch(
    uni_marca,
    "mes" = "month",
    "dia" = "day",
    "semanaepi" = "semana"
  )
  if (is.null(col_nombre)) {
    col_nombre <- fechas_column_nombres[3]
  }
  if (uni_marca == "semana") {
    var_x <- "semana"
    data_agrupada[[var_x]] <- as.numeric(data_agrupada[[var_x]])
  }
  etiqueta_x <- paste0("\nFecha de inicio de sintomas por ",
                       uni_marca,
                       "\n")
  plot_casos_inisintomas <-
    ggplot2::ggplot(data_agrupada) +
    ggplot2::geom_col(ggplot2::aes(x = .data[[var_x]],
                                   y = .data[["casos"]],
                                   fill = .data[["nombre_evento"]]),
                      alpha = 0.9) +
    ggplot2::labs(x = etiqueta_x,
                  y = "Numero de casos\n",
                  caption = fuente_data) +
    obtener_estetica_escala(escala = num_eventos, nombre = "Eventos") +
    tema_sivirep() +
    ggplot2::theme(legend.position = "bottom") + {
      if (uni_marca != "semana") {
        ggplot2::scale_x_date(date_breaks = paste0("1 ",
                                                   uni_marca),
                              date_labels = "%b")
      }else {
        ggplot2::scale_x_continuous(breaks = seq(1,
                                                 53,
                                                 2))
      }
    }
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
#' @param fuente_data Un character (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es NULL
#' @return Un plot o gráfico de distribución de casos por fecha de notificación
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' data_agrupada <- agrupar_fecha_notifica(data_event,
#'                                         col_nombre = "fec_not")
#' plot_fecha_notifica(data_agrupada = data_agrupada,
#'                     col_nombre = "fec_not",
#'                     uni_marca = "semanaepi")
#' @export
plot_fecha_notifica <- function(data_agrupada,
                                col_nombre = "fec_not",
                                uni_marca = "semanaepi",
                                fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada))
  stopifnot("El parametro col_nombre debe ser una cadena de caracteres" =
              is.character(col_nombre))
  stopifnot("El parametro uni_marca debe ser una cadena de caracteres" =
              is.character(uni_marca))
  stopifnot("Valor invalido para el parametro uni_marca" =
              uni_marca %in% c("mes", "dia", "semanaepi"))
  fechas_column_nombres <- config::get(file =
                                         system.file("extdata",
                                                     "config.yml",
                                                     package = "sivirep"),
                                       "dates_column_names")
  var_x <- col_nombre
  num_eventos <- length(unique(data_agrupada[["nombre_evento"]]))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  uni_marca <- switch(
    uni_marca,
    "mes" = "month",
    "dia" = "day",
    "semanaepi" = "semana"
  )
  if (is.null(col_nombre)) {
    col_nombre <- fechas_column_nombres[2]
  }
  if (uni_marca == "semana") {
    var_x <- "semana"
    data_agrupada[[var_x]] <- as.numeric(data_agrupada[[var_x]])
  }
  etiqueta_x <- paste0("\nFecha de notificacion por ",
                       uni_marca,
                       "\n")
  plot_casos_fecha_notifica <-
    ggplot2::ggplot(data_agrupada) +
    ggplot2::geom_col(ggplot2::aes(x = .data[[var_x]],
                                   y = .data[["casos"]],
                                   fill = .data[["nombre_evento"]]),
                      alpha = 0.9) +
    ggplot2::labs(x = etiqueta_x,
                  y = "Numero de casos\n",
                  caption = fuente_data) +
    obtener_estetica_escala(escala = num_eventos, nombre = "Eventos") +
    tema_sivirep() +
    ggplot2::theme(legend.position = "bottom") + {
      if (uni_marca != "semana") {
        ggplot2::scale_x_date(date_breaks = paste0("1 ",
                                                   uni_marca),
                              date_labels = "%b")
      } else {
        ggplot2::scale_x_continuous(breaks = seq(1,
                                                 53,
                                                 2))
      }
    }
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
#' @param fuente_data Un character (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es NULL
#' @return Un plot o gráfico de distribución de casos por sexo
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' data_agrupada <- agrupar_sex(data_event,
#'                              col_nombre = "sexo",
#'                              porcentaje = TRUE)
#' plot_sex(data_agrupada = data_agrupada,
#'          col_nombre = "sexo",
#'          porcentaje = TRUE)
#' @export
plot_sex <- function(data_agrupada,
                     col_nombre = "sexo",
                     porcentaje = TRUE,
                     fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada))
  stopifnot("El parametro col_nombre debe ser una cadena de caracteres" =
              is.character(col_nombre))
  stopifnot("El parametro porcentaje debe ser un booleano" =
              is.logical(porcentaje))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  plot_casos_sex <- ggplot2::ggplot(data_agrupada,
                                    ggplot2::aes(x = .data[[col_nombre]],
                                                 y = .data[["casos"]],
                                                 fill = .data[[col_nombre]])) +
    ggplot2::geom_bar(width = 0.5,
                      stat = "identity") +
    ggplot2::labs(x = "\nSexo\n", y = "Numero de casos\n",
                  caption = fuente_data) +
    ggplot2::theme_classic() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(.data[["casos"]],
                                                   " \n (",
                                                   .data[["porcentaje"]],
                                                   " %)")),
                       vjust = 1.5,
                       color = "black",
                       hjust = 0.5) +
    obtener_estetica_escala(escala = 2, nombre = "Sexo") +
    tema_sivirep() +
    ggplot2::facet_wrap(facets = ~nombre_evento,
                        scales = "free_y",
                        ncol = 2)
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
#' @param fuente_data Un character (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es NULL
#' @return Un plot o gráfico de distribución de casos por sexo y semana
#' epidemiológica
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' data_agrupada <- agrupar_sex_semanaepi(data_event,
#'                                        col_nombres = c("sexo", "semana"),
#'                                        porcentaje = TRUE)
#' plot_sex_semanaepi(data_agrupada = data_agrupada,
#'                    col_nombres = c("sexo", "semana"))
#' @export
plot_sex_semanaepi <- function(data_agrupada,
                               col_nombres = c("sexo", "semana"),
                               fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada))
  stopifnot("El parametro col_nombres debe ser un arreglo" =
              is.character(col_nombres))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  plot_casos_sex_semanaepi <-
    ggplot2::ggplot(data_agrupada,
                    ggplot2::aes(x = .data[[col_nombres[2]]],
                                 y = .data[["casos"]],
                                 fill = .data[[col_nombres[1]]])) +
    ggplot2::geom_bar(width = 0.5,
                      stat = "identity") +
    ggplot2::labs(x = "\nSemana epidemiologica\n", y = "Numero de casos\n",
                  caption = fuente_data) +
    ggplot2::theme_classic() +
    obtener_estetica_escala(escala = 2, nombre = "Sexo") +
    tema_sivirep() +
    ggplot2::facet_wrap(facets = ~nombre_evento,
                        scales = "free_y",
                        ncol = 1)
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
#' @param fuente_data Un character (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es NULL
#' @return Un plot o gráfico de distribución de casos por edad
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' data_agrupada <- agrupar_edad(data_event,
#'                               col_nombre = "edad",
#'                               porcentaje = FALSE)
#' plot_edad(data_agrupada = data_agrupada,
#'           col_nombre = "edad")
#' @export
plot_edad <- function(data_agrupada,
                      col_nombre = "edad",
                      fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada))
  stopifnot("El parametro col_nombre debe ser una cadena de caracteres"
            = is.character(col_nombre))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  plot_casos_edad <-
    ggplot2::ggplot(data_agrupada,
                    ggplot2::aes(x = .data[[col_nombre]],
                                 y = .data[["casos"]])) +
    ggplot2::geom_bar(width = 0.7,
                      stat = "identity",
                      fill = "#2274BB") +
    ggplot2::labs(x = "\nEdad\n", y = "Numero de casos\n",
                  caption = fuente_data) +
    ggplot2::theme_classic() +
    tema_sivirep()
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
#' @param fuente_data Un character (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es NULL
#' @return Un plot o gráfico de distribución de casos por edad y sexo
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' data_agrupada <- agrupar_edad_sex(data_event,
#'                                   col_nombres = c("edad", "sexo"),
#'                                   porcentaje = FALSE)
#' plot_edad_sex(data_agrupada = data_agrupada,
#'               col_nombres = c("edad", "sexo"))
#' @export
plot_edad_sex <- function(data_agrupada,
                          col_nombres = c("edad", "sexo"),
                          fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada))
  stopifnot("El parametro col_nombres debe ser un arreglo"
            = is.character(col_nombres))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  plot_casos_edad_sexo <-
    ggplot2::ggplot(data_agrupada,
                    ggplot2::aes(x = .data[[col_nombres[1]]],
                                 y = .data[["casos"]],
                                 fill = .data[[col_nombres[2]]])) +
    ggplot2::geom_bar(width = 0.7,
                      stat = "identity") +
    ggplot2::labs(x = "\nEdad\n", y = "Numero de casos\n",
                  caption = fuente_data) +
    ggplot2::theme_classic() +
    obtener_estetica_escala(escala = 2, nombre = "Sexo") +
    tema_sivirep()
  return(plot_casos_edad_sexo)
}

#' Generar gráfico de distribución de casos por municipios
#'
#' Función que genera el gráfico de distribución de casos por municipios
#' @param data_agrupada Un data frame que contiene los datos de la
#' enfermedad o evento agrupados
#' @param col_nombre Un character (cadena de carácteres) con el nombre de
#' la columna de los datos agrupados de la enfermedad o evento que contiene
#' los municipios; su valor por defecto es "nombre"
#' @param fuente_data Un character (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es NULL
#' @return Un plot o gráfico de distribución de casos por municipios
#' @examples
#' data(dengue2020)
#' data_event <- dengue2020
#' data_event <- limpiar_data_sivigila(data_event, 2020)
#' data_event <- estandarizar_geo_cods(data_event)
#' data_agrupada <- agrupar_mun(data_event,
#'                              dept_nombre = "Antioquia")
#' plot_muns(data_agrupada,
#'           col_nombre = "nombre")
#' @export
plot_muns <- function(data_agrupada,
                      col_nombre = "nombre",
                      fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada))
  stopifnot("El parametro col_nombre debe ser una cadena de caracteres"
            = is.character(col_nombre))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  num_eventos <- length(unique(data_agrupada[["nombre_evento"]]))
  plot_casos_muns <-
    ggplot2::ggplot(data_agrupada,
                    ggplot2::aes(x = .data[[col_nombre]],
                                 y = .data[["casos"]],
                                 fill = .data[["nombre_evento"]])) +
    ggplot2::geom_bar(width = 0.5,
                      stat = "identity") +
    ggplot2::labs(x = "\nMunicipio\n", y = "Numero de casos\n",
                  caption = fuente_data) +
    ggplot2::theme_classic() +
    obtener_estetica_escala(escala = num_eventos, nombre = "Eventos") +
    tema_sivirep() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::coord_flip()
  return(plot_casos_muns)
}
