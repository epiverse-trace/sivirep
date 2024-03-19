#' Función que genera el mapa por departamentos o municipios con el número de
#' casos de una enfermedad o evento
#' @param data_agrupada Un `data.frame` que contiene los datos de la enfermedad
#' agrupados por departamento y número de casos
#' @param col_codigos Un `character` (cadena de caracteres) que contiene el
#' nombre de la columna para unir con el archivo de forma (shape file);
#' su valor por defecto `NULL`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos de la enfermedad o evento;
#' su valor por defecto `NULL`
#' @param dpto Un `character` (cadena de caracteres) que contiene el
#' nombre del departamento; su valor por defecto `NULL`
#' @param mpio Un `character` (cadena de caracteres) que contiene el
#' nombre del municipio; su valor por defecto `NULL`
#' @return Un `plot` o mapa por departamentos o municipios con el número de
#' casos de una enfermedad específica
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_espacial_dpto <- estandarizar_geo_cods(data_limpia)
#' data_filtrada <- geo_filtro(data_event = data_espacial_dpto,
#'                             dpto = "Antioquia",
#'                             mpio = "Envigado")
#' data_espacial_dpto <- agrupar_mpio(data_event = data_filtrada,
#'                                    dpto = "Antioquia")
#' plot_map(data_agrupada = data_espacial_dpto,
#'    col_codigos = "cod_mun_o",
#'    fuente_data = "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia",
#'    dpto = "Antioquia",
#'    mpio = "Envigado")
#' @export
plot_map <- function(data_agrupada,
                     col_codigos = NULL,
                     fuente_data = NULL,
                     dpto = NULL,
                     mpio = NULL) {
  stopifnot("El parametro data_agrupada es obligatorio" =
              !missing(data_agrupada),
            "El parametro data_agrupada debe ser un data.frame" =
              is.data.frame(data_agrupada),
            "El parametro data_agrupada no debe estar vacio" =
              nrow(data_agrupada) > 0)
  titulo <- "Colombia"
  subtitulo <- "Analisis efectuado por geografia de "
  cols_geo_ocurrencia <- NULL
  data_tabla <- data.frame()
  nombres_col <- NULL
  if (is.null(fuente_data)) {
    fuente_data <- "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  stopifnot("El parametro fuente_data debe ser un cadena de caracteres"
            = is.character(fuente_data))
  nombre_events <- unique(data_agrupada$nombre_evento)[1]
  cols_geo_ocurrencia <- obtener_tip_ocurren_geo(nombre_event = nombre_events)
  if (length(cols_geo_ocurrencia) > 1) {
    subtitulo <- paste0(subtitulo, cols_geo_ocurrencia[5])
  }
  config_file <- system.file("extdata", "config.yml", package = "sivirep")
  base_path <- config::get(file = config_file, "map_shape_file")
  if (is.null(dpto)) {
    nomb_cols <- colnames(data_agrupada)
    pos_col_dpto <- which(stringr::str_detect(nomb_cols,
                                              stringr::fixed("departamento")))
    pos_col_mpio <- which(stringr::str_detect(nomb_cols,
                                              stringr::fixed("municipio")))
    if (length(pos_col_dpto) > 0) {
      aux_dpto <- NULL
      aux_mpio <- NULL
      if (length(pos_col_dpto) > 0) {
        aux_dpto <- unique(data_agrupada[[nomb_cols[pos_col_dpto]]])
      }
      if (length(pos_col_mpio) > 0) {
        aux_mpio <- unique(data_agrupada[[nomb_cols[pos_col_mpio]]])
      }
      if (length(aux_dpto) == 1) {
        dpto <- aux_dpto
      } else {
        base_path <- config::get(file = config_file, "dpto_shape_file")
      }
      if (length(aux_mpio) == 1) {
        mpio <- aux_mpio
      }
    }
  }
  dsn <-  system.file(base_path,
                      package = "sivirep")
  shp <- sf::st_read(dsn = dsn, quiet = TRUE)
  polygon_seleccionado <- shp
  if (!is.null(dpto)) {
    stopifnot("El parametro dpto debe ser un cadena de caracteres"
              = is.character(dpto))
    data_dept <- obtener_info_depts(dpto, mpio)
    data_dept <- data_dept[1, ]
    titulo <- paste0("Departamento de ", stringr::str_to_title(dpto))
    polygon_seleccionado <- shp[shp$DPTO_CCDGO ==
                                  data_dept$codigo_departamento, ]
    polygon_seleccionado$MPIO_CCDGO <- paste0(polygon_seleccionado$DPTO_CCDGO,
                                              polygon_seleccionado$MPIO_CCDGO)
    if (!is.null(mpio)) {
      stopifnot("El parametro mpio debe ser un cadena de caracteres"
                = is.character(mpio))
      polygon_seleccionado <-
        polygon_seleccionado[polygon_seleccionado$MPIO_CCDGO ==
                               data_dept$codigo_municipio, ]
      titulo <- paste0(titulo, " , ", mpio)
    }
    colnames(polygon_seleccionado)[colnames(polygon_seleccionado) ==
                                     "MPIO_CCDGO"] <- "id"
  } else {
    colnames(polygon_seleccionado)[colnames(polygon_seleccionado) ==
                                     "DPTO"] <- "id"
  }
  if (!is.null(col_codigos)) {
    colnames(data_agrupada)[colnames(data_agrupada) ==
                              col_codigos] <- "id"
  } else {
    pos_col <- NULL
    if (is.null(dpto)) {
      pos_col <- which(colnames(data_agrupada) %in%
                         cols_geo_ocurrencia[1])
      nombres_col <- cols_geo_ocurrencia[2]
    } else {
      pos_col <- which(colnames(data_agrupada) %in%
                         cols_geo_ocurrencia[3])
      nombres_col <- cols_geo_ocurrencia[4]
    }
    if (length(pos_col) == 1) {
      colnames(data_agrupada)[pos_col] <- "id"
      col_codigos <- "id"
    } else {
      stopifnot("Debe ingresar el nombre de la columna que contiene
                los codigos de los departamentos o municipios en el
                parametro col_codigos" =
                  length(pos_col) == 1)
    }
  }
  data_agrupada <- data_agrupada %>%
    group_by_at(c("id", nombres_col)) %>%
    dplyr::summarise(casos = sum(.data$casos), .groups = "drop")
  polygon_seleccionado <- ggplot2::fortify(polygon_seleccionado, region = "id")
  polygon_seleccionado$indice <- seq_len(nrow(polygon_seleccionado))
  polygon_seleccionado <- polygon_seleccionado %>%
    dplyr::left_join(data_agrupada, by = "id")
  polygon_seleccionado <-
    cbind(polygon_seleccionado,
          sf::st_coordinates(sf::st_centroid(polygon_seleccionado$geometry)))
  if (!is.null(dpto)) {
    data_tabla <-
      data.frame(Indice = polygon_seleccionado$indice,
                 Codigo = polygon_seleccionado$id,
                 Municipio =
                   stringr::str_to_title(polygon_seleccionado$MPIO_CNMBR))
  } else {
    data_tabla <-
      data.frame(Indice = seq_along(length(data_agrupada$id)),
                 Codigo = data_agrupada$id,
                 Departamento =
                   stringr::str_to_title(data_agrupada[[nombres_col]]))
  }
  data_tabla <- data_tabla[order(data_tabla$Indice), ]
  sysfonts::font_add_google("Montserrat", "Montserrat")
  showtext::showtext_auto()
  map <- ggplot2::ggplot(polygon_seleccionado) +
    ggplot2::ggtitle(label = titulo, subtitle = subtitulo) +
    ggplot2::geom_sf(data = polygon_seleccionado,
                     ggplot2::aes(fill = .data$casos)) +
    ggplot2::geom_sf_text(ggplot2::aes(label = .data$indice),
                          size = ggplot2::unit(3, "cm"),
                          fontface = "bold") +
    ggplot2::scale_fill_continuous(low = "#fcebfc", high = "#be0000",
                                   guide = "colorbar", na.value = "white") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,
                                                      face = "bold"),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5,
                                                         face = "bold"),
                   text = ggplot2::element_text(size = 14),
                   legend.title = ggplot2::element_text(face = "bold")) +
    ggplot2::labs(caption = fuente_data, fill = "Casos")
  tema_tabla <- gridExtra::ttheme_minimal(base_size = 14,
                                          base_family = "Arial",
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
#' @param data_agrupada Un `data.frame` que contiene los datos de la enfermedad
#' o evento agrupados
#' @param uni_marca Un `character` (cadena de caracteres) que contiene la unidad
#' de las marcas del gráfico (`"dia"`, `"semanaepi"` y `"mes"``);
#' su valor por defecto es `"semanaepi"`
#' @param nomb_col Un `character` (cadena de caracteres) que contiene el
#' nombre de la columna en los datos de la enfermedad o evento
#' agrupados con las fechas de inicio de síntomas; su valor por
#' defecto es `"ini_sin"`
#' @param tipo Un `character` (cadena de caracteres) que contiene el tipo de
#' grafico (`"barras"` o `"tendencia"`); su valor por defecto es `"barras"`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de la distribución de casos por fecha de inicio
#' de síntomas
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_fecha_inisintomas(
#'                                      data_event = data_limpia,
#'                                      nomb_col = "ini_sin")
#' plot_fecha_inisintomas(data_agrupada = data_agrupada,
#'                        nomb_col = "ini_sin",
#'                        uni_marca = "semanaepi")
#' @export
plot_fecha_inisintomas <- function(data_agrupada,
                                   nomb_col = "ini_sin",
                                   uni_marca = "semanaepi",
                                   tipo = "barras",
                                   fuente_data = NULL) {
  stopifnot("El parametro data_agrupada es obligatorio" =
              !missing(data_agrupada),
            "El parametro data_agrupada debe ser un data.frame" =
              is.data.frame(data_agrupada),
            "El parametro data_agrupada no debe estar vacio" =
              nrow(data_agrupada) > 0,
            "El parametro nomb_col debe ser una cadena de caracteres" =
              is.character(nomb_col),
            "El parametro uni_marca debe ser una cadena de caracteres" =
              is.character(uni_marca),
            "Valor invalido para el parametro uni_marca" =
              uni_marca %in% c("dia", "semanaepi", "mes"),
            "El parametro tipo debe ser una cadena de caracteres" =
              is.character(tipo),
            "Valor invalido para el parametro tipo" =
              tipo %in% c("barras", "tendencia"))
  fechas_column_nombres <- config::get(file = system.file("extdata",
                                                          "config.yml",
                                                          package = "sivirep"),
                                       "dates_column_names")
  var_x <- nomb_col
  num_eventos <- length(unique(data_agrupada[["nombre_evento"]]))
  data_plot <- data_agrupada
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  stopifnot("El parametro fuente_data debe ser un cadena de caracteres"
            = is.character(fuente_data))
  uni_marca <- switch(
    uni_marca,
    mes = "month",
    dia = "day",
    semanaepi = "semana"
  )
  if (is.null(nomb_col)) {
    nomb_col <- fechas_column_nombres[3]
  }
  if (uni_marca == "semana") {
    var_x <- "semana"
    data_plot[[var_x]] <- as.numeric(data_agrupada[[var_x]])
  }
  if (tipo == "tendencia" && uni_marca != "day") {
    data_plot <- data_plot %>% dplyr::group_by_at(c(var_x,
                                                    "nombre_evento")) %>%
      dplyr::summarise(casos = sum(.data$casos), .groups = "drop")
  }
  etiqueta_x <- paste0("\nFecha de inicio de sintomas por ",
                       uni_marca,
                       "\n")
  pos_leyenda <- ggplot2::theme(legend.position = "right")
  if (num_eventos > 3) {
    pos_leyenda <- ggplot2::theme(legend.position = "bottom")
  }
  plot_casos_inisintomas <-
    ggplot2::ggplot(data_plot,
                    ggplot2::aes(x = .data[[var_x]],
                                 y = .data[["casos"]],
                                 fill = .data[["nombre_evento"]])) + {
      if (tipo == "tendencia") {
        ggplot2::geom_line(linewidth = 1,
                           color = "#FDDA0D")
      } else {
        ggplot2::geom_col(alpha = 0.9)
      }
    } +
    ggplot2::labs(x = etiqueta_x,
                  y = "Numero de casos\n",
                  caption = fuente_data) +
    obtener_estetica_escala(escala = num_eventos, nombre = "Eventos") +
    tema_sivirep() +
    pos_leyenda + {
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
  return(plot_casos_inisintomas)
}

#' Generar gráfico de distribución de casos por fecha de notificación
#'
#' Función que genera el gráfico de distribución de casos por
#' fecha de notificación
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados
#' @param uni_marca Un `character` (cadena de caracteres) que contiene la unidad
#' de las marcas del gráfico (`"dia"`, `"semanaepi"`y `"mes"``);
#' su valor por defecto es `"semanaepi"`
#' @param nomb_col Un `character` (cadena de caracteres) que contiene el
#' nombre de la columna en los datos de la enfermedad o evento agrupados con
#' las fechas de notificación; su valor por defecto es `"fec_not"`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @param tipo Un `character` (cadena de caracteres) que contiene el tipo de
#' grafico (`"barras"` o `"tendencia"`); su valor por defecto es `"barras"`
#' @return Un `plot` o gráfico de distribución de casos por fecha de
#' notificación
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_fecha_notifica(data_event = data_limpia,
#'                                         nomb_col = "fec_not")
#' plot_fecha_notifica(data_agrupada = data_agrupada,
#'                     nomb_col = "fec_not",
#'                     uni_marca = "semanaepi")
#' @export
plot_fecha_notifica <- function(data_agrupada,
                                nomb_col = "fec_not",
                                uni_marca = "semanaepi",
                                fuente_data = NULL,
                                tipo = "barras") {
  stopifnot("El parametro data_agrupada es obligatorio" =
              !missing(data_agrupada),
            "El parametro data_agrupada debe ser un data.frame" =
              is.data.frame(data_agrupada),
            "El parametro data_agrupada no debe estar vacio" =
              nrow(data_agrupada) > 0,
            "El parametro nomb_col debe ser una cadena de caracteres" =
              is.character(nomb_col),
            "El parametro uni_marca debe ser una cadena de caracteres" =
              is.character(uni_marca),
            "Valor invalido para el parametro uni_marca" =
              uni_marca %in% c("dia", "semanaepi", "mes"),
            "El parametro tipo debe ser una cadena de caracteres" =
              is.character(tipo),
            "Valor invalido para el parametro tipo" =
              tipo %in% c("barras", "tendencia"))
  fechas_column_nombres <- config::get(file =
                                         system.file("extdata",
                                                     "config.yml",
                                                     package = "sivirep"),
                                       "dates_column_names")
  var_x <- nomb_col
  num_eventos <- length(unique(data_agrupada[["nombre_evento"]]))
  data_plot <- data_agrupada
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  stopifnot("El parametro fuente_data debe ser una cadena de caracteres" =
              is.character(fuente_data))
  uni_marca <- switch(
    uni_marca,
    mes = "month",
    dia = "day",
    semanaepi = "semana"
  )
  if (is.null(nomb_col)) {
    nomb_col <- fechas_column_nombres[2]
  }
  if (uni_marca == "semana") {
    var_x <- "semana"
    data_plot[[var_x]] <- as.numeric(data_agrupada[[var_x]])
  }
  if (tipo == "tendencia" && uni_marca != "day") {
    data_plot <- data_plot %>% dplyr::group_by_at(c(var_x,
                                                  "nombre_evento")) %>%
      dplyr::summarise(casos = sum(.data$casos), .groups = "drop")
  }
  etiqueta_x <- paste0("\nFecha de notificacion por ",
                       uni_marca,
                       "\n")
  pos_leyenda <- ggplot2::theme(legend.position = "right")
  if (num_eventos > 3) {
    pos_leyenda <- ggplot2::theme(legend.position = "bottom")
  }
  plot_casos_fecha_notifica <-
    ggplot2::ggplot(data_plot,
                    ggplot2::aes(x = .data[[var_x]],
                                 y = .data[["casos"]],
                                 fill = .data[["nombre_evento"]])) + {
      if (tipo == "tendencia") {
          ggplot2::geom_line(linewidth = 1,
                             color = "#FDDA0D")
      } else {
          ggplot2::geom_col(alpha = 0.9)
      }
    } +
    ggplot2::labs(x = etiqueta_x,
                  y = "Numero de casos\n",
                  caption = fuente_data) +
    obtener_estetica_escala(escala = num_eventos, nombre = "Eventos") +
    tema_sivirep() +
    pos_leyenda + {
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
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados
#' @param nomb_col Un `character` (cadena de caracteres) con el
#' nombre de la columna de los datos agrupados de la enfermedad
#' o evento que contiene el sexo; su valor por defecto es `"sexo"`
#' @param porcentaje Un boolean (TRUE/FALSE) que indica si los datos
#' tienen porcentajes; su valor por defecto es `TRUE`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de distribución de casos por sexo
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_sex(data_event = data_limpia,
#'                              nomb_col = "sexo",
#'                              porcentaje = TRUE)
#' plot_sex(data_agrupada = data_agrupada,
#'          nomb_col = "sexo",
#'          porcentaje = TRUE)
#' @export
plot_sex <- function(data_agrupada,
                     nomb_col = "sexo",
                     porcentaje = TRUE,
                     fuente_data = NULL) {
  stopifnot("El parametro data_agrupada es obligatorio" =
              !missing(data_agrupada),
            "El parametro data_agrupada debe ser un data.frame" =
              is.data.frame(data_agrupada),
            "El parametro data_agrupada no debe estar vacio" =
              nrow(data_agrupada) > 0,
            "El parametro nomb_col debe ser una cadena de caracteres" =
              is.character(nomb_col),
            "El parametro porcentaje debe ser un booleano" =
              is.logical(porcentaje))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  stopifnot("El parametro fuente_data debe ser un cadena de caracteres"
            = is.character(fuente_data))
  plot_casos_sex <- ggplot2::ggplot(data_agrupada,
                                    ggplot2::aes(x = .data[[nomb_col]],
                                                 y = .data[["casos"]],
                                                 fill = .data[[nomb_col]])) +
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
#' @param data_agrupada Un `data.frame` que contiene los datos de la enfermedad
#' o evento agrupados
#' @param nomb_cols Un `array` (arreglo) de `character` (cadena de caracteres)
#' con los nombres de columna de los datos agrupados de la enfermedad o evento
#' que contienen el sexo y las semanas epidemiológicas; su valor por defecto es
#' `c("sexo", "semana")`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de distribución de casos por sexo y semana
#' epidemiológica
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_sex_semanaepi(data_event = data_limpia,
#'                                        nomb_cols = c("sexo", "semana"),
#'                                        porcentaje = TRUE)
#' plot_sex_semanaepi(data_agrupada = data_agrupada,
#'                    nomb_cols = c("sexo", "semana"))
#' @export
plot_sex_semanaepi <- function(data_agrupada,
                               nomb_cols = c("sexo", "semana"),
                               fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada),
            "El parametro nomb_cols debe ser un arreglo" =
              is.character(nomb_cols))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  plot_casos_sex_semanaepi <-
    ggplot2::ggplot(data_agrupada,
                    ggplot2::aes(x = .data[[nomb_cols[2]]],
                                 y = .data[["casos"]],
                                 fill = .data[[nomb_cols[1]]])) +
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
#' @param data_agrupada Un `data.frame` que contiene los datos de la enfermedad
#' o evento agrupados
#' @param nomb_col Un `character` (cadena de carácteres) con el nombre de
#' la columna de los datos agrupados de la enfermedad o evento que contiene
#' las edades; su valor por defecto es `"edad"`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de distribución de casos por edad
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_edad(data_event = data_limpia,
#'                               nomb_col = "edad",
#'                               porcentaje = FALSE)
#' plot_edad(data_agrupada = data_agrupada,
#'           nomb_col = "edad")
#' @export
plot_edad <- function(data_agrupada,
                      nomb_col = "edad",
                      fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada),
            "El parametro nomb_col debe ser una cadena de caracteres"
            = is.character(nomb_col))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  plot_casos_edad <-
    ggplot2::ggplot(data_agrupada,
                    ggplot2::aes(x = .data[[nomb_col]],
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
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados
#' @param nomb_cols Un `array` (arreglo) de `character` (cadena de caracteres)
#' con los nombres de las columnas de los datos agrupados de la enfermedad o
#' evento que contienen las edades y las semanas epidemiológicas; su valor
#' por defecto es `c("edad", "sexo")`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de distribución de casos por edad y sexo
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_edad_sex(data_event = data_limpia,
#'                                   nomb_cols = c("edad", "sexo"),
#'                                   porcentaje = FALSE)
#' plot_edad_sex(data_agrupada = data_agrupada,
#'               nomb_cols = c("edad", "sexo"))
#' @export
plot_edad_sex <- function(data_agrupada,
                          nomb_cols = c("edad", "sexo"),
                          fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada),
            "El parametro nomb_cols debe ser un arreglo"
            = is.character(nomb_cols))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  plot_casos_edad_sexo <-
    ggplot2::ggplot(data_agrupada,
                    ggplot2::aes(x = .data[[nomb_cols[1]]],
                                 y = .data[["casos"]],
                                 fill = .data[[nomb_cols[2]]])) +
    ggplot2::geom_bar(width = 0.7,
                      stat = "identity") +
    ggplot2::labs(x = "\nEdad\n", y = "Numero de casos\n",
                  caption = fuente_data) +
    ggplot2::theme_classic() +
    obtener_estetica_escala(escala = 2, nombre = "Sexo") +
    tema_sivirep()
  return(plot_casos_edad_sexo)
}

#' Generar gráfico de distribución de casos por departamentos
#'
#' Función que genera el gráfico de distribución de casos por departamentos
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados
#' @param nomb_col Un `character` (cadena de carácteres) con el nombre de
#' la columna de los datos agrupados de la enfermedad o evento que contiene
#' los departamentos; su valor por defecto es `NULL`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de distribución de casos por departamentos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_limpia <- estandarizar_geo_cods(data_limpia)
#' data_agrupada <- agrupar_mpio(data_event = data_limpia,
#'                               dpto = "Antioquia")
#' plot_dptos(data_agrupada,
#'            nomb_col = "departamento_ocurrencia")
#' @export
plot_dptos <- function(data_agrupada,
                       nomb_col = NULL,
                       fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  if (is.null(nomb_col)) {
    cols_geo_ocurrencia <-
      obtener_tip_ocurren_geo(nombre_event =
                                data_agrupada[["nombre_evento"]][1])
    if (length(cols_geo_ocurrencia) > 1) {
      nomb_col <- cols_geo_ocurrencia[2]
    }
  } else {
    stopifnot("El parametro nomb_col debe ser una cadena de caracteres"
              = is.character(nomb_col))
  }
  num_eventos <- length(unique(data_agrupada[["nombre_evento"]]))
  pos_leyenda <- ggplot2::theme(legend.position = "right")
  if (num_eventos > 3) {
    pos_leyenda <- ggplot2::theme(legend.position = "bottom")
  }
  data_agrupada <- data_agrupada %>%
    group_by_at(nomb_col) %>%
    dplyr::summarise(casos = sum(.data[["casos"]]), .groups = "drop")
  plot_casos_dptos <-
    ggplot2::ggplot(data_agrupada,
                    ggplot2::aes(x = stats::reorder(.data[[nomb_col]],
                                                    .data[["casos"]]),
                                 y = .data[["casos"]])) +
    ggplot2::geom_bar(width = 0.5,
                      stat = "identity",
                      fill = "#2274BB") +
    ggplot2::labs(x = "\nDepartamento\n", y = "Numero de casos\n",
                  caption = fuente_data) +
    ggplot2::theme_classic() +
    tema_sivirep() +
    pos_leyenda +
    ggplot2::coord_flip()
  return(plot_casos_dptos)
}

#' Generar gráfico de distribución de casos por municipios
#'
#' Función que genera el gráfico de distribución de casos por municipios
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados
#' @param nomb_col Un `character` (cadena de carácteres) con el nombre de
#' la columna de los datos agrupados de la enfermedad o evento que contiene
#' los municipios; su valor por defecto es `NULL`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de distribución de casos por municipios
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_limpia <- estandarizar_geo_cods(data_limpia)
#' data_agrupada <- agrupar_mpio(data_event = data_limpia,
#'                               dpto = "Antioquia")
#' plot_mpios(data_agrupada,
#'            nomb_col = "municipio_ocurrencia")
#' @export
plot_mpios <- function(data_agrupada,
                       nomb_col = NULL,
                       fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  if (is.null(nomb_col)) {
    cols_geo_ocurrencia <-
      obtener_tip_ocurren_geo(nombre_event =
                                data_agrupada[["nombre_evento"]][1])
    if (length(cols_geo_ocurrencia) > 1) {
      nomb_col <- cols_geo_ocurrencia[4]
    }
  } else {
    stopifnot("El parametro nomb_col debe ser una cadena de caracteres"
              = is.character(nomb_col))
  }
  num_eventos <- length(unique(data_agrupada[["nombre_evento"]]))
  pos_leyenda <- ggplot2::theme(legend.position = "right")
  if (num_eventos > 3) {
    pos_leyenda <- ggplot2::theme(legend.position = "bottom")
  }
  data_agrupada <- data_agrupada %>%
    group_by_at(nomb_col) %>%
    dplyr::summarise(casos = sum(.data[["casos"]]), .groups = "drop")
  plot_casos_muns <-
    ggplot2::ggplot(data_agrupada,
                    ggplot2::aes(x = stats::reorder(.data[[nomb_col]],
                                                    .data[["casos"]]),
                                 y = .data[["casos"]])) +
    ggplot2::geom_bar(width = 0.5,
                      stat = "identity",
                      fill = "#2274BB") +
    ggplot2::labs(x = "\nMunicipio\n", y = "Numero de casos\n",
                  caption = fuente_data) +
    ggplot2::theme_classic() +
    tema_sivirep() +
    pos_leyenda +
    ggplot2::coord_flip()
  return(plot_casos_muns)
}

#' Generar gráfico de distribución de casos por área geográfica
#'
#' Función que genera el gráfico de casos por área geográfica
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados
#' @param nomb_col Un `character` (cadena de carácteres) con el nombre de
#' la columna de los datos agrupados de la enfermedad o evento por
#' departamentos o municipios; su valor por defecto es `"area"`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de distribución de casos por área geográfica
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_area_geo(data_event = data_limpia,
#'                                   dpto = "Antioquia")
#' plot_area_geo(data_agrupada,
#'               nomb_col = "area")
#' @export
plot_area_geo <- function(data_agrupada,
                          nomb_col = "area",
                          fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada),
            "El parametro nomb_col debe ser una cadena de caracteres"
            = is.character(nomb_col))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  cols_geo_ocurrencia <-
    obtener_tip_ocurren_geo(nombre_event =
                              data_agrupada[["nombre_evento"]][1])
  etiquetas_areas <- config::get(file =
                                   system.file("extdata",
                                               "config.yml",
                                               package = "sivirep"),
                           "labels_geo_areas")
  if (length(cols_geo_ocurrencia) > 1) {
      if (length(names(data_agrupada)) > 5) {
        nomb_col <- append(nomb_col, cols_geo_ocurrencia[4])
      } else {
        nomb_col <- append(nomb_col, cols_geo_ocurrencia[2])
      }
  }
  pos_leyenda <- ggplot2::theme(legend.position = "right")
  data_agrupada_area <- data_agrupada %>%
    group_by_at(nomb_col) %>%
    dplyr::summarise(casos = sum(.data[["casos"]]), .groups = "drop")
  plot_casos_area <-
    ggplot2::ggplot(data_agrupada_area,
                    ggplot2::aes(x = .data[[nomb_col[2]]],
                                 y = .data[["casos"]],
                                 fill = .data[[nomb_col[1]]])) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(x = "\nDepartamento\n", y = "Numero de casos\n",
                  caption = fuente_data) +
    ggplot2::theme_classic() +
    obtener_estetica_escala(escala = 3, nombre = "Area\n",
                            etiquetas = etiquetas_areas) +
    tema_sivirep() +
    pos_leyenda +
    ggplot2::coord_flip()
  return(plot_casos_area)
}


#' Generar tabla con la distribución de casos por tipo de
#' enfermedad o evento
#'
#' Función que genera el la tabla con la distribución de casos por
#' tipo de enfermedad o evento
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados
#' @param nomb_col Un `character` (cadena de carácteres) con el nombre de
#' la columna de los datos agrupados de la enfermedad o evento por
#' tipo; su valor por defecto es `"nombre_evento"`
#' @return Una `kable` (tabla gráfica) con la distribución de casos
#' por tipo de enfermedad o evento
plot_tabla_tipos_event <- function(data_agrupada,
                                   nomb_col = "nombre_evento") {
  data_agrupada[["nombre_evento"]] <-
    stringr::str_to_title(data_agrupada[["nombre_evento"]])
  tabla_tipos <- knitr::kable(data_agrupada,
                              col.names = c("Codigo ",
                                            "Tipo ", "Casos"),
                              align = "c",
                              caption = "Distribucion de casos por evento") %>%
    kableExtra::row_spec(0, color = "white", background = "#2274BB") %>%
    kableExtra::kable_styling(full_width = FALSE,
                              latex_options = "HOLD_position")
  return(tabla_tipos)
}

#' Generar gráfico de distribución de casos por año
#'
#' Función que genera el gráfico de casos por año
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados por año
#' @param nomb_col Un `character` (cadena de carácteres) con el nombre de
#' la columna de los datos agrupados de la enfermedad o evento por
#' año; su valor por defecto es `"ano"`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de distribución de casos por año
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_years(data_event = data_limpia)
#' plot_years(data_agrupada,
#'            nomb_col = "ano")
#' @export
plot_years <- function(data_agrupada,
                       nomb_col = "ano",
                       fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada),
            "El parametro nomb_col debe ser una cadena de caracteres"
            = is.character(nomb_col))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  eventos <- length(unique(data_agrupada[["nombre_evento"]]))
  plot_casos_years <-
    ggplot2::ggplot(data_agrupada,
                    ggplot2::aes(x = .data[[nomb_col]],
                                 y = .data[["casos"]],
                                 fill = .data[["nombre_evento"]])) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(x = "\nYear\n", y = "Numero de casos\n",
                  caption = fuente_data) +
    ggplot2::theme_classic() +
    obtener_estetica_escala(escala = eventos, nombre = "Eventos\n") +
    tema_sivirep() +
    ggplot2::theme(legend.position = "right")
  return(plot_casos_years)
}


#' Generar gráfico de distribución de casos por la clasificacion inicial
#' del caso
#'
#' Función que genera el gráfico por la clasificación inicial de los
#' casos
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados por la clasificación inicial
#' @param nomb_col Un `character` (cadena de carácteres) con el nombre de
#' la columna de los datos agrupados de la enfermedad o evento por la
#' clasificación inicial; su valor por defecto es `"tip_cas"`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de distribución de casos por la clasificación
#' inicial
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_years(data_event = data_limpia)
#' plot_tipo_caso(data_agrupada,
#'                nomb_col = "ano")
#' @export
plot_tipo_caso <- function(data_agrupada,
                           nomb_col = "tip_cas",
                           fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada),
            "El parametro nomb_col debe ser una cadena de caracteres"
            = is.character(nomb_col))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  nomb_col <- c(nomb_col, "nombre_evento")
  etiquetas <- config::get(file =
                             system.file("extdata",
                                         "config.yml",
                                         package = "sivirep"),
                           "labels_cas_tip")
  clasificacion <- unique(data_agrupada[[nomb_col[1]]])
  escala <- length(clasificacion)
  etiquetas <- etiquetas[as.character(clasificacion)]
  etiquetas <- unlist(etiquetas)
  plot_casos_years <-
    ggplot2::ggplot(data_agrupada,
                    ggplot2::aes(x = .data[[nomb_col[1]]],
                                 y = .data[["casos"]],
                                 fill = .data[[nomb_col[2]]])) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(x = "\nClasificacion del caso\n", y = "Numero de casos\n",
                  caption = fuente_data) +
    ggplot2::theme_classic() +
    obtener_estetica_escala(escala = escala, nombre = "Eventos\n") +
    ggplot2::scale_x_discrete(labels = etiquetas)
    tema_sivirep() +
    ggplot2::theme(legend.position = "right")
  return(plot_casos_years)
}

#' Generar gráfico de distribución de casos por la clasificacion inicial
#' del caso y los años seleccionados
#'
#' Función que genera el gráfico por la clasificación inicial de los
#' casos y los años seleccionados
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados por la clasificación inicial y los años
#' @param col_tipo Un `character` (cadena de carácteres) con el nombre de
#' la columna de los datos agrupados de la enfermedad o evento por la
#' clasificación inicial; su valor por defecto es `"tip_cas"`
#' @param col_year Un `character` (cadena de carácteres) con el nombre de
#' la columna de los datos agrupados de la enfermedad o evento por el año;
#' su valor por defecto es `"ano"`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de distribución de casos por la clasificación
#' inicial y los años seleccionados
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_tipo_caso(data_event = data_limpia,
#' nomb_cols = c("tip_cas", "ano"))
#' plot_tipo_caso_years(data_agrupada,
#'                      col_tipo = "tip_cas",
#'                      col_year = "ano")
#' @export
plot_tipo_caso_years <- function(data_agrupada,
                                 col_tipo = "tip_cas",
                                 col_year = "ano",
                                 fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada),
            "El parametro col_tipo debe ser una cadena de caracteres"
            = is.character(col_tipo),
            "El parametro col_year debe ser una cadena de caracteres"
            = is.character(col_year))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  etiquetas <- config::get(file =
                             system.file("extdata",
                                         "config.yml",
                                         package = "sivirep"),
                           "labels_cas_tip")
  clasificacion <- unique(data_agrupada[[col_tipo]])
  escala <- length(unique(data_agrupada[[col_tipo]]))
  etiquetas <- etiquetas[as.character(clasificacion)]
  etiquetas <- unlist(etiquetas)
  plot_casos_years <-
    ggplot2::ggplot(data_agrupada,
                    ggplot2::aes(x = .data[[col_year]],
                                 y = .data[["casos"]],
                                 fill = .data[[col_tipo]])) +
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    ggplot2::labs(x = "\nAño\n", y = "Numero de casos\n",
                  caption = fuente_data) +
    ggplot2::theme_classic() +
    obtener_estetica_escala(escala = escala,
                            nombre = "Clasificacion del caso\n",
                            etiquetas = etiquetas) +
    tema_sivirep() +
    ggplot2::theme(legend.position = "right")
  return(plot_casos_years)
}
