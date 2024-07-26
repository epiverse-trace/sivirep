#' @title Generar mapa
#' @description Función que genera el mapa por departamentos o municipios con el
#' número de casos o la incidencia de una enfermedad o evento.
#' @param data_agrupada Un `data.frame` que contiene los datos de la enfermedad
#' agrupados por departamento y número de casos.
#' @param col_distribucion Un `character` (cadena de caracteres) que contiene el
#' nombre de la columna que tiene los valores de la distribución, por número de
#' casos o incidencia; su valor por defecto es `"incidencia"`.
#' @param col_codigos Un `character` (cadena de caracteres) que contiene el
#' nombre de la columna con los códigos de los departamentos o municipios, se
#' utilizan para obtener los poligonos de las áreas geográficas del archivo
#' geoespacial o de figuras (Shapefile); su valor por defecto `NULL`.
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos de la enfermedad o evento;
#' su valor por defecto `NULL`.
#' @param dpto Un `character` (cadena de caracteres) que contiene el
#' nombre del departamento; su valor por defecto `NULL`.
#' @param mpio Un `character` (cadena de caracteres) que contiene el
#' nombre del municipio; su valor por defecto `NULL`.
#' @return Un `plot` o mapa por departamentos o municipios con el número de
#' casos o incidencia de una enfermedad específica.
#' @examples
#' \dontrun{
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_estandar <- estandarizar_geo_cods(data_limpia)
#' # Mapa por departamentos
#' data_espacial <- agrupar_dpto(data_event = data_estandar)
#' plot_map(
#'   data_agrupada = data_espacial,
#'   col_distribucion = "casos"
#' )
#' # Mapa por municipios de un departamento especifico
#' data_filtrada_dpto <- geo_filtro(
#'   data_event = data_estandar,
#'   dpto = "Cundinamarca"
#' )
#' data_espacial_dpto <- agrupar_mpio(data_event = data_filtrada_dpto)
#' plot_map(
#'   data_agrupada = data_espacial_dpto,
#'   col_codigos = "cod_mun_o",
#'   col_distribucion = "casos"
#' )
#' # Mapa por municipio especifico
#' data_filtrada_mpio <- geo_filtro(
#'   data_event = data_estandar,
#'   dpto = "Antioquia",
#'   mpio = "Medellin"
#' )
#' data_espacial_mpio <- agrupar_mpio(data_event = data_filtrada_mpio)
#' plot_map(
#'   data_agrupada = data_espacial_mpio,
#'   col_codigos = "cod_mun_o",
#'   col_distribucion = "casos",
#'   dpto = "Antioquia",
#'   mpio = "Medellin"
#' )
#' # Mapa con la incidencia por municipios de un departamento específico
#' incidencia_dpto <-
#'   calcular_incidencia_geo(data_agrupada = data_espacial_dpto)
#' plot_map(
#'   data_agrupada = incidencia_dpto$data_incidencia,
#'   col_codigos = "cod_mun_o",
#'   col_distribucion = "incidencia"
#' )
#' }
#' @export
plot_map <- function(data_agrupada,
                     col_distribucion = "incidencia",
                     col_codigos = NULL,
                     fuente_data = NULL,
                     dpto = NULL,
                     mpio = NULL) {
  validar_data_agrupada(data_agrupada)
  titulo <- "Colombia"
  subtitulo <- obtener_val_config("label_geo_analysis")
  cols_geo_ocurrencia <- NULL
  nombres_col <- NULL
  pos_col <- NULL
  etiqueta_relleno <- "Casos"
  shp <- import_shape_map()
  if (is.null(fuente_data)) {
    fuente_data <- "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  stopifnot(
    "El parametro fuente_data debe ser un cadena de caracteres" =
      is.character(fuente_data)
  )
  color_min <- "#fcebfc"
  if ("nombre_evento" %in% names(data_agrupada)) {
    nombre_events <- unique(data_agrupada$nombre_evento)[1]
    cols_geo_ocurrencia <- obtener_tip_ocurren_geo(nombre_event = nombre_events)
  }
  if ("cod_eve" %in% names(data_agrupada)) {
    cod_event <- unique(data_agrupada$cod_eve)[1]
    cols_geo_ocurrencia <- obtener_tip_ocurren_geo(cod_event = cod_event)
  }
  if (length(cols_geo_ocurrencia) > 1) {
    subtitulo <- paste0(subtitulo, cols_geo_ocurrencia[5])
  }
  if (col_distribucion == "incidencia") {
    etiqueta_relleno <- "Incidencia"
    cond_incidencia <-
      obtener_cond_inciden_event(cod_eve = data_agrupada$cod_eve[1])
    etiqueta_relleno <- paste0(
      etiqueta_relleno, " por \n",
      as.integer(cond_incidencia$coeficiente),
      " habitantes"
    )
  }
  config_map <- obtener_config_map(
    data_agrupada, dpto, mpio,
    cols_geo_ocurrencia, shp
  )
  pos_col <- which(colnames(data_agrupada) %in%
    cols_geo_ocurrencia[1])
  nombres_col <- cols_geo_ocurrencia[2]
  if (!is.null(config_map$dpto)) {
    titulo <- paste0(
      "Departamento de ",
      stringr::str_to_title(config_map$dpto)
    )
  }
  if (!is.null(config_map$mpio)) {
    titulo <- paste0(
      titulo, ", ",
      stringr::str_to_title(config_map$mpio)
    )
    color_min <- "#be0000"
    pos_col <- which(colnames(data_agrupada) %in%
      cols_geo_ocurrencia[3])
    nombres_col <- cols_geo_ocurrencia[4]
  }
  if (!is.null(col_codigos)) {
    colnames(data_agrupada)[colnames(data_agrupada) ==
      col_codigos] <- "id"
  } else if (length(pos_col) == 1) {
    colnames(data_agrupada)[pos_col] <- "id"
    col_codigos <- "id"
  } else {
    stop("Debe ingresar el nombre de la columna que contiene
          los codigos de los departamentos o municipios en el
          parametro col_codigos")
  }
  polygon_seleccionado <- config_map$poligono
  data_agrupada <- group_by(
    data_agrupada,
    dplyr::across(dplyr::all_of(c("id", nombres_col)))
  )
  data_agrupada <- dplyr::summarise(data_agrupada,
    casos =
      sum(.data[[col_distribucion]]),
    .groups = "drop"
  )
  data_agrupada <- data_agrupada[order(data_agrupada[["casos"]],
    decreasing = FALSE
  ), ]
  polygon_seleccionado <- ggplot2::fortify(polygon_seleccionado, region = "id")
  polygon_seleccionado <- dplyr::left_join(polygon_seleccionado,
    data_agrupada,
    by = "id"
  )
  polygon_seleccionado <-
    cbind(
      polygon_seleccionado,
      sf::st_coordinates(sf::st_centroid(polygon_seleccionado$geometry))
    )
  map <- ggplot2::ggplot(polygon_seleccionado) +
    ggplot2::ggtitle(label = titulo, subtitle = subtitulo) +
    ggplot2::geom_sf(
      data = polygon_seleccionado,
      ggplot2::aes(fill = .data$casos)
    ) +
    ggplot2::scale_fill_continuous(
      low = color_min, high = "#be0000",
      guide = "colorbar", na.value = "white"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        size = 18
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        size = 16
      ),
      text = ggplot2::element_text(size = 16),
      legend.text = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(
        size = 16,
        face = "bold"
      ),
      plot.caption = ggplot2::element_text(size = 12),
    ) +
    ggplot2::labs(caption = fuente_data, fill = etiqueta_relleno) +
    ggplot2::guides(
      fill =
        ggplot2::guide_colourbar(
          barwidth =
            ggplot2::unit(0.04, "npc"),
          barheight =
            ggplot2::unit(0.5, "npc")
        )
    )
  return(map)
}

#' @title Generar gráfico de distribución de casos por fecha de inicio de
#' síntomas
#' @description Función que genera el gráfico de distribución de casos
#' por fecha de inicio de síntomas.
#' @param data_agrupada Un `data.frame` que contiene los datos de la enfermedad
#' o evento agrupados.
#' @param uni_marca Un `character` (cadena de caracteres) que contiene la unidad
#' de las marcas del gráfico (`"dia"`, `"semanaepi"` y `"mes"``);
#' su valor por defecto es `"semanaepi"`.
#' @param col_fecha Un `character` (cadena de caracteres) que contiene el
#' nombre de la columna con las fechas de notificación en los datos de la
#' enfermedad o evento agrupados; su valor por defecto es `"ini_sin"`.
#' @param tipo Un `character` (cadena de caracteres) que contiene el tipo de
#' gráfico (`"barras"` o `"tendencia"`); su valor por defecto es `"barras"`.
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto
#' es `NULL`.
#' @return Un `plot` o gráfico de la distribución de casos por fecha de inicio
#' de síntomas.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_fecha_inisintomas(
#'   data_event = data_limpia
#' )
#' plot_fecha_inisintomas(
#'   data_agrupada = data_agrupada,
#'   col_fecha = "ini_sin",
#'   uni_marca = "semanaepi"
#' )
#' @export
plot_fecha_inisintomas <- function(data_agrupada,
                                   col_fecha = "ini_sin",
                                   uni_marca = "semanaepi",
                                   tipo = "barras",
                                   fuente_data = NULL) {
  validar_data_agrupada(data_agrupada)
  validar_fecha_inisintomas(data_agrupada, col_fecha, uni_marca, tipo)
  fechas_column_nombres <- obtener_val_config("dates_column_names")
  var_x <- col_fecha
  num_eventos <- length(unique(data_agrupada[["nombre_evento"]]))
  data_plot <- data_agrupada
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  stopifnot(
    "El parametro fuente_data debe ser un cadena de caracteres" =
      is.character(fuente_data)
  )
  uni_marca <- switch(uni_marca,
    mes = "month",
    dia = "day",
    semanaepi = "semana"
  )
  if (is.null(col_fecha)) {
    col_fecha <- fechas_column_nombres[3]
  }
  if (uni_marca == "semana") {
    var_x <- "semana"
    data_plot[[var_x]] <- as.numeric(data_agrupada[[var_x]])
  }
  if (tipo == "tendencia" && uni_marca != "day") {
    data_plot <- dplyr::group_by(
      data_plot,
      dplyr::across(dplyr::all_of(c(var_x, "nombre_evento")))
    )
    data_plot <- dplyr::summarise(data_plot,
      casos = sum(.data$casos),
      .groups = "drop"
    )
  }
  etiqueta_fecha <- obtener_val_config("label_date_ini")
  etiqueta_x <- paste0("\n", etiqueta_fecha, " por ", uni_marca, "\n")
  etiqueta_casos <- obtener_val_config("label_cases")
  pos_leyenda <- ggplot2::theme(legend.position = "right")
  if (num_eventos > 3) {
    pos_leyenda <- ggplot2::theme(legend.position = "bottom")
  }
  plot_casos_inisintomas <-
    ggplot2::ggplot(
      data_plot,
      ggplot2::aes(
        x = .data[[var_x]],
        y = .data[["casos"]],
        fill = .data[["nombre_evento"]]
      )
    ) +
    {
      if (tipo == "tendencia") {
        ggplot2::geom_line(
          linewidth = 1,
          color = "#FDDA0D"
        )
      } else {
        ggplot2::geom_col(alpha = 0.9)
      }
    } +
    ggplot2::labs(
      x = etiqueta_x,
      y = paste0(etiqueta_casos, "\n"),
      caption = fuente_data
    ) +
    obtener_estetica_escala(escala = num_eventos, nombre = "Eventos") +
    tema_sivirep() +
    pos_leyenda +
    {
      if (uni_marca != "semana") {
        ggplot2::scale_x_date(
          date_breaks = paste0(
            "1 ",
            uni_marca
          ),
          date_labels = "%b"
        )
      } else {
        ggplot2::scale_x_continuous(breaks = seq(
          1,
          53,
          2
        ))
      }
    }
  return(plot_casos_inisintomas)
}

#' @title Generar gráfico de distribución de casos por sexo
#' @description Función que genera el gráfico de distribución de casos por sexo.
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados.
#' @param col_sex Un `character` (cadena de caracteres) con el nombre de la
#' columna que contiene el sexo en los datos agrupados de la enfermedad o
#' evento; su valor por defecto es `"sexo"`.
#' @param col_distribucion Un `character` (cadena de caracteres) que contiene el
#' nombre de la columna que tiene los valores de la distribución, por número de
#' casos o incidencia; su valor por defecto es `"incidencia"`.
#' @param porcentaje Un `boolean` (TRUE/FALSE) que indica si los datos
#' tienen porcentajes; su valor por defecto es `TRUE`.
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`.
#' @return Un `plot` o gráfico de distribución de casos por sexo.
#' @examples
#' \donttest{
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_sex(
#'   data_event = data_limpia,
#'   porcentaje = TRUE
#' )
#' plot_sex(
#'   data_agrupada = data_agrupada,
#'   col_sex = "sexo",
#'   porcentaje = TRUE
#' )
#' }
#' @export
plot_sex <- function(data_agrupada,
                     col_sex = "sexo",
                     col_distribucion = "casos",
                     porcentaje = TRUE,
                     fuente_data = NULL) {
  validar_data_agrupada(data_agrupada)
  validar_sex(data_agrupada, col_sex)
  validar_porcentaje(porcentaje)
  stopifnot(
    "Valor invalido para el parametro col_distribucion" =
      col_distribucion %in% c("casos", "incidencia")
  )
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  stopifnot(
    "El parametro fuente_data debe ser un cadena de caracteres" =
      is.character(fuente_data)
  )
  if (col_distribucion == "casos") {
    etiqueta_eje <- obtener_val_config("label_cases")
  } else {
    etiqueta_eje <- obtener_val_config("label_incidence")
  }
  plot_casos_sex <- ggplot2::ggplot(
    data_agrupada,
    ggplot2::aes(
      x =
        .data[[col_sex]],
      y =
        .data[[col_distribucion]],
      fill = .data[[col_sex]]
    )
  ) +
    ggplot2::geom_col(width = 0.5) +
    ggplot2::labs(
      x = "\nSexo\n", y = paste0(etiqueta_eje, "\n"),
      caption = fuente_data
    ) +
    {
      if (porcentaje) {
        ggplot2::geom_text(
          ggplot2::aes(
            label =
              paste0(
                .data[[col_distribucion]],
                " \n (",
                .data[["porcentaje"]],
                " %)"
              )
          ),
          vjust = 1.5,
          color = "white",
          hjust = 0.5
        )
      } else {
        ggplot2::geom_text(ggplot2::aes(label = .data[[col_distribucion]]),
          vjust = 1.5,
          color = "white",
          hjust = 0.5
        )
      }
    } +
    obtener_estetica_escala(escala = 2, nombre = "Sexo") +
    tema_sivirep() +
    ggplot2::facet_wrap(
      facets = ~nombre_evento,
      scales = "free_y",
      ncol = 2
    )
  return(plot_casos_sex)
}

#' @title Generar gráfico de distribución de casos por sexo y semana
#' epidemiológica
#' @description Función que genera el gráfico de distribución de casos por sexo
#' y semana epidemiológica.
#' @param data_agrupada Un `data.frame` que contiene los datos de la enfermedad
#' o evento agrupados.
#' @param col_sex Un `character` (cadena de caracteres) con el nombre de la
#' columna que contiene el sexo en los datos agrupados de la enfermedad o
#' evento; su valor por defecto es `"sexo"`.
#' @param col_semanaepi Un `character` (cadena de caracteres) con el nombre de
#' la columna que contiene las semanas epidemiológicas en los datos agrupados
#' de la enfermedad o evento; su valor por defecto es `"semana"`.
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto
#' es `NULL`.
#' @return Un `plot` o gráfico de distribución de casos por sexo y semana
#' epidemiológica.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_sex_semanaepi(data_event = data_limpia)
#' plot_sex_semanaepi(
#'   data_agrupada = data_agrupada,
#'   col_sex = "sexo",
#'   col_semanaepi = "semana"
#' )
#' @export
plot_sex_semanaepi <- function(data_agrupada,
                               col_sex = "sexo",
                               col_semanaepi = "semana",
                               fuente_data = NULL) {
  validar_data_agrupada(data_agrupada)
  validar_sex(data_agrupada, col_sex)
  stopifnot(
    "El parametro col_semanaepi debe ser una cadena de caracteres" =
      is.character(col_semanaepi)
  )
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  max_casos <- max(data_agrupada$casos)
  data_agrupada$semana <- as.numeric(data_agrupada$semana)
  max_semana <- max(data_agrupada$semana)
  etiqueta_episemana <- obtener_val_config("label_epiweek")
  etiqueta_casos <- obtener_val_config("label_cases")
  plot_casos_sex_semanaepi <-
    ggplot2::ggplot(
      data_agrupada,
      ggplot2::aes(
        x = .data[[col_semanaepi]],
        y = .data[["casos"]],
        fill = .data[[col_sex]]
      )
    ) +
    ggplot2::geom_col(width = 0.5) +
    ggplot2::labs(
      x = paste0("\n", etiqueta_episemana, "\n"),
      y = paste0(etiqueta_casos, "\n"),
      caption = fuente_data
    ) +
    {
      if (max_casos < 6) {
        ggplot2::scale_y_continuous(breaks = seq(0, max_casos, 1))
      }
    } +
    ggplot2::scale_x_continuous(breaks = seq(1, max_semana, 1)) +
    obtener_estetica_escala(escala = 2, nombre = "Sexo") +
    tema_sivirep() +
    ggplot2::facet_wrap(
      facets = ~nombre_evento,
      scales = "free_y",
      ncol = 1
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(
      angle = 90,
      hjust = 1
    ))
  return(plot_casos_sex_semanaepi)
}

#' @title Generar gráfico de distribución de casos por edad
#' @description Función que genera el gráfico de distribución de casos
#' por edad.
#' @param data_agrupada Un `data.frame` que contiene los datos de la enfermedad
#' o evento agrupados.
#' @param col_edad Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene las edades en los datos agrupados de la enfermedad o
#' evento; su valor por defecto es `"edad"`.
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto
#' es `NULL`.
#' @return Un `plot` o gráfico de distribución de casos por edad.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_edad(data_event = data_limpia)
#' plot_edad(
#'   data_agrupada = data_agrupada,
#'   col_edad = "edad"
#' )
#' @export
plot_edad <- function(data_agrupada,
                      col_edad = "edad",
                      fuente_data = NULL) {
  validar_data_agrupada(data_agrupada)
  validar_edad(data_agrupada, col_edad)
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  etiqueta_casos <- obtener_val_config("label_cases")
  plot_casos_edad <-
    ggplot2::ggplot(
      data_agrupada,
      ggplot2::aes(
        x = .data[[col_edad]],
        y = .data[["casos"]]
      )
    ) +
    ggplot2::geom_col(
      width = 0.7,
      fill = "#2274BB"
    ) +
    ggplot2::labs(
      x = "\nGrupo de edad\n",
      y = paste0(etiqueta_casos, "\n"),
      caption = fuente_data
    ) +
    tema_sivirep()
  return(plot_casos_edad)
}

#' @title Generar gráfico de distribución de casos por edad y sexo
#' @description Función que genera el gráfico de distribución de casos por
#' edad y sexo.
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados.
#' @param col_edad Un `character` (cadena de caracteres) con el nombre de la
#' columna que contiene las edades en los datos agrupados de la enfermedad o
#' evento; su valor por defecto es `"edad`.
#' @param col_sex Un `character` (cadena de caracteres) con el nombre de la
#' columna que contiene el sexo en los datos agrupados de la enfermdedad o
#' evento; su valor por defecto es `"sexo`.
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto
#' es `NULL`.
#' @return Un `plot` o gráfico de distribución de casos por edad y sexo.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_edad_sex(data_event = data_limpia)
#' plot_edad_sex(
#'   data_agrupada = data_agrupada,
#'   col_edad = "edad",
#'   col_sex = "sexo"
#' )
#' @export
plot_edad_sex <- function(data_agrupada,
                          col_edad = "edad",
                          col_sex = "sexo",
                          fuente_data = NULL) {
  validar_data_agrupada(data_agrupada)
  validar_edad(data_agrupada, col_edad)
  validar_sex(data_agrupada, col_sex)
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  etiqueta_casos <- obtener_val_config("label_cases")
  plot_casos_edad_sexo <-
    ggplot2::ggplot(
      data_agrupada,
      ggplot2::aes(
        x = .data[[col_edad]],
        y = .data[["casos"]],
        fill = .data[[col_sex]]
      )
    ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::labs(
      x = "\nGrupo de edad\n", y = paste0(etiqueta_casos, "\n"),
      caption = fuente_data
    ) +
    obtener_estetica_escala(escala = 2, nombre = "Sexo") +
    tema_sivirep()
  return(plot_casos_edad_sexo)
}

#' @title Generar gráfico de distribución de casos por departamentos
#' @description Función que genera el gráfico de distribución de casos por
#' departamentos.
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados por departamentos.
#' @param col_dptos Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene los departamenos en los datos agrupados de la
#' enfermedad o evento; su valor por defecto es `NULL`.
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto
#' es `NULL`.
#' @return Un `plot` o gráfico de distribución de casos por departamentos.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_limpia <- estandarizar_geo_cods(data_limpia)
#' data_agrupada <- agrupar_dpto(data_event = data_limpia)
#' plot_dptos(data_agrupada,
#'   col_dptos = "departamento_ocurrencia"
#' )
#' @export
plot_dptos <- function(data_agrupada,
                       col_dptos = NULL,
                       fuente_data = NULL) {
  validar_data_agrupada(data_agrupada)
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  if (is.null(col_dptos)) {
    cols_geo_ocurrencia <-
      obtener_tip_ocurren_geo(
        nombre_event =
          data_agrupada[["nombre_evento"]][1]
      )
    if (length(cols_geo_ocurrencia) > 1) {
      col_dptos <- cols_geo_ocurrencia[2]
    }
  } else {
    stopifnot(
      "El parametro col_dptos debe ser una cadena de caracteres" =
        is.character(col_dptos)
    )
  }
  etiqueta_casos <- obtener_val_config("label_cases")
  num_eventos <- length(unique(data_agrupada[["nombre_evento"]]))
  pos_leyenda <- ggplot2::theme(legend.position = "right")
  if (num_eventos > 3) {
    pos_leyenda <- ggplot2::theme(legend.position = "bottom")
  }
  data_agrupada <- group_by(
    data_agrupada,
    dplyr::across(dplyr::all_of(col_dptos))
  )
  data_agrupada <-
    dplyr::summarise(data_agrupada,
      casos = sum(.data[["casos"]]), .groups = "drop"
    )
  plot_casos_dptos <-
    ggplot2::ggplot(
      data_agrupada,
      ggplot2::aes(
        x = .data[["casos"]],
        y = stats::reorder(
          .data[[col_dptos]],
          .data[["casos"]]
        )
      )
    ) +
    ggplot2::geom_col(
      width = 0.5,
      fill = "#2274BB",
      orientation = "y"
    ) +
    ggplot2::labs(
      x = "\nDepartamento\n",
      y = paste0(etiqueta_casos, "\n"),
      caption = fuente_data
    ) +
    tema_sivirep() +
    pos_leyenda +
    return(plot_casos_dptos)
}

#' @title Generar gráfico de distribución de casos por municipios
#' @description Función que genera el gráfico de distribución de casos por
#' municipios.
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados por municipios.
#' @param col_mpios Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene los municipios en los datos agrupados de la
#' enfermedad o evento; su valor por defecto es `NULL`.
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto
#' es `NULL`.
#' @return Un `plot` o gráfico de distribución de casos por municipios.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_limpia <- estandarizar_geo_cods(data_limpia)
#' data_agrupada <- agrupar_mpio(
#'   data_event = data_limpia,
#'   dpto = "Antioquia"
#' )
#' plot_mpios(data_agrupada,
#'   col_mpios = "municipio_ocurrencia"
#' )
#' @export
plot_mpios <- function(data_agrupada,
                       col_mpios = NULL,
                       fuente_data = NULL) {
  validar_data_agrupada(data_agrupada)
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  if (is.null(col_mpios)) {
    cols_geo_ocurrencia <-
      obtener_tip_ocurren_geo(
        nombre_event =
          data_agrupada[["nombre_evento"]][1]
      )
    if (length(cols_geo_ocurrencia) > 1) {
      col_mpios <- cols_geo_ocurrencia[4]
    }
  } else {
    stopifnot(
      "El parametro col_mpios debe ser una cadena de caracteres" =
        is.character(col_mpios)
    )
  }
  etiqueta_casos <- obtener_val_config("label_cases")
  num_eventos <- length(unique(data_agrupada[["nombre_evento"]]))
  pos_leyenda <- ggplot2::theme(legend.position = "right")
  if (num_eventos > 3) {
    pos_leyenda <- ggplot2::theme(legend.position = "bottom")
  }
  data_agrupada <- group_by(
    data_agrupada,
    dplyr::across(dplyr::all_of(col_mpios))
  )
  data_agrupada <-
    dplyr::summarise(data_agrupada,
      casos = sum(.data[["casos"]]), .groups = "drop"
    )
  plot_casos_muns <-
    ggplot2::ggplot(
      data_agrupada,
      ggplot2::aes(
        x = .data[["casos"]],
        y = stats::reorder(
          .data[[col_mpios]],
          .data[["casos"]]
        )
      )
    ) +
    ggplot2::geom_col(
      width = 0.5,
      fill = "#2274BB",
      orientation = "y"
    ) +
    ggplot2::labs(
      x = "\nMunicipio\n",
      y = paste0(etiqueta_casos, "\n"),
      caption = fuente_data
    ) +
    tema_sivirep() +
    pos_leyenda
  return(plot_casos_muns)
}

#' @title Generar gráfico de distribución de casos por área geográfica
#' @description Función que genera el gráfico de casos por área geográfica.
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados.
#' @param col_area Un `character` (cadena de carácteres) con el nombre de
#' la columna con el área geografica en los datos agrupados de la enfermedad
#' o evento; su valor por defecto es `"area"`.
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto
#' es `NULL`.
#' @return Un `plot` o gráfico de distribución de casos por área geográfica.
#' @examples
#' \donttest{
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_area_geo(data_event = data_limpia)
#' plot_area_geo(data_agrupada,
#'   col_area = "area"
#' )
#' }
#' @export
plot_area_geo <- function(data_agrupada,
                          col_area = "area",
                          fuente_data = NULL) {
  validar_data_agrupada(data_agrupada)
  stopifnot(
    "El parametro col_area debe ser una cadena de caracteres" =
      is.character(col_area)
  )
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  etiquetas_areas <- obtener_val_config("labels_geo_areas")
  etiqueta_casos <- obtener_val_config("label_cases")
  etiqueta_area_geo <- obtener_val_config("label_geo_area")
  pos_leyenda <- ggplot2::theme(legend.position = "right")
  plot_casos_area <-
    ggplot2::ggplot(
      data_agrupada,
      ggplot2::aes(
        x = .data[[col_area]],
        y = .data[["casos"]]
      )
    ) +
    ggplot2::geom_col(
      fill = "#2274BB",
      width = 0.3
    ) +
    ggplot2::labs(
      x = paste0("\n", etiqueta_area_geo),
      y = paste0(etiqueta_casos, "\n"),
      caption = fuente_data
    ) +
    tema_sivirep() +
    ggplot2::scale_x_discrete(labels = etiquetas_areas) +
    pos_leyenda
  return(plot_casos_area)
}

#' @title Generar gráfico de distribución de casos por área geográfica a nivel
#' departamental o municipal
#' @description Función que genera el gráfico de casos por área geográfica a
#' nivel departamental o municipal.
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados.
#' @param col_area Un `character` (cadena de carácteres) con el nombre de
#' la columna con el área geografica en los datos agrupados de la enfermedad
#' o evento; su valor por defecto es `"area"`.
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto
#' es `NULL`.
#' @return Un `plot` o gráfico de distribución de casos por área geográfica.
#' @examples
#' \donttest{
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_top_area_geo(
#'   data_event = data_limpia,
#'   dpto = "Antioquia"
#' )
#' plot_top_area_geo(data_agrupada,
#'   col_area = "area"
#' )
#' }
#' @export
plot_top_area_geo <- function(data_agrupada,
                              col_area = "area",
                              fuente_data = NULL) {
  validar_data_agrupada(data_agrupada)
  stopifnot(
    "El parametro col_area debe ser una cadena de caracteres" =
      is.character(col_area)
  )
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  cols_geo_ocurrencia <-
    obtener_tip_ocurren_geo(
      nombre_event =
        data_agrupada[["nombre_evento"]][1]
    )
  etiquetas_areas <- obtener_val_config("labels_geo_areas")
  etiqueta_casos <- obtener_val_config("label_cases")
  etiqueta_area_geo <- obtener_val_config("label_geo_area")
  nomb_cols <- NULL
  if (length(cols_geo_ocurrencia) > 1) {
    if (cols_geo_ocurrencia[4] %in% names(data_agrupada)) {
      nomb_cols <- append(col_area, cols_geo_ocurrencia[4])
    } else {
      nomb_cols <- append(col_area, cols_geo_ocurrencia[2])
    }
  }
  pos_leyenda <- ggplot2::theme(legend.position = "right")
  data_agrupada_area <- group_by(
    data_agrupada,
    dplyr::across(dplyr::all_of(nomb_cols))
  )
  data_agrupada_area <- dplyr::summarise(data_agrupada_area,
    casos = sum(.data[["casos"]]),
    .groups = "drop"
  )
  plot_casos_area <-
    ggplot2::ggplot(
      data_agrupada_area,
      ggplot2::aes(
        x = .data[["casos"]],
        y = .data[[nomb_cols[2]]],
        fill = .data[[nomb_cols[1]]]
      )
    ) +
    ggplot2::geom_col(orientation = "y") +
    ggplot2::labs(
      x = "\nDepartamento\n",
      y = paste0("\n", etiqueta_casos),
      caption = fuente_data
    ) +
    obtener_estetica_escala(
      escala = 3,
      nombre = paste0(etiqueta_area_geo, "\n"),
      etiquetas = etiquetas_areas
    ) +
    tema_sivirep() +
    pos_leyenda
  return(plot_casos_area)
}

#' @title Generar tabla con la distribución de casos por tipo de
#' enfermedad o evento
#' @description Función que genera la tabla con la distribución de casos
#' por tipo de enfermedad o evento.
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados por tipo.
#' @param col_event Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene el tipo de evento en los datos agrupados de
#' la enfermedad o evento; su valor por defecto es `"nombre_evento"`.
#' @return Una `kable` (tabla gráfica) con la distribución de casos
#' por tipo de enfermedad o evento.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_eventos(
#'   data_event = data_limpia,
#'   col_event = "cod_eve"
#' )
#' plot_tabla_tipos_event(data_agrupada,
#'   col_event = "nombre_evento"
#' )
#' @export
plot_tabla_tipos_event <- function(data_agrupada,
                                   col_event = "nombre_evento") {
  validar_data_agrupada(data_agrupada)
  stopifnot(
    "El parametro col_event debe ser una cadena de caracteres" =
      is.character(col_event)
  )
  etiqueta_cod <- obtener_val_config("label_code")
  caption_tabla <- obtener_val_config("caption_table_events")
  data_agrupada[[col_event]] <-
    stringr::str_to_title(data_agrupada[[col_event]])
  tabla_tipos <- kableExtra::kbl(
    data_agrupada[, c(
      "cod_eve",
      col_event,
      "casos"
    )],
    col.names = c(
      etiqueta_cod,
      "Evento", "Casos"
    ),
    align = "c",
    caption = caption_tabla
  )
  tabla_tipos <- kableExtra::row_spec(tabla_tipos, 0,
    color = "white",
    background = "#2274BB"
  )
  tabla_tipos <-
    kableExtra::kable_styling(tabla_tipos,
      full_width = FALSE,
      latex_options = "HOLD_position"
    )
  return(tabla_tipos)
}

#' @title Generar gráfico de distribución de casos por año
#' @description Función que genera el gráfico de casos por año.
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados por año.
#' @param col_year Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene los años en los datos agrupados de la enfermedad
#' o evento por año; su valor por defecto es `"ano"`.
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto
#' es `NULL`.
#' @return Un `plot` o gráfico de distribución de casos por año.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_years(data_event = data_limpia)
#' plot_years(data_agrupada,
#'   col_year = "ano"
#' )
#' \donttest{
#' data_years <- import_data_event(
#'   nombre_event = "ZIKA",
#'   years = c(2019, 2020),
#'   consentimiento = "Si"
#' )
#' data_limpia <- limpiar_data_sivigila(data_years)
#' data_agrupada <- agrupar_years(data_event = data_limpia)
#' plot_years(data_agrupada,
#'   col_year = "ano"
#' )
#' }
#' @export
plot_years <- function(data_agrupada,
                       col_year = "ano",
                       fuente_data = NULL) {
  validar_data_agrupada(data_agrupada)
  validar_years(data_agrupada, col_year)
  etiqueta_year <- obtener_val_config("label_year")
  etiqueta_casos <- obtener_val_config("label_cases")
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  eventos <- length(unique(data_agrupada[["nombre_evento"]]))
  plot_casos_years <-
    ggplot2::ggplot(
      data_agrupada,
      ggplot2::aes(
        x = .data[[col_year]],
        y = .data[["casos"]],
        fill = .data[["nombre_evento"]]
      )
    ) +
    ggplot2::geom_col() +
    ggplot2::labs(
      x = paste0("\n", etiqueta_year, "\n"),
      y = paste0(etiqueta_casos, "\n"),
      caption = fuente_data
    ) +
    obtener_estetica_escala(escala = eventos, nombre = "Eventos\n") +
    tema_sivirep() +
    ggplot2::theme(legend.position = "right")
  return(plot_casos_years)
}

#' @title Generar gráfico de distribución de casos por la clasificacion
#' inicial del caso
#' @description Función que genera el gráfico de distribución por la
#' clasificación inicial de los casos.
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados por la clasificación inicial de los casos.
#' @param col_tipo Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene la clasificación inicial de los casos en los
#' datos agrupados de la enfermedad o evento; su valor por defecto es
#' `"tip_cas"`.
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto
#' es `NULL`.
#' @return Un `plot` o gráfico de distribución de casos por la clasificación
#' inicial.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_tipo_caso(data_event = data_limpia)
#' plot_tipo_caso(data_agrupada,
#'   col_tipo = "tip_cas"
#' )
#' @export
plot_tipo_caso <- function(data_agrupada,
                           col_tipo = "tip_cas",
                           fuente_data = NULL) {
  validar_data_agrupada(data_agrupada)
  stopifnot(
    "El parametro col_tipo debe ser una cadena de caracteres" =
      is.character(col_tipo)
  )
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  if (length(col_tipo) == 1) {
    nomb_cols <- c(col_tipo, "nombre_evento")
  }
  etiquetas <- obtener_val_config("labels_cas_tip")
  etiqueta_casos <- obtener_val_config("label_cases")
  etiqueta_tipo <- obtener_val_config("label_type_case")
  clasificacion <- unique(data_agrupada[[nomb_cols[1]]])
  escala <- length(unique(data_agrupada[[nomb_cols[2]]]))
  etiquetas <- etiquetas[as.character(clasificacion)]
  etiquetas <- unlist(etiquetas)
  plot_tipo_casos <-
    ggplot2::ggplot(
      data_agrupada,
      ggplot2::aes(
        x = .data[[nomb_cols[1]]],
        y = .data[["casos"]],
        fill = .data[[nomb_cols[2]]]
      )
    ) +
    ggplot2::geom_col(width = 0.5) +
    ggplot2::labs(
      x = paste0("\n", etiqueta_tipo, "\n"),
      y = paste0(etiqueta_casos, "\n"),
      caption = fuente_data
    ) +
    obtener_estetica_escala(escala = escala, nombre = "Eventos\n") +
    ggplot2::scale_x_discrete(labels = etiquetas)
  tema_sivirep() +
    ggplot2::theme(legend.position = "right")
  return(plot_tipo_casos)
}

#' @title Generar gráfico de distribución de casos por la clasificacion
#' inicial del caso y los años seleccionados
#' @description Función que genera el gráfico por la clasificación inicial
#' de los casos y los años seleccionados.
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento, agrupados por la clasificación inicial y los años
#' seleccionados.
#' @param col_tipo Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene la clasificación inicial del caso en los datos
#' agrupados de la enfermedad o evento; su valor por defecto es
#' `"tip_cas"`.
#' @param col_year Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene el año en los datos agrupados de la enfermedad
#' o evento; su valor por defecto es `"ano"`.
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto
#' es `NULL`.
#' @return Un `plot` o gráfico de distribución de casos por la clasificación
#' inicial y los años seleccionados.
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_tipo_caso(
#'   data_event = data_limpia,
#'   cols_tipo = c(
#'     "tip_cas",
#'     "ano"
#'   )
#' )
#' plot_tipo_caso_years(data_agrupada,
#'   col_tipo = "tip_cas",
#'   col_year = "ano"
#' )
#' @export
plot_tipo_caso_years <- function(data_agrupada,
                                 col_tipo = "tip_cas",
                                 col_year = "ano",
                                 fuente_data = NULL) {
  validar_data_agrupada(data_agrupada)
  validar_years(data_agrupada, col_year)
  stopifnot(
    "El parametro col_tipo debe ser una cadena de caracteres" =
      is.character(col_tipo)
  )
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  etiquetas <- obtener_val_config("labels_cas_tip")
  etiqueta_year <- obtener_val_config("label_year")
  etiqueta_casos <- obtener_val_config("label_cases")
  etiqueta_tipo <- obtener_val_config("label_type_case")
  clasificacion <- unique(data_agrupada[[col_tipo]])
  escala <- length(unique(data_agrupada[[col_tipo]]))
  etiquetas <- etiquetas[as.character(clasificacion)]
  etiquetas <- unlist(etiquetas)
  plot_casos_years <-
    ggplot2::ggplot(
      data_agrupada,
      ggplot2::aes(
        x = .data[[col_year]],
        y = .data[["casos"]],
        fill = .data[[col_tipo]]
      )
    ) +
    ggplot2::geom_col() +
    ggplot2::labs(
      x = paste0("\n", etiqueta_year, "\n"),
      y = paste0(etiqueta_casos, "\n"),
      caption = fuente_data
    ) +
    obtener_estetica_escala(
      escala = escala,
      nombre = paste0(etiqueta_tipo, "\n"),
      etiquetas = etiquetas
    ) +
    tema_sivirep() +
    ggplot2::theme(legend.position = "right")
  return(plot_casos_years)
}

#' @title Generar gráfico de distribución de casos por la pertenencia
#' étnica
#' @description Función que genera el gráfico de la distribución de casos
#' por la pertenencia étnica.
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados por la pertenencia étnica.
#' @param col_etn Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene la pertenencia étnica en los datos agrupados de
#' la enfermedad o evento; su valor por defecto es `"per_etn"`.
#' @param porcentaje Un `boolean` (TRUE/FALSE) que indica si los datos
#' tienen porcentajes; su valor por defecto es `TRUE`.
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto
#' es `NULL`.
#' @return Un `plot` o gráfico de la distribución de casos por la pertenencia
#' étnica.
#' @examples
#' \donttest{
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_per_etn(data_event = data_limpia)
#' plot_per_etn(data_agrupada,
#'   col_etn = "per_etn"
#' )
#' }
#' @export
plot_per_etn <- function(data_agrupada,
                         col_etn = "per_etn",
                         porcentaje = TRUE,
                         fuente_data = NULL) {
  validar_data_agrupada(data_agrupada)
  stopifnot(
    "El parametro col_etn debe ser una cadena de caracteres" =
      is.character(col_etn)
  )
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  etiqueta_casos <- obtener_val_config("label_cases")
  etiqueta_etn <- obtener_val_config("label_etn_groups")
  etiquetas <- obtener_val_config("labels_per_etn")
  grupos <- unique(data_agrupada[[col_etn]])
  etiquetas <- etiquetas[as.character(grupos)]
  etiquetas <- unlist(etiquetas)
  data_agrupada <- agrupar_cols_casos(
    data_event = data_agrupada,
    nomb_cols = col_etn,
    porcentaje = TRUE,
    estandar = FALSE
  )
  plot_per_etn <-
    ggplot2::ggplot(
      data_agrupada,
      ggplot2::aes(
        x = .data[["casos"]],
        y = .data[[col_etn]]
      )
    ) +
    ggplot2::geom_col(fill = "#2274BB", orientation = "y") +
    ggplot2::labs(
      x = paste0(etiqueta_etn, "\n"),
      y = paste0("\n", etiqueta_casos),
      caption = fuente_data
    ) +
    ggplot2::scale_x_discrete(labels = stringr::str_wrap(
      etiquetas,
      5
    )) +
    tema_sivirep() +
    return(plot_per_etn)
}

#' @title Generar tabla con la incidencia
#' @description Función que genera la tabla con la incidencia según
#' distribución geográfica.
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados por departamento o municipio.
#' @param col_geo Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene los nombres de los departamentos o municipios
#' en los datos agrupados de la enfermedad o evento; su valor por
#' defecto es `NULL`.
#' @return Una `kable` (tabla gráfica) con la incidencia según
#' distribución geográfica.
#' @examples
#' \donttest{
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' data_agrupada <- agrupar_mpio(data_limpia, dpto = "Antioquia")
#' incidencia_mpios <- calcular_incidencia_geo(
#'   data_agrupada =
#'     data_agrupada
#' )
#' plot_tabla_incidencia_geo(
#'   data_agrupada = incidencia_mpios$data_incidencia,
#'   col_geo = "municipio_ocurrencia"
#' )
#' }
#' @export
plot_tabla_incidencia_geo <- function(data_agrupada,
                                      col_geo = NULL) {
  validar_data_agrupada(data_agrupada)
  nomb_cols <- obtener_tip_ocurren_geo(data_agrupada$nombre_evento[1])
  etiqueta_geo <- "Departamento"
  etiqueta_cod <- obtener_val_config("label_code")
  if (is.null(col_geo)) {
    col_geo <- nomb_cols[1:2]
  }
  if (nomb_cols[3] %in% colnames(data_agrupada) &&
    length(unique(data_agrupada[[nomb_cols[1]]])) == 1) {
    etiqueta_geo <- "Municipio"
    col_geo <- nomb_cols[3:4]
  }
  cond_incidencia <-
    obtener_cond_inciden_event(cod_eve = data_agrupada$cod_eve[1])
  caption_tabla <- obtener_val_config("caption_geo_incidence")
  caption_tabla <- paste0(
    caption_tabla, nomb_cols[5], " por ",
    cond_incidencia$coeficiente,
    " habitantes"
  )
  data_agrupada[[col_geo[2]]] <-
    stringr::str_to_title(data_agrupada[[col_geo[2]]])
  data_tabla <- group_by(
    data_agrupada,
    dplyr::across(dplyr::all_of(c(col_geo, "incidencia")))
  )
  data_tabla <- dplyr::summarise(data_tabla,
    incidencia = sum(.data[["incidencia"]]),
    .groups = "drop"
  )
  data_tabla <- data_tabla[order(data_tabla$incidencia,
    decreasing = TRUE
  ), ]
  tabla_geo <- kableExtra::kbl(data_tabla,
    col.names = c(
      etiqueta_cod,
      etiqueta_geo,
      "Incidencia"
    ),
    align = "c",
    caption = caption_tabla,
    longtable = TRUE
  )
  tabla_geo <- kableExtra::row_spec(tabla_geo, 0,
    color = "white",
    background = "#2274BB"
  )
  tabla_geo <-
    kableExtra::kable_styling(tabla_geo,
      full_width = FALSE,
      latex_options = "HOLD_position"
    )
  return(tabla_geo)
}

#' @title Generar tabla con la incidencia por sexo
#' @description Función que genera la tabla con la incidencia según por
#' sexo.
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados por departamento o municipio.
#' @param col_sex Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene el sexo en los datos agrupados de la enfermedad
#' o evento; su valor por defecto es `"sexo"`.
#' @return Una `kable` (tabla gráfica) con la incidencia por sexo.
#' @examples
#' \donttest{
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' data_agrupada_sex <- agrupar_sex(data_limpia)
#' incidencia_mpios <-
#'   calcular_incidencia_sex(
#'     data_agrupada = data_agrupada_sex,
#'     dpto = "Antioquia"
#'   )
#' plot_tabla_incidencia_sex(
#'   data_agrupada = incidencia_mpios$data_incidencia,
#'   col_sex = "sexo"
#' )
#' }
#' @export
plot_tabla_incidencia_sex <- function(data_agrupada,
                                      col_sex = "sexo") {
  validar_data_agrupada(data_agrupada)
  validar_sex(data_agrupada, col_sex)
  etiqueta_sex <- "Sexo"
  etiqueta_cod <- obtener_val_config("label_code")
  caption_tabla <- obtener_val_config("caption_sex_incidence")
  event_especiales <- obtener_val_config("special_events")
  cond_incidencia <-
    obtener_cond_inciden_event(cod_eve = data_agrupada$cod_eve[1])
  caption_tabla <- paste0(
    caption_tabla, " por ",
    cond_incidencia$coeficiente,
    " habitantes"
  )
  nombre_event <- tolower(data_agrupada$nombre_evento[1])
  data_agrupada[[col_sex]] <-
    stringr::str_to_title(data_agrupada[[col_sex]])
  data_agrupada[["nombre_evento"]] <-
    stringr::str_to_title(data_agrupada[["nombre_evento"]])
  data_agrupada <- data_agrupada[order(data_agrupada$incidencia,
    decreasing = TRUE
  ), ]
  for (evento in event_especiales) {
    if (stringr::str_detect(nombre_event, evento$event)) {
      col_nomb <- rep(
        stringr::str_to_title(evento$event),
        nrow(data_agrupada)
      )
      col_cod <- rep(evento$cod_eve, nrow(data_agrupada))
      data_agrupada[["nombre_evento"]] <- col_nomb
      data_agrupada[["cod_eve"]] <- col_cod
      break
    }
  }
  data_tabla <- data.frame(
    cod_eve = data_agrupada$cod_eve,
    nombre_evento = data_agrupada$nombre_evento,
    sexo = data_agrupada$sexo,
    incidencia = data_agrupada$incidencia
  )
  tabla_sex <- kableExtra::kbl(data_tabla,
    col.names = c(
      etiqueta_cod,
      "Evento",
      etiqueta_sex,
      "Incidencia"
    ),
    align = "c",
    caption = caption_tabla
  )
  tabla_sex <- kableExtra::row_spec(tabla_sex,
    0,
    color = "white",
    background = "#2274BB"
  )
  tabla_sex <- kableExtra::kable_styling(tabla_sex,
    full_width = FALSE,
    latex_options = "HOLD_position"
  )
  return(tabla_sex)
}
