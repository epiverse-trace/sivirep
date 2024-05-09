#' Función que genera el mapa por departamentos o municipios con el número de
#' casos o la incidencia de una enfermedad o evento
#' @param data_agrupada Un `data.frame` que contiene los datos de la enfermedad
#' agrupados por departamento y número de casos
#' @param col_distribucion Un `character` (cadena de caracteres) que contiene el
#' nombre de la columna que tiene los valores de la distribución, por número de
#' casos o incidencia; su valor por defecto es `"incidencia"`
#' @param col_codigos Un `character` (cadena de caracteres) que contiene el
#' nombre de la columna con los códigos de los departamentos o municipios, se
#' utilizan para obtener los poligonos de las áreas geográficas del archivo
#' geoespacial o de figuras (Shapefile); su valor por defecto `NULL`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos de la enfermedad o evento;
#' su valor por defecto `NULL`
#' @param dpto Un `character` (cadena de caracteres) que contiene el
#' nombre del departamento; su valor por defecto `NULL`
#' @param mpio Un `character` (cadena de caracteres) que contiene el
#' nombre del municipio; su valor por defecto `NULL`
#' @return Un `plot` o mapa por departamentos o municipios con el número de
#' casos o incidencia de una enfermedad específica
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_estandar <- estandarizar_geo_cods(data_limpia)
#' # Mapa por departamentos
#' data_espacial <- agrupar_dpto(data_event = data_estandar)
#' plot_map(data_agrupada = data_espacial,
#'          col_distribucion = "casos")
#' # Mapa por municipios de un departamento especifico
#' data_filtrada_dpto <- geo_filtro(data_event = data_estandar,
#'                                  dpto = "Cundinamarca")
#' data_espacial_dpto <- agrupar_mpio(data_event = data_filtrada_dpto)
#' plot_map(data_agrupada = data_espacial_dpto,
#'          col_codigos = "cod_mun_o",
#'          col_distribucion = "casos")
#' # Mapa por municipio especifico
#' data_filtrada_mpio <- geo_filtro(data_event = data_estandar,
#'                                  dpto = "Antioquia",
#'                                  mpio = "Envigado")
#' data_espacial_mpio <- agrupar_mpio(data_event = data_filtrada_mpio)
#' plot_map(data_agrupada = data_espacial_mpio,
#'          col_codigos = "cod_mun_o",
#'          col_distribucion = "casos",
#'          dpto = "Antioquia",
#'          mpio = "Envigado")
#' # Mapa con la incidencia por municipios de un departamento especifico
#' \dontrun{
#' proyecciones <- import_data_incidencia()
#' incidencia_dpto <-
#'     calcular_incidencia_geo(data_incidencia = proyecciones,
#'                             data_agrupada = data_espacial_dpto,
#'                             year = 2020)
#' plot_map(data_agrupada = incidencia_dpto,
#'          col_codigos = "cod_mun_o",
#'          col_distribucion = "incidencia")
#' }
#' @export
plot_map <- function(data_agrupada,
                     col_distribucion = "incidencia",
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
  subtitulo <-  config::get(file =
                              system.file("extdata",
                                          "config.yml",
                                          package = "sivirep"),
                            "label_geo_analysis")
  cols_geo_ocurrencia <- NULL
  nombres_col <- NULL
  etiqueta_relleno <- "Casos"
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
  if (col_distribucion == "incidencia") {
    etiqueta_relleno <- "Incidencia"
    cond_incidencia <-
      obtener_cond_inciden_event(cod_eve = data_agrupada$cod_eve[1])
    etiqueta_relleno <- paste0(etiqueta_relleno, " por \n",
                               cond_incidencia$coeficiente,
                               " habitantes")
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
    dplyr::summarise(casos = sum(.data[[col_distribucion]]), .groups = "drop")
  polygon_seleccionado <- ggplot2::fortify(polygon_seleccionado, region = "id")
  polygon_seleccionado$indice <- seq_len(nrow(polygon_seleccionado))
  polygon_seleccionado <- polygon_seleccionado %>%
    dplyr::left_join(data_agrupada, by = "id")
  polygon_seleccionado <-
    cbind(polygon_seleccionado,
          sf::st_coordinates(sf::st_centroid(polygon_seleccionado$geometry)))
  map <- ggplot2::ggplot(polygon_seleccionado) +
    ggplot2::ggtitle(label = titulo, subtitle = subtitulo) +
    ggplot2::geom_sf(data = polygon_seleccionado,
                     ggplot2::aes(fill = .data$casos)) +
    ggplot2::scale_fill_continuous(low = "#fcebfc", high = "#be0000",
                                   guide = "colorbar", na.value = "white") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,
                                                      face = "bold"),
                   plot.subtitle = ggplot2::element_text(hjust = 0.5,
                                                         face = "bold"),
                   text = ggplot2::element_text(size = 14),
                   legend.title = ggplot2::element_text(face = "bold")) +
    ggplot2::labs(caption = fuente_data, fill = etiqueta_relleno)
  return(map)
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
#' @param col_fecha Un `character` (cadena de caracteres) que contiene el
#' nombre de la columna con las fechas de notificación en los datos de la
#' enfermedad o evento agrupados; su valor por defecto es `"ini_sin"`
#' @param tipo Un `character` (cadena de caracteres) que contiene el tipo de
#' gráfico (`"barras"` o `"tendencia"`); su valor por defecto es `"barras"`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de la distribución de casos por fecha de inicio
#' de síntomas
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_fecha_inisintomas(
#'                                      data_event = data_limpia)
#' plot_fecha_inisintomas(data_agrupada = data_agrupada,
#'                        col_fecha = "ini_sin",
#'                        uni_marca = "semanaepi")
#' @export
plot_fecha_inisintomas <- function(data_agrupada,
                                   col_fecha = "ini_sin",
                                   uni_marca = "semanaepi",
                                   tipo = "barras",
                                   fuente_data = NULL) {
  stopifnot("El parametro data_agrupada es obligatorio" =
              !missing(data_agrupada),
            "El parametro data_agrupada debe ser un data.frame" =
              is.data.frame(data_agrupada),
            "El parametro data_agrupada no debe estar vacio" =
              nrow(data_agrupada) > 0,
            "El parametro col_fecha debe ser una cadena de caracteres" =
              is.character(col_fecha),
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
  var_x <- col_fecha
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
  if (is.null(col_fecha)) {
    col_fecha <- fechas_column_nombres[3]
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
  etiqueta_fecha <- config::get(file =
                                  system.file("extdata",
                                              "config.yml",
                                              package = "sivirep"),
                                "label_date_ini")
  etiqueta_x <- paste0("\n", etiqueta_fecha, " por ", uni_marca, "\n")
  etiqueta_casos <- config::get(file =
                                  system.file("extdata",
                                              "config.yml",
                                              package = "sivirep"),
                                "label_cases")
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
                  y = paste0(etiqueta_casos, "\n"),
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

#' Generar gráfico de distribución de casos por sexo
#'
#' Función que genera el gráfico de distribución de casos por sexo
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados
#' @param col_sex Un `character` (cadena de caracteres) con el nombre de la
#' columna que contiene el sexo en los datos agrupados de la enfermedad o
#' evento; su valor por defecto es `"sexo"`
#' @param col_distribucion Un `character` (cadena de caracteres) que contiene el
#' nombre de la columna que tiene los valores de la distribución, por número de
#' casos o incidencia; su valor por defecto es `"incidencia"`
#' @param porcentaje Un `boolean` (TRUE/FALSE) que indica si los datos
#' tienen porcentajes; su valor por defecto es `TRUE`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de distribución de casos por sexo
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_sex(data_event = data_limpia,
#'                              porcentaje = TRUE)
#' plot_sex(data_agrupada = data_agrupada,
#'          col_sex = "sexo",
#'          porcentaje = TRUE)
#' @export
plot_sex <- function(data_agrupada,
                     col_sex = "sexo",
                     col_distribucion = "casos",
                     porcentaje = TRUE,
                     fuente_data = NULL) {
  stopifnot("El parametro data_agrupada es obligatorio" =
              !missing(data_agrupada),
            "El parametro data_agrupada debe ser un data.frame" =
              is.data.frame(data_agrupada),
            "El parametro data_agrupada no debe estar vacio" =
              nrow(data_agrupada) > 0,
            "El parametro col_sex debe ser una cadena de caracteres" =
              is.character(col_sex),
            "El parametro porcentaje debe ser un booleano" =
              is.logical(porcentaje))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  stopifnot("El parametro fuente_data debe ser un cadena de caracteres"
            = is.character(fuente_data))
  etiqueta_eje <- NULL
  if (col_distribucion == "casos") {
    etiqueta_eje <- config::get(file =
                                    system.file("extdata",
                                                "config.yml",
                                                package = "sivirep"),
                                "label_cases")
  } else {
    etiqueta_eje <- config::get(file =
                                    system.file("extdata",
                                                "config.yml",
                                                package = "sivirep"),
                                "label_incidence")
  }
  if (porcentaje) {
    plot_casos_sex <- ggplot2::ggplot(data_agrupada,
                                      ggplot2::aes(x =
                                                     .data[[col_sex]],
                                                   y =
                                                     .data[[col_distribucion]],
                                                   fill = .data[[col_sex]])) +
      ggplot2::geom_bar(width = 0.5,
                        stat = "identity") +
      ggplot2::labs(x = "\nSexo\n", y = paste0(etiqueta_eje, "\n"),
                    caption = fuente_data) +
      ggplot2::theme_classic() +
      ggplot2::geom_text(ggplot2::aes(label = paste0(.data[[col_distribucion]],
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
  } else {
    plot_casos_sex <- ggplot2::ggplot(data_agrupada,
                                      ggplot2::aes(x =
                                                     .data[[col_sex]],
                                                   y =
                                                     .data[[col_distribucion]],
                                                   fill = .data[[col_sex]])) +
      ggplot2::geom_bar(width = 0.5,
                        stat = "identity") +
      ggplot2::labs(x = "\nSexo\n", y = paste0(etiqueta_eje, "\n"),
                    caption = fuente_data) +
      ggplot2::theme_classic() +
      ggplot2::geom_text(ggplot2::aes(label = .data[[col_distribucion]]),
                         vjust = 1.5,
                         color = "black",
                         hjust = 0.5) +
      obtener_estetica_escala(escala = 2, nombre = "Sexo") +
      tema_sivirep() +
      ggplot2::facet_wrap(facets = ~nombre_evento,
                          scales = "free_y",
                          ncol = 2)
  }
  return(plot_casos_sex)
}

#' Generar gráfico de distribución de casos por sexo y semana epidemiológica
#'
#' Función que genera el gráfico de distribución de casos por sexo
#' y semana epidemiológica
#' @param data_agrupada Un `data.frame` que contiene los datos de la enfermedad
#' o evento agrupados
#' @param col_sex Un `character` (cadena de caracteres) con el nombre de la
#' columna que contiene el sexo en los datos agrupados de la enfermedad o
#' evento; su valor por defecto es `"sexo"`
#' @param col_semanaepi Un `character` (cadena de caracteres) con el nombre de
#' la columna que contiene las semanas epidemiológicas en los datos agrupados
#' de la enfermedad o evento; su valor por defecto es `"semana"`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de distribución de casos por sexo y semana
#' epidemiológica
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_sex_semanaepi(data_event = data_limpia)
#' plot_sex_semanaepi(data_agrupada = data_agrupada,
#'                    col_sex = "sexo",
#'                    col_semanaepi = "semana")
#' @export
plot_sex_semanaepi <- function(data_agrupada,
                               col_sex = "sexo",
                               col_semanaepi = "semana",
                               fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada),
            "El parametro col_sexo debe ser una cadena de caracteres" =
              is.character(col_sex),
            "El parametro col_semanaepi debe ser una cadena de caracteres" =
              is.character(col_semanaepi))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  max_casos <- max(data_agrupada$casos)
  data_agrupada$semana <- as.numeric(data_agrupada$semana)
  max_semana <- max(data_agrupada$semana)
  etiqueta_episemana <- config::get(file =
                                  system.file("extdata",
                                              "config.yml",
                                              package = "sivirep"),
                                "label_epiweek")
  etiqueta_casos <- config::get(file =
                                  system.file("extdata",
                                              "config.yml",
                                              package = "sivirep"),
                                "label_cases")
  plot_casos_sex_semanaepi <-
    ggplot2::ggplot(data_agrupada,
                    ggplot2::aes(x = .data[[col_semanaepi]],
                                 y = .data[["casos"]],
                                 fill = .data[[col_sex]])) +
    ggplot2::geom_bar(width = 0.5,
                      stat = "identity") +
    ggplot2::labs(x = paste0("\n", etiqueta_episemana, "\n"),
                  y = paste0(etiqueta_casos, "\n"),
                  caption = fuente_data) + {
    if (max_casos < 6)
      ggplot2::scale_y_continuous(breaks = seq(0, max_casos, 1))
    } +
    ggplot2::scale_x_continuous(breaks = seq(1, max_semana, 1)) +
    ggplot2::theme_classic() +
    obtener_estetica_escala(escala = 2, nombre = "Sexo") +
    tema_sivirep() +
    ggplot2::facet_wrap(facets = ~nombre_evento,
                        scales = "free_y",
                        ncol = 1) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(
      angle = 90,
      hjust = 1))
  return(plot_casos_sex_semanaepi)
}

#' Generar gráfico de distribución de casos por edad
#'
#' Función que genera el gráfico de distribución de casos por edad
#' @param data_agrupada Un `data.frame` que contiene los datos de la enfermedad
#' o evento agrupados
#' @param col_edad Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene las edades en los datos agrupados de la enfermedad o
#' evento; su valor por defecto es `"edad"`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de distribución de casos por edad
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_edad(data_event = data_limpia)
#' plot_edad(data_agrupada = data_agrupada,
#'           col_edad = "edad")
#' @export
plot_edad <- function(data_agrupada,
                      col_edad = "edad",
                      fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada),
            "El parametro col_edad debe ser una cadena de caracteres"
            = is.character(col_edad))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  etiqueta_casos <- config::get(file =
                                  system.file("extdata",
                                              "config.yml",
                                              package = "sivirep"),
                                "label_cases")
  plot_casos_edad <-
    ggplot2::ggplot(data_agrupada,
                    ggplot2::aes(x = .data[[col_edad]],
                                 y = .data[["casos"]])) +
    ggplot2::geom_bar(width = 0.7,
                      stat = "identity",
                      fill = "#2274BB") +
    ggplot2::labs(x = "\nGrupo de edad\n",
                  y = paste0(etiqueta_casos, "\n"),
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
#' @param col_edad Un `character` (cadena de caracteres) con el nombre de la
#' columna que contiene las edades en los datos agrupados de la enfermdedad o
#' evento; su valor por defecto es `"edad`
#' @param col_sex Un `character` (cadena de caracteres) con el nombre de la
#' columna que contiene el sexo en los datos agrupados de la enfermdedad o
#' evento; su valor por defecto es `"sexo`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de distribución de casos por edad y sexo
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_edad_sex(data_event = data_limpia)
#' plot_edad_sex(data_agrupada = data_agrupada,
#'               col_edad = "edad",
#'               col_sex = "sexo")
#' @export
plot_edad_sex <- function(data_agrupada,
                          col_edad = "edad",
                          col_sex = "sexo",
                          fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada),
            "El parametro col_edad debe ser una cadena de caracteres"
            = is.character(col_edad),
            "El parametro col_sex debe ser una cadena de caracteres"
            = is.character(col_sex))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  etiqueta_casos <- config::get(file =
                                  system.file("extdata",
                                              "config.yml",
                                              package = "sivirep"),
                                "label_cases")
  plot_casos_edad_sexo <-
    ggplot2::ggplot(data_agrupada,
                    ggplot2::aes(x = .data[[col_edad]],
                                 y = .data[["casos"]],
                                 fill = .data[[col_sex]])) +
    ggplot2::geom_bar(width = 0.7,
                      stat = "identity") +
    ggplot2::labs(x = "\nGrupo de edad\n", y = paste0(etiqueta_casos, "\n"),
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
#' enfermedad o evento agrupados por departamentos
#' @param col_dptos Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene los departamenos en los datos agrupados de la
#' enfermedad o evento; su valor por defecto es `NULL`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de distribución de casos por departamentos
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_limpia <- estandarizar_geo_cods(data_limpia)
#' data_agrupada <- agrupar_dpto(data_event = data_limpia)
#' plot_dptos(data_agrupada,
#'            col_dptos = "departamento_ocurrencia")
#' @export
plot_dptos <- function(data_agrupada,
                       col_dptos = NULL,
                       fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  if (is.null(col_dptos)) {
    cols_geo_ocurrencia <-
      obtener_tip_ocurren_geo(nombre_event =
                                data_agrupada[["nombre_evento"]][1])
    if (length(cols_geo_ocurrencia) > 1) {
      col_dptos <- cols_geo_ocurrencia[2]
    }
  } else {
    stopifnot("El parametro col_dptos debe ser una cadena de caracteres"
              = is.character(col_dptos))
  }
  etiqueta_casos <- config::get(file =
                                  system.file("extdata",
                                              "config.yml",
                                              package = "sivirep"),
                                "label_cases")
  num_eventos <- length(unique(data_agrupada[["nombre_evento"]]))
  pos_leyenda <- ggplot2::theme(legend.position = "right")
  if (num_eventos > 3) {
    pos_leyenda <- ggplot2::theme(legend.position = "bottom")
  }
  data_agrupada <- data_agrupada %>%
    group_by_at(col_dptos) %>%
    dplyr::summarise(casos = sum(.data[["casos"]]), .groups = "drop")
  plot_casos_dptos <-
    ggplot2::ggplot(data_agrupada,
                    ggplot2::aes(x = stats::reorder(.data[[col_dptos]],
                                                    .data[["casos"]]),
                                 y = .data[["casos"]])) +
    ggplot2::geom_bar(width = 0.5,
                      stat = "identity",
                      fill = "#2274BB") +
    ggplot2::labs(x = "\nDepartamento\n",
                  y = paste0(etiqueta_casos, "\n"),
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
#' enfermedad o evento agrupados por municipios
#' @param col_mpios Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene los municipios en los datos agrupados de la
#' enfermedad o evento; su valor por defecto es `NULL`
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
#'            col_mpios = "municipio_ocurrencia")
#' @export
plot_mpios <- function(data_agrupada,
                       col_mpios = NULL,
                       fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  if (is.null(col_mpios)) {
    cols_geo_ocurrencia <-
      obtener_tip_ocurren_geo(nombre_event =
                                data_agrupada[["nombre_evento"]][1])
    if (length(cols_geo_ocurrencia) > 1) {
      col_mpios <- cols_geo_ocurrencia[4]
    }
  } else {
    stopifnot("El parametro col_mpios debe ser una cadena de caracteres"
              = is.character(col_mpios))
  }
  etiqueta_casos <- config::get(file =
                                  system.file("extdata",
                                              "config.yml",
                                              package = "sivirep"),
                                "label_cases")
  num_eventos <- length(unique(data_agrupada[["nombre_evento"]]))
  pos_leyenda <- ggplot2::theme(legend.position = "right")
  if (num_eventos > 3) {
    pos_leyenda <- ggplot2::theme(legend.position = "bottom")
  }
  data_agrupada <- data_agrupada %>%
    group_by_at(col_mpios) %>%
    dplyr::summarise(casos = sum(.data[["casos"]]), .groups = "drop")
  plot_casos_muns <-
    ggplot2::ggplot(data_agrupada,
                    ggplot2::aes(x = stats::reorder(.data[[col_mpios]],
                                                    .data[["casos"]]),
                                 y = .data[["casos"]])) +
    ggplot2::geom_bar(width = 0.5,
                      stat = "identity",
                      fill = "#2274BB") +
    ggplot2::labs(x = "\nMunicipio\n",
                  y = paste0(etiqueta_casos, "\n"),
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
#' @param col_area Un `character` (cadena de carácteres) con el nombre de
#' la columna con el área geografica en los datos agrupados de la enfermedad
#' o evento; su valor por defecto es `"area"`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de distribución de casos por área geográfica
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_area_geo(data_event = data_limpia)
#' plot_area_geo(data_agrupada,
#'               col_area = "area")
#' @export
plot_area_geo <- function(data_agrupada,
                          col_area = "area",
                          fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada),
            "El parametro col_area debe ser una cadena de caracteres"
            = is.character(col_area))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  etiquetas_areas <- config::get(file =
                                   system.file("extdata",
                                               "config.yml",
                                               package = "sivirep"),
                                 "labels_geo_areas")
  etiqueta_casos <- config::get(file =
                                  system.file("extdata",
                                              "config.yml",
                                              package = "sivirep"),
                                "label_cases")
  etiqueta_area_geo <- config::get(file =
                                     system.file("extdata",
                                                 "config.yml",
                                                 package = "sivirep"),
                                   "label_geo_area")
  pos_leyenda <- ggplot2::theme(legend.position = "right")
  plot_casos_area <-
    ggplot2::ggplot(data_agrupada,
                    ggplot2::aes(x = .data[[col_area]],
                                 y = .data[["casos"]])) +
    ggplot2::geom_bar(stat = "identity",
                      fill = "#2274BB") +
    ggplot2::labs(x = paste0("\n", etiqueta_area_geo),
                  y = paste0(etiqueta_casos, "\n"),
                  caption = fuente_data) +
    ggplot2::theme_classic() +
    tema_sivirep() +
    ggplot2::scale_x_discrete(labels = etiquetas_areas) +
    pos_leyenda
  return(plot_casos_area)
}

#' Generar gráfico de distribución de casos por área geográfica a nivel
#' departamental o municipal
#'
#' Función que genera el gráfico de casos por área geográfica a nivel
#' departamental o municipal
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados
#' @param col_area Un `character` (cadena de carácteres) con el nombre de
#' la columna con el área geografica en los datos agrupados de la enfermedad
#' o evento; su valor por defecto es `"area"`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de distribución de casos por área geográfica
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_top_area_geo(data_event = data_limpia,
#'                                       dpto = "Antioquia")
#' plot_top_area_geo(data_agrupada,
#'               col_area = "area")
#' @export
plot_top_area_geo <- function(data_agrupada,
                          col_area = "area",
                          fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada),
            "El parametro col_area debe ser una cadena de caracteres"
            = is.character(col_area))
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
  etiqueta_casos <- config::get(file =
                                  system.file("extdata",
                                              "config.yml",
                                              package = "sivirep"),
                                "label_cases")
  etiqueta_area_geo <- config::get(file =
                                     system.file("extdata",
                                                 "config.yml",
                                                 package = "sivirep"),
                                   "label_geo_area")
  nomb_cols <- NULL
  if (length(cols_geo_ocurrencia) > 1) {
      if (length(names(data_agrupada)) > 5) {
        nomb_cols <- append(col_area, cols_geo_ocurrencia[4])
      } else {
        nomb_cols <- append(col_area, cols_geo_ocurrencia[2])
      }
  }
  pos_leyenda <- ggplot2::theme(legend.position = "right")
  data_agrupada_area <- data_agrupada %>%
    group_by_at(nomb_cols) %>%
    dplyr::summarise(casos = sum(.data[["casos"]]), .groups = "drop")
  plot_casos_area <-
    ggplot2::ggplot(data_agrupada_area,
                    ggplot2::aes(x = .data[[nomb_cols[2]]],
                                 y = .data[["casos"]],
                                 fill = .data[[nomb_cols[1]]])) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(x = "\nDepartamento\n",
                  y = paste0("\n", etiqueta_casos),
                  caption = fuente_data) +
    ggplot2::theme_classic() +
    obtener_estetica_escala(escala = 3,
                            nombre = paste0(etiqueta_area_geo, "\n"),
                            etiquetas = etiquetas_areas) +
    tema_sivirep() +
    pos_leyenda +
    ggplot2::coord_flip()
  return(plot_casos_area)
}

#' Generar tabla con la distribución de casos por tipo de
#' enfermedad o evento
#'
#' Función que genera la tabla con la distribución de casos por
#' tipo de enfermedad o evento
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados por tipo
#' @param col_event Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene el tipo de evento en los datos agrupados de
#' la enfermedad o evento; su valor por defecto es `"nombre_evento"`
#' @return Una `kable` (tabla gráfica) con la distribución de casos
#' por tipo de enfermedad o evento
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_eventos(data_event = data_limpia,
#'                                  col_event = "cod_eve")
#' plot_tabla_tipos_event(data_agrupada,
#'                        col_event = "nombre_evento")
#' @export
plot_tabla_tipos_event <- function(data_agrupada,
                                   col_event = "nombre_evento") {
  etiqueta_cod <- config::get(file =
                              system.file("extdata",
                                          "config.yml",
                                          package = "sivirep"),
                              "label_code")
  caption_tabla <- config::get(file =
                                 system.file("extdata",
                                             "config.yml",
                                             package = "sivirep"),
                               "caption_table_events")
  data_agrupada[[col_event]] <-
    stringr::str_to_title(data_agrupada[[col_event]])
  tabla_tipos <- knitr::kable(data_agrupada[, c("cod_eve",
                                                col_event,
                                                "casos")],
                              col.names = c(etiqueta_cod,
                                            "Evento", "Casos"),
                              align = "c",
                              caption = caption_tabla) %>%
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
#' @param col_year Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene los años en los datos agrupados de la enfermedad
#' o evento por año; su valor por defecto es `"ano"`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de distribución de casos por año
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_years(data_event = data_limpia)
#' plot_years(data_agrupada,
#'            col_year = "ano")
#' \dontrun{
#' data_years <- import_data_event(nombre_event = "DENGUE",
#'                                 years = seq(2007, 2020))
#' data_limpia <- limpiar_data_sivigila(data_years)
#' data_agrupada <- agrupar_years(data_event = data_limpia)
#' plot_years(data_agrupada,
#'            col_year = "ano")
#' }
#' @export
plot_years <- function(data_agrupada,
                       col_year = "ano",
                       fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada),
            "El parametro col_year debe ser una cadena de caracteres"
            = is.character(col_year))
  etiqueta_year <- config::get(file =
                             system.file("extdata",
                                         "config.yml",
                                         package = "sivirep"),
                             "label_year")
  etiqueta_casos <- config::get(file =
                                 system.file("extdata",
                                             "config.yml",
                                             package = "sivirep"),
                               "label_cases")
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  eventos <- length(unique(data_agrupada[["nombre_evento"]]))
  plot_casos_years <-
    ggplot2::ggplot(data_agrupada,
                    ggplot2::aes(x = .data[[col_year]],
                                 y = .data[["casos"]],
                                 fill = .data[["nombre_evento"]])) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(x = paste0("\n", etiqueta_year, "\n"),
                  y = paste0(etiqueta_casos, "\n"),
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
#' Función que genera el gráfico de distribución por la clasificación
#' inicial de los casos
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados por la clasificación inicial de los casos
#' @param col_tipo Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene la clasificación inicial de los casos en los
#' datos agrupados de la enfermedad o evento; su valor por defecto es
#' `"tip_cas"`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de distribución de casos por la clasificación
#' inicial
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_tipo_caso(data_event = data_limpia)
#' plot_tipo_caso(data_agrupada,
#'                col_tipo = "tip_cas")
#' @export
plot_tipo_caso <- function(data_agrupada,
                           col_tipo = "tip_cas",
                           fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada),
            "El parametro col_tipo debe ser una cadena de caracteres"
            = is.character(col_tipo))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  nomb_cols <- c(col_tipo, "nombre_evento")
  etiquetas <- config::get(file =
                             system.file("extdata",
                                         "config.yml",
                                         package = "sivirep"),
                           "labels_cas_tip")
  etiqueta_casos <- config::get(file =
                                  system.file("extdata",
                                              "config.yml",
                                              package = "sivirep"),
                                "label_cases")
  etiqueta_tipo <- config::get(file =
                                  system.file("extdata",
                                              "config.yml",
                                              package = "sivirep"),
                                "label_type_case")
  clasificacion <- unique(data_agrupada[[nomb_cols[1]]])
  escala <- length(unique(data_agrupada[[nomb_cols[2]]]))
  etiquetas <- etiquetas[as.character(clasificacion)]
  etiquetas <- unlist(etiquetas)
  plot_tipo_casos <-
    ggplot2::ggplot(data_agrupada,
                    ggplot2::aes(x = .data[[nomb_cols[1]]],
                                 y = .data[["casos"]],
                                 fill = .data[[nomb_cols[2]]])) +
    ggplot2::geom_bar(stat = "identity", width = 0.5) +
    ggplot2::labs(x = paste0("\n", etiqueta_tipo, "\n"),
                  y = paste0(etiqueta_casos, "\n"),
                  caption = fuente_data) +
    ggplot2::theme_classic() +
    obtener_estetica_escala(escala = escala, nombre = "Eventos\n") +
    ggplot2::scale_x_discrete(labels = etiquetas)
    tema_sivirep() +
    ggplot2::theme(legend.position = "right")
  return(plot_tipo_casos)
}

#' Generar gráfico de distribución de casos por la clasificacion inicial
#' del caso y los años seleccionados
#'
#' Función que genera el gráfico por la clasificación inicial de los
#' casos y los años seleccionados
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento, agrupados por la clasificación inicial y los años
#' seleccionados
#' @param col_tipo Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene la clasificación inicial del caso en los datos
#' agrupados de la enfermedad o evento; su valor por defecto es `"tip_cas"`
#' @param col_year Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene el año en los datos agrupados de la enfermedad
#' o evento; su valor por defecto es `"ano"`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de distribución de casos por la clasificación
#' inicial y los años seleccionados
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_tipo_caso(data_event = data_limpia,
#'                                    cols_tipo = c("tip_cas",
#'                                                  "ano"))
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
  etiqueta_year <- config::get(file =
                                 system.file("extdata",
                                             "config.yml",
                                             package = "sivirep"),
                               "label_year")
  etiqueta_casos <- config::get(file =
                                  system.file("extdata",
                                              "config.yml",
                                              package = "sivirep"),
                                "label_cases")
  etiqueta_tipo <- config::get(file =
                                 system.file("extdata",
                                             "config.yml",
                                             package = "sivirep"),
                               "label_type_case")
  clasificacion <- unique(data_agrupada[[col_tipo]])
  escala <- length(unique(data_agrupada[[col_tipo]]))
  etiquetas <- etiquetas[as.character(clasificacion)]
  etiquetas <- unlist(etiquetas)
  plot_casos_years <-
    ggplot2::ggplot(data_agrupada,
                    ggplot2::aes(x = .data[[col_year]],
                                 y = .data[["casos"]],
                                 fill = .data[[col_tipo]])) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(x = paste0("\n", etiqueta_year, "\n"),
                  y = paste0(etiqueta_casos, "\n"),
                  caption = fuente_data) +
    ggplot2::theme_classic() +
    obtener_estetica_escala(escala = escala,
                            nombre = paste0(etiqueta_tipo, "\n"),
                            etiquetas = etiquetas) +
    tema_sivirep() +
    ggplot2::theme(legend.position = "right")
  return(plot_casos_years)
}

#' Generar gráfico de distribución de casos por la pertenencia étnica
#'
#' Función que genera el gráfico de la distribución de casos por la
#' pertenencia étnica
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados por la pertenencia étnica
#' @param col_etn Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene la pertenencia étnica en los datos agrupados de
#' la enfermedad o evento; su valor por defecto es `"per_etn"`
#' @param porcentaje Un `boolean` (TRUE/FALSE) que indica si los datos
#' tienen porcentajes; su valor por defecto es `TRUE`
#' @param fuente_data Un `character` (cadena de caracteres) que contiene la
#' leyenda o fuente de información de los datos; su valor por defecto es `NULL`
#' @return Un `plot` o gráfico de la distribución de casos por la pertenencia
#' étnica
#' @examples
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(dengue2020)
#' data_agrupada <- agrupar_per_etn(data_event = data_limpia)
#' plot_per_etn(data_agrupada,
#'              col_etn = "per_etn")
#' @export
plot_per_etn <- function(data_agrupada,
                         col_etn = "per_etn",
                         porcentaje = TRUE,
                         fuente_data = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada),
            "El parametro col_etn debe ser una cadena de caracteres"
            = is.character(col_etn))
  if (is.null(fuente_data)) {
    fuente_data <-
      "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia"
  }
  etiqueta_casos <- config::get(file =
                                  system.file("extdata",
                                              "config.yml",
                                              package = "sivirep"),
                                "label_cases")
  etiqueta_etn <- config::get(file =
                                  system.file("extdata",
                                              "config.yml",
                                              package = "sivirep"),
                                "label_etn_groups")
  etiquetas <- config::get(file =
                             system.file("extdata",
                                         "config.yml",
                                         package = "sivirep"),
                           "labels_per_etn")
  grupos <- unique(data_agrupada[[col_etn]])
  etiquetas <- etiquetas[as.character(grupos)]
  etiquetas <- unlist(etiquetas)
  data_agrupada <- agrupar_cols_casos(data_event = data_agrupada,
                                      nomb_cols = col_etn,
                                      porcentaje = TRUE,
                                      estandar = FALSE)
  plot_per_etn <-
    ggplot2::ggplot(data_agrupada,
                    ggplot2::aes(x = .data[[col_etn]],
                                 y = .data[["casos"]])) +
    ggplot2::geom_bar(stat = "identity", width = 0.5,
                      fill = "#2274BB") +
    ggplot2::labs(x = paste0(etiqueta_etn, "\n"),
                  y = paste0("\n", etiqueta_casos),
                  caption = fuente_data) + {
                    if (porcentaje) {
      ggplot2::geom_text(ggplot2::aes(label = paste0(.data[["porcentaje"]],
                                                     "%\n")),
                         vjust = 0.8,
                         color = "black",
                         hjust = 0)
      }
    } +
    ggplot2::theme_classic() +
    ggplot2::scale_x_discrete(labels = etiquetas) +
    tema_sivirep() +
    ggplot2::coord_flip()
  return(plot_per_etn)
}

#' Generar tabla con la incidencia
#'
#' Función que genera la tabla con la incidencia según
#' distribución geográfica
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados por departamento o municipio
#' @param col_geo Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene los nombres de los departamentos o municipios
#' en los datos agrupados de la enfermedad o evento; su valor por
#' defecto es `NULL`
#' @return Una `kable` (tabla gráfica) con la incidencia según
#' distribución geográfica
#' @examples
#' \dontrun{
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' proyecciones <- import_data_incidencia()
#' data_agrupada <- agrupar_mpio(data_limpia, dpto = "Antioquia")
#' incidencia_mpios <- calcular_incidencia_geo(
#'                         data_incidencia = proyecciones,
#'                         data_agrupada = data_agrupada,
#'                         year = 2020)
#  plot_tabla_incidencia_geo(data_agrupada = incidencia_mpios,
#'                           col_geo = "municipio_ocurrencia")
#' }
#' @export
plot_tabla_incidencia_geo <- function(data_agrupada,
                                      col_geo = NULL) {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada))
  nomb_cols <- obtener_tip_ocurren_geo(data_agrupada$nombre_evento[1])
  etiqueta_geo <- "Departamento"
  etiqueta_cod <- config::get(file =
                                system.file("extdata",
                                            "config.yml",
                                            package = "sivirep"),
                              "label_code")
  if (is.null(col_geo)) {
    col_geo <- nomb_cols[1:2]
  }
  if (nomb_cols[3] %in% colnames(data_agrupada) &&
      length(unique(data_agrupada[[nomb_cols[1]]])) == 1) {
    etiqueta_geo <- "Municipio"
    col_geo <- nomb_cols[3:4]
  }
  caption_tabla <- config::get(file =
                                 system.file("extdata",
                                             "config.yml",
                                             package = "sivirep"),
                               "caption_geo_incidence")
  data_agrupada[[col_geo[2]]] <-
    stringr::str_to_title(data_agrupada[[col_geo[2]]])
  data_tabla <- data_agrupada %>%
    group_by_at(c(col_geo, "incidencia")) %>%
    dplyr::summarise(incidencia = sum(.data[["incidencia"]]),
                     .groups = "drop")
  data_tabla <- data_tabla[order(data_tabla$incidencia,
                                   decreasing = TRUE), ]
  tabla_geo <- knitr::kable(data_tabla,
                            col.names = c(etiqueta_cod,
                                          etiqueta_geo,
                                          "Incidencia"),
                            align = "c",
                            caption = caption_tabla) %>%
    kableExtra::row_spec(0, color = "white", background = "#2274BB") %>%
    kableExtra::kable_styling(full_width = FALSE,
                              latex_options = "HOLD_position")
  return(tabla_geo)
}

#' Generar tabla con la incidencia por sexo
#'
#' Función que genera la tabla con la incidencia según por sexo
#' @param data_agrupada Un `data.frame` que contiene los datos de la
#' enfermedad o evento agrupados por departamento o municipio
#' @param col_sex Un `character` (cadena de carácteres) con el nombre de
#' la columna que contiene el sexo en los datos agrupados de la enfermedad
#' o evento; su valor por defecto es `"sexo"`
#' @return Una `kable` (tabla gráfica) con la incidencia por sexo
#' @examples
#' \dontrun{
#' data(dengue2020)
#' data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
#' proyecciones <- import_data_incidencia()
#' data_agrupada <- agrupar_sex(data_limpia)
#' incidencia_mpios <- calcular_incidencia_sex(
#'                         data_incidencia = proyecciones,
#'                         data_agrupada = data_agrupada,
#'                         dpto = "Antioquia",
#'                         year = 2020)
#  plot_tabla_incidencia_sex(data_agrupada = incidencia_mpios,
#'                           col_sex = "sexo")
#' }
#' @export
plot_tabla_incidencia_sex <- function(data_agrupada,
                                      col_sex = "sexo") {
  stopifnot("El parametro data_agrupada debe ser un data.frame"
            = is.data.frame(data_agrupada),
            "El parametro col_sex debe ser una cadena de caracteres"
            = is.character(col_sex))
  etiqueta_sex <- "Sexo"
  etiqueta_cod <- config::get(file =
                                system.file("extdata",
                                            "config.yml",
                                            package = "sivirep"),
                              "label_code")
  caption_tabla <- config::get(file =
                                 system.file("extdata",
                                             "config.yml",
                                             package = "sivirep"),
                               "caption_sex_incidence")
  data_agrupada[[col_sex]] <-
    stringr::str_to_title(data_agrupada[[col_sex]])
  data_agrupada[["nombre_evento"]] <-
    stringr::str_to_title(data_agrupada[["nombre_evento"]])
  data_agrupada <- data_agrupada[order(data_agrupada$incidencia,
                                 decreasing = TRUE), ]
  tabla_sex <- knitr::kable(data_agrupada[, c("cod_eve",
                                              "nombre_evento",
                                              col_sex,
                                              "incidencia")],
                             col.names = c(etiqueta_cod,
                                           "Evento",
                                           etiqueta_sex,
                                           "Incidencia"),
                             align = "c",
                             caption = caption_tabla) %>%
    kableExtra::row_spec(0, color = "white", background = "#2274BB") %>%
    kableExtra::kable_styling(full_width = FALSE,
                              latex_options = "HOLD_position")
  return(tabla_sex)
}
