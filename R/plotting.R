#' Plot Epiweek
#'
#' Función que genera la gráfica de la Semana epidemiológica
#' Function that generates the graphic of the Epidemiological Week
#' @param dat Disease data
#' @param col_week  Column Name that indicates the week in the data
#' @param col_cases Column Name hat indicates the cases number in the data
#' @param year Year of the data
#' @param type Type for axis x (week, date)
#' @param xlabel Label of axis x
#' @param ylabel Label of axis y
#' @return The graphic of Epidemiological Week
#' @examples
#' sivigila_summary_data <- import_sivigila_summary_data()
#' data <- filter_disease("MALAR", sivigila_summary_data)
#' plot_epiweek(data, col_week = "SEMANA", col_cases = "conteo_casos", 
#' year = 2019, type = "date", xlabel = "Epidemiological week date")
#' plot_epiweek(data, col_week = "SEMANA", col_cases = "conteo_casos", 
#' year = 2019, type = "week", xlabel = "Epidemiological week date")
#' plot_epiweek(data, col_week = "SEMANA", col_cases = "conteo_casos", 
#' year = 2019, xlabel = "Epidemiological week date")
#' @export
plot_epiweek <- function(dat, col_week, col_cases, year, type = "week", xlabel = "Semana epidemiológica", ylabel = "Número de casos por semana") {
  dat$epiweek <- dat[,col_week]
  dat$cases_count <- dat[,col_cases]
  dat_plot <- dat %>% dplyr::group_by(.data$epiweek, .data$Nombre) %>% dplyr::summarise(casos = sum(.data$cases_count), .groups = "drop")

  if (type == "week") {
      plot <- ggplot2::ggplot(dat_plot) +
        ggplot2::geom_col(ggplot2::aes(x = .data$epiweek, y = .data$casos, fill = .data$Nombre), alpha = 0.9) +
        ggplot2::theme_classic() +
        ggplot2::xlab(xlabel) + ggplot2::ylab(ylabel) +
        ggplot2::scale_fill_discrete(name = "") +
        ggplot2::theme(legend.position = "bottom")
  }

  if (type == "date") {
    dat_plot$date_week <- as.Date(paste(year, dat_plot$epiweek, 1, sep = "-"), "%Y-%U-%u")
    plot <- ggplot2::ggplot(dat_plot) +
      ggplot2::geom_col(ggplot2::aes(x = .data$date_week, y = .data$casos, fill = .data$Nombre), alpha = 0.9) +
      ggplot2::theme_classic() +
      ggplot2::xlab(xlabel) + ggplot2::ylab(ylabel) +
      ggplot2::scale_fill_discrete(name = "") +
      ggplot2::theme(legend.position = "bottom")
  }

  return(plot)
}

#' Plot Department Map
#'
#' Función que genera el mapa por departamento con los datos de una enfermedad especifica
#' Function that generates the map by department with the data of a specific disease
#' @param data_map_depto Data for department
#' @param col_name_lj Column name to join with the shape file
#' @return The map for department with the data of a specific disease
#' @examples
#' geo_codes <- import_geo_codes()
#' deptos_data <- get_depto_codes(geo_codes)
#' sivigila_summary_data <- import_sivigila_summary_data()
#' filtered_data <- filter_disease("MALAR", sivigila_summary_data)
#' data_map_disease_deptos <- clean_depto_disease_codes(deptos_data, filtered_data)
#' plot_dept_map(data_map_disease_deptos, col_name_lj = "id")
#' @export
plot_dept_map <- function(data_map_depto, col_name_lj = "id") {
  maptools::gpclibPermit()
  
  shp <- rgdal::readOGR(dsn = system.file("extdata/depto_adm_shp", "depto.shp", package = "sivirep"), stringsAsFactors = FALSE)
  shp.df <- ggplot2::fortify(shp, region = "DPTO")
  shp.df <- shp.df %>%
    dplyr::left_join(data_map_depto, by = col_name_lj)

  map <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = shp.df, ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group, fill = .data$casos),
                 colour = "black") +
    ggplot2::scale_fill_gradient(low = "white", high = "darkred") +
    ggplot2::theme_void()

  return(map)
}

#' Plot by Variable
#'
#' Función que genera gráficas por cualquier tipo de variable
#' Function that generates graphs by any type of variable
#' @param data The data to plot
#' @param var_x The variable name for axis x
#' @param var_y The variable name for axis y
#' @param var_per The variable name if the data have percentages
#' @param var_fill The variable fill
#' @param wt_per Indicates if the data have percentages
#' @param label_x The label for axis x
#' @param label_y The label for axis y
#' @param scale_name The scale name
#' @param scale_labels The scale labels
#' @param diagram_title The diagram title
#' @param legend_pos The legend position
#' @param bar_wd The bar width
#' @param text_sz The text size
#' @param show_val Indicates if the bars should show the values
#' @return The map for department
#' @examples 
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' disease_dt_by_gender_wk <- group_by_columns_and_cases(disease_data, "SEXO", wt_percentage = TRUE)
#' plot_by_variable(disease_dt_by_gender_wk, var_x = "SEXO", var_y = "Casos", var_fill = "SEXO", 
#' var_per = "Porcentaje", label_x = "Gender", label_y = "Cases \n (Percentage)", 
#' scale_name = "Gender", scale_labels = c("Female", "Male"), diagram_title = "Cases by Gender", 
#' legend_pos = "right", bar_wd = 0.6, text_sz = 4)
#' @export
plot_by_variable <- function(data, var_x, var_y, var_per = NULL, var_fill, wt_per = TRUE, label_x, label_y,
                             scale_name = NULL, scale_labels = NULL, diagram_title, legend_pos, bar_wd = 1, text_sz, show_val = TRUE) {
  ggplot2::ggplot(data, ggplot2::aes_string(x = var_x, y = var_y, fill = var_fill) ) +
    ggplot2::geom_bar(width = bar_wd, stat = "identity", position = ggplot2::position_dodge()) +
    ggplot2::labs(x = label_x, y = label_y) +
    ggplot2::labs(fill = "") +
    ggplot2::theme_classic() +
    {if (show_val)
         ggplot2::geom_text(
                {if (!is.null(var_per)) eval(parse(text = paste0("ggplot2::aes(label = paste0(", var_y,", '\n (' ,", var_per, ", '%', ')'","))")))
                 else eval(parse(text = paste0("ggplot2::aes(label = ",var_y,")")))},
                vjust = 1.3,
                color = "black",
                hjust = 0.5,
                position = ggplot2::position_dodge(0.9),
                angle = 0,
                size = text_sz,
        )
    } +
    # ggplot2::scale_fill_discrete(name = scale_name, {if (!is.null(scale_labels)) labels = scale_labels}) +
    # theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = -0.3)) +
    # theme_linedraw() +
    ggplot2::theme(legend.position = legend_pos) +
    ggplot2::facet_grid(~as.character(diagram_title))
}