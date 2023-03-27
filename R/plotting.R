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
#' @export
plot_epiweek <- function(dat, col_week, col_cases, year, type = "week", xlabel = "Semana epidemiologica", ylabel = "Numero de casos por semana") {
  dat$epiweek <- dat[,col_week]
  dat$cases_count <- dat[,col_cases]
  dat_plot <- dat %>% dplyr::group_by(.data$epiweek, .data$nombre) %>% dplyr::summarise(casos = sum(.data$cases_count), .groups = "drop")
  
  if (type == "week") {
    plot <- ggplot2::ggplot(dat_plot) +
      ggplot2::geom_col(ggplot2::aes(x = .data$epiweek, y = .data$casos, fill = .data$nombre), alpha = 0.9) +
      ggplot2::theme_classic() +
      ggplot2::xlab(xlabel) + ggplot2::ylab(ylabel) +
      ggplot2::scale_fill_discrete(name = "") +
      ggplot2::theme(legend.position = "bottom")
  }
  
  if (type == "date") {
    dat_plot$date_week <- as.Date(paste(year, dat_plot$epiweek, 1, sep = "-"), "%Y-%U-%u")
    plot <- ggplot2::ggplot(dat_plot) +
      ggplot2::geom_col(ggplot2::aes(x = .data$date_week, y = .data$casos, fill = .data$nombre), alpha = 0.9) +
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
plot_dept_map <- function(data_map_depto, col_name_lj = "id", map_title, caption_label = "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia") {
  maptools::gpclibPermit()
  
  shp <- rgdal::readOGR(dsn = system.file("extdata/depto_adm_shp", "depto.shp", package = "sivirep"), stringsAsFactors = FALSE, verbose = FALSE)
  shp.df <- ggplot2::fortify(shp, region = "DPTO")
  shp.df <- shp.df %>%
    dplyr::left_join(data_map_depto, by = col_name_lj)
  
  map <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = shp.df, ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group, fill = .data$casos),
                          colour = "black") +
    ggplot2::scale_fill_gradient(low = "white", high = "darkred") +
    ggplot2::theme_void() + 
    ggplot2::ggtitle(map_title) + 
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), plot.caption = ggplot2::element_text(hjust = 0.5)) + 
    ggplot2::labs(caption = caption_label)
  
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
#' @export
plot_by_variable <- function(data, var_x, var_y, var_per = NULL, var_fill = NULL, wt_per = TRUE, label_x, label_y,
                             scale_name = NULL, scale_labels = NULL, diagram_title = NULL, legend_pos, bar_wd = 1, text_sz = 3, show_val = TRUE, ref_caption = "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia") {
  
  ggplot2::ggplot(data, {if (is.null(var_fill)) ggplot2::aes_string(x = var_x, y = var_y) else ggplot2::aes_string(x = var_x, y = var_y, fill = var_fill)}) +
    {if (is.null(var_fill)) ggplot2::geom_bar(width = bar_wd, stat = "identity", position = ggplot2::position_dodge(), fill = "#90C73D") 
      else  ggplot2::geom_bar(width = bar_wd, stat = "identity", position = ggplot2::position_dodge())} +
    ggplot2::labs(x = label_x, y = label_y, caption = ref_caption) +
    ggplot2::labs(fill = "") +
    ggplot2::theme_classic() + {if (text_sz > 3)
    ggplot2::theme(text = ggplot2::element_text(size = text_sz * 2))} +
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 6)) +
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
    # {if (is.null(diagram_title)) ggplot2::facet_grid(~as.character(diagram_title)) } +
    # ggplot2::ggtitle(diagram_title)
    ggplot2::theme(legend.position = legend_pos) +
    {if (ncol(data) == 3 || (!is.null(var_fill) && var_fill == "sexo")) ggplot2::scale_fill_manual(values = c("#56B4E9", "#E69F00")) else ggplot2::theme(legend.position = legend_pos) }
}

#' plot_onset_symptoms
#'
#' Función que genera la gráficas de distribucion de casos por fecha de inicio de sintomas
#' Function that generates the plot of cases distribution by onset symptoms date
#' @param disease_data Disease data
#' @param year Year
#' @param type Time unit
#' @param col_name Data set column name
#' @return A plot of cases distribution by onset symptoms date
#' @examples
#' disease_data <-  import_data_disease_by_year(2020, "DENGUE")
#' plot_onset_symptoms(disease_data, col_name = "ini_sin", type = "month")
#' @export
plot_onset_symptoms <- function(disease_data, col_name = "ini_sin", type = "month") {
  dates_column_names <- config::get(file = 
                                      system.file("extdata", "config.yml", 
                                                  package = "sivirep"), "dates_column_names")
  if (is.null(col_name)) {
    col_name <- dates_column_names[3]
  }
  
  plot_cases_by_onset_symp <- plot_by_variable(disease_data,
                                              var_x = col_name, 
                                              var_y = "casos", 
                                              label_x = "\nFecha de inicio de sintomas\n", 
                                              label_y = "Numero de casos\n", 
                                              legend_pos = "right",
                                              show_val = FALSE) + 
    ggplot2::scale_x_date(date_breaks = paste0("1 ", type), 
                          date_labels = "%b %d")
  return(plot_cases_by_onset_symp)
}

#' plot_notification_date
#'
#' Función que genera la gráficas de distribucion de casos por fecha de notificacion
#' Function that generates the plot of cases distribution by notification date
#' @param disease_data Disease data
#' @param year Year
#' @param type Time unit
#' @param col_name Data set column name
#' @return A plot of cases distribution by onset notification date
#' @examples
#' disease_data <-  import_data_disease_by_year(2020, "DENGUE")
#' plot_notification_date(disease_data, col_name = "fec_not", type = "month")
#' @export
plot_notification_date <- function(disease_data, col_name = "fec_not", type = "month") {
  dates_column_names <- config::get(file = 
                                      system.file("extdata", "config.yml", 
                                                  package = "sivirep"), "dates_column_names")
  if (is.null(col_name)) {
    col_name <- dates_column_names[2]
  }
  
  plot_cases_by_onset_symp <- plot_by_variable(disease_data,
                                               var_x = col_name, 
                                               var_y = "casos", 
                                               label_x = "\nFecha de notificacion\n", 
                                               label_y = "Numero de casos\n", 
                                               legend_pos = "right", 
                                               show_val = FALSE) + 
    ggplot2::scale_x_date(date_breaks = paste0("1 ", type), 
                          date_labels = "%b %d")
  return(plot_cases_by_onset_symp)
}

#' plot_gender
#'
#' Función que genera la gráficas de distribucion de casos por genero
#' Function that generates the plot of cases distribution by gender
#' @param disease_data Disease data
#' @param col_name Data set column name
#' @return A plot of cases distribution by gender
#' @examples
#' disease_data <-  import_data_disease_by_year(2020, "DENGUE")
#' plot_gender(disease_data, col_name = "sexo", percentage = T)
#' @export
plot_gender <- function(disease_data, col_name = "sexo", percentage = T) {
 
  plot_cases_by_gender <- plot_by_variable(disease_data, 
                                           var_x = col_name, 
                                           var_y = "casos", 
                                           var_fill =  col_name, 
                                           var_per = "porcentaje", 
                                           label_x = "\nSexo\n", 
                                           label_y = "Numero de casos\n", 
                                           scale_name = "Sexo", 
                                           scale_labels = c("Femenino", "Masculino"), 
                                           legend_pos = "right", 
                                           bar_wd = 0.5, 
                                           text_sz = 3, 
                                           show_val = percentage)
  return(plot_cases_by_gender)

}

#' plot_gender_and_week
#'
#' Función que genera la gráficas de distribucion de casos por genero y semana epidemiologica
#' Function that generates the plot of cases distribution by gender and epiweek
#' @param disease_data Disease data
#' @param col_names Data set column names
#' @return A plot of cases distribution by gender and epiweek
#' @examples
#' disease_data <-  import_data_disease_by_year(2020, "DENGUE")
#' plot_gender_and_week(disease_data, col_names = c("sexo", "semana"), percentage = F)
#' @export
plot_gender_and_week <- function(disease_data, col_names = c("sexo", "semana"), percentage = F) {
  
  plot_cases_by_gender_and_week <- plot_by_variable(disease_data, 
                                                    var_x = col_names[2], 
                                                    var_y = "casos", 
                                                    var_fill = col_names[1], 
                                                    var_per = "porcentaje", 
                                                    label_x = "\nSexo\n", 
                                                    label_y = "Numero de casos\n", 
                                                    scale_name = "Sexo", 
                                                    scale_labels = c("Femenino", "Masculino"), 
                                                    legend_pos = "right", 
                                                    bar_wd = 0.5, 
                                                    text_sz = 3, 
                                                    show_val = percentage) +
    ggplot2::scale_x_continuous(breaks = seq(1, 52, 4))
  
  return(plot_cases_by_gender_and_week)
}

#' plot_age_and_week
#'
#' Función que genera la gráficas de distribucion de casos por edad y semana epidemiologica
#' Function that generates the plot of cases distribution by age and epiweek
#' @param disease_data Disease data
#' @param col_names Data set column names
#' @return A plot of cases distribution by age and epiweek
#' @examples
#' disease_data <-  import_data_disease_by_year(2020, "DENGUE")
#' plot_gender_and_week(disease_data, col_names = c("edad", "semana"), percentage = T)
#' @export
plot_age_and_week <- function(disease_data, col_names = c("edad", "semana"), percentage = T) {
  plot_cases_by_age_and_week <- plot_by_variable(disease_data,
                                               var_x = col_names[1], 
                                               var_y = "casos",
                                               label_x = "\nEdad\n", 
                                               label_y = "Numero de casos\n", 
                                               scale_name = "Edad", 
                                               legend_pos = "right", 
                                               bar_wd = 0.7, 
                                               text_sz = 2.5)
  return(plot_cases_by_age_and_week)
}

#' plot_age_and_gender
#'
#' Función que genera la gráficas de distribucion de casos por edad y genero
#' Function that generates the plot of cases distribution by age and gender
#' @param disease_data Disease data
#' @param col_names Data set column names
#' @return A plot of cases distribution by age and gender
#' @examples
#' disease_data <-  import_data_disease_by_year(2020, "DENGUE")
#' plot_age_and_gender(disease_data, col_names = c("edad", "sexo"), percentage = T)
#' @export
plot_age_and_gender <- function(disease_data, col_names = c("edad", "sexo"), percentage = T) {
  plot_cases_by_age_and_gender <- plot_by_variable(disease_data, 
                                                 var_x = col_names[1], 
                                                 var_y = "casos", 
                                                 var_fill = col_names[2],
                                                 label_x = "\nEdad\n", 
                                                 label_y = "Numero de casos\n", 
                                                 scale_name = "Edad", 
                                                 legend_pos = "right", 
                                                 bar_wd = 0.7, 
                                                 text_sz = 3, 
                                                 show_val = FALSE)
  return(plot_cases_by_age_and_gender)
}

#' plot_special_population
#'
#' Función que genera la gráficas de distribucion de casos por poblacion especial
#' Function that generates the plot of cases distribution by special population
#' @param disease_data Disease data
#' @param col_name Data set column name
#' @return A plot of cases distribution by special population
#' @examples
#' disease_data <-  import_data_disease_by_year(2020, "DENGUE")
#' plot_special_population(disease_data, col_name = "poblacion", percentage = T)
#' @export
plot_special_population <- function(disease_data, col_name = "poblacion", percentage = T) {
  plot_cases_by_special_population <- plot_by_variable(disease_data, 
                                                       var_x = col_name, 
                                                       var_y = "casos",
                                                       var_fill =  col_name,
                                                       label_x = "Poblacion",
                                                       label_y = "casos",
                                                       scale_name = "Poblacion", 
                                                       legend_pos = "right", 
                                                       bar_wd = 0.5, 
                                                       text_sz = 3, 
                                                       show_val = TRUE) + 
                                      ggplot2::theme(legend.position = "bottom")
  return(plot_cases_by_special_population)
}