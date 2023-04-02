#' Plot epidemiological weeks
#'
#' Function that generates the graphic of the epidemiological weeks
#' @param dat The disease data
#' @param col_week  Column name in the disease data that contains the epidemiological weeks
#' @param col_cases Column Name in the disease data that contains the cases number
#' @param year Year of the disease data
#' @param type Time unit for axis x (week and date)
#' @param xlabel Label of axis x
#' @param ylabel Label of axis y
#' @return The graphic of epidemiological weeks
#' @examples
#' sivigila_summary_data <- import_sivigila_summary_data()
#' filtered_data <- filter_disease("MALAR", sivigila_summary_data)
#' plot_epiweek(filtered_data, col_week = "semana", col_cases = "conteo_casos", year = 2019, type = "date", xlabel = "Fecha de semana epidemiologica")
#' @export
plot_epiweek <- function(dat, col_week, col_cases, year, type = "week", xlabel = "Semana epidemiologica", ylabel = "Numero de casos por semana") {
  dat$epiweek <- dat[, col_week]
  dat$cases_count <- dat[, col_cases]
  dat_plot <- dat %>%
    dplyr::group_by(.data$epiweek, .data$nombre) %>%
    dplyr::summarise(casos = sum(.data$cases_count), .groups = "drop")

  if (type == "week") {
    plot <- ggplot2::ggplot(dat_plot) +
      ggplot2::geom_col(ggplot2::aes(x = .data$epiweek, y = .data$casos, fill = .data$nombre), alpha = 0.9) +
      ggplot2::theme_classic() +
      ggplot2::xlab(xlabel) +
      ggplot2::ylab(ylabel) +
      ggplot2::scale_fill_discrete(name = "") +
      ggplot2::theme(legend.position = "bottom")
  }

  if (type == "date") {
    dat_plot$date_week <- as.Date(paste(year, dat_plot$epiweek, 1, sep = "-"), "%Y-%U-%u")
    plot <- ggplot2::ggplot(dat_plot) +
      ggplot2::geom_col(ggplot2::aes(x = .data$date_week, y = .data$casos, fill = .data$nombre), alpha = 0.9) +
      ggplot2::theme_classic() +
      ggplot2::xlab(xlabel) +
      ggplot2::ylab(ylabel) +
      ggplot2::scale_fill_discrete(name = "") +
      ggplot2::theme(legend.position = "bottom")
  }

  return(plot)
}

#' Plot map by department
#'
#' Function that generates the map by department with the cases number of a specific disease
#' @param data_grouped The disease data grouped by department and cases number
#' @param col_name_lj Column name to join with the shape file
#' @return The map by department with the cases number of a specific disease
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' departments_spacial_data <- group_dept(disease_data)
#' plot_dept_map(departments_spacial_data, col_name_lj = "id", caption_label = "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia")
#' @export
plot_dept_map <- function(data_grouped, col_name_lj = "id", map_title, caption_label = "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia") {
  maptools::gpclibPermit()

  shp <- rgdal::readOGR(dsn = system.file("extdata/depto_adm_shp", "depto.shp", package = "sivirep"), stringsAsFactors = FALSE, verbose = FALSE)
  shp.df <- ggplot2::fortify(shp, region = "DPTO")
  shp.df <- shp.df %>%
    dplyr::left_join(data_grouped, by = col_name_lj)

  map <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = shp.df, ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group, fill = .data$casos),
      colour = "black"
    ) +
    ggplot2::scale_fill_gradient(low = "white", high = "darkred") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::labs(caption = caption_label)

  return(map)
}

#' Plot by variable(s) or column(s)
#'
#' Function that generates a graph by any type of variable or column of dataframe
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
#' @return The plot by variable(s) or column(s)
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' plot_variable(disease_data,
#'   var_x = col_name,
#'   var_y = "casos",
#'   var_fill = col_name,
#'   var_per = "porcentaje",
#'   label_x = "Genero",
#'   label_y = "Numero de casos",
#'   scale_name = "Sexo",
#'   scale_labels = c("Femenino", "Masculino"),
#'   legend_pos = "right",
#'   bar_wd = 0.5,
#'   text_sz = 3,
#'   show_val = percentage
#'   )
#' @export
plot_variable <- function(data, var_x, var_y, var_per = NULL, var_fill = NULL, wt_per = TRUE, label_x, label_y,
                             scale_name = NULL, scale_labels = NULL, diagram_title = NULL, legend_pos, bar_wd = 1, text_sz = 3, show_val = TRUE, ref_caption = "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia") {
  ggplot2::ggplot(data, {
    if (is.null(var_fill)) ggplot2::aes_string(x = var_x, y = var_y) else ggplot2::aes_string(x = var_x, y = var_y, fill = var_fill)
  }) +
    {
      if (is.null(var_fill)) {
        ggplot2::geom_bar(width = bar_wd, stat = "identity", position = ggplot2::position_dodge(), fill = "#90C73D")
      } else {
        ggplot2::geom_bar(width = bar_wd, stat = "identity", position = ggplot2::position_dodge())
      }
    } +
    ggplot2::labs(x = label_x, y = label_y, caption = ref_caption) +
    ggplot2::labs(fill = "") +
    ggplot2::theme_classic() +
    {
      if (text_sz > 3) {
        ggplot2::theme(text = ggplot2::element_text(size = text_sz * 2))
      }
    } +
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 8)) +
    {
      if (show_val) {
        ggplot2::geom_text(
          {
            if (!is.null(var_per)) {
              eval(parse(text = paste0("ggplot2::aes(label = paste0(", var_y, ", '\n (' ,", var_per, ", '%', ')'", "))")))
            } else {
              eval(parse(text = paste0("ggplot2::aes(label = ", var_y, ")")))
            }
          },
          vjust = 1.3,
          color = "black",
          hjust = 0.5,
          position = ggplot2::position_dodge(0.9),
          angle = 0,
          size = text_sz,
        )
      }
    } +
    # ggplot2::scale_fill_discrete(name = scale_name, {if (!is.null(scale_labels)) labels = scale_labels}) +
    # theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust = -0.3)) +
    # theme_linedraw() +
    # {if (is.null(diagram_title)) ggplot2::facet_grid(~as.character(diagram_title)) } +
    # ggplot2::ggtitle(diagram_title)
    ggplot2::theme(legend.position = legend_pos) +
    {
      if (ncol(data) == 3 || (!is.null(var_fill) && var_fill == "sexo")) ggplot2::scale_fill_manual(values = c("#56B4E9", "#E69F00")) else ggplot2::theme(legend.position = legend_pos)
    }
}

#' Plot cases distribution by symptoms onset date
#'
#' Function that generates the plot of cases distribution by onset symptoms date
#' @param data_grouped The disease data grouped
#' @param year Year of the disease data grouped
#' @param type Time unit (day, month and year)
#' @param col_name Column name in the disease data grouped that contains the symptom onset dates
#' @return A plot of cases distribution by symptoms onset date
#' @examples
#' disease_data <- import_data_disease_by_year(2020, "DENGUE")
#' data_grouped <- group_by_onset_symptoms(disease_data, col_name = "ini_sin", break_tick_date = "month")
#' plot_onset_symptoms(data_grouped, col_name = "ini_sin", break_tick_date = "month")
#' @export
plot_onset_symptoms <- function(data_grouped, col_name = "ini_sin", break_tick_date = "month") {
  dates_column_names <- config::get(
    file =
      system.file("extdata", "config.yml",
        package = "sivirep"
      ), "dates_column_names"
  )
  if (is.null(col_name)) {
    col_name <- dates_column_names[3]
  }

  plot_cases_by_onset_symp <- plot_variable(data_grouped,
    var_x = col_name,
    var_y = "casos",
    label_x = "\nFecha de inicio de sintomas\n por dia",
    label_y = "Numero de casos\n",
    legend_pos = "right",
    show_val = FALSE
  ) +
    ggplot2::scale_x_date(
      date_breaks = paste0("1 ", break_tick_date),
      date_labels = "%b"
    )
  return(plot_cases_by_onset_symp)
}

#' Plot cases distribution by notification date
#'
#' Function that generates the plot of cases distribution by notification date
#' @param data_grouped The disease data grouped
#' @param year Year of the disease data grouped
#' @param type Time unit (day, month and year)
#' @param col_name Column name in the disease data grouped that contains the notification dates
#' @return A plot of cases distribution by onset notification date
#' @examples
#' disease_data <- import_data_disease_by_year(2020, "DENGUE")
#' data_grouped <- group_notification_date(disease_data, col_name = "fec_not", break_tick_date = "month")
#' plot_notification_date(data_grouped, col_name = "fec_not", break_tick_date = "month")
#' @export
plot_notification_date <- function(data_grouped, col_name = "fec_not", break_tick_date = "month") {
  dates_column_names <- config::get(
    file =
      system.file("extdata", "config.yml",
        package = "sivirep"
      ), "dates_column_names"
  )
  if (is.null(col_name)) {
    col_name <- dates_column_names[2]
  }

  plot_cases_by_onset_symp <- plot_variable(data_grouped,
    var_x = col_name,
    var_y = "casos",
    label_x = "\nFecha de notificacion\n",
    label_y = "Numero de casos\n  por dia",
    legend_pos = "right",
    show_val = FALSE
  ) +
    ggplot2::scale_x_date(
      date_breaks = paste0("1 ", break_tick_date),
      date_labels = "%b"
    )
  return(plot_cases_by_onset_symp)
}

#' Plot cases distribution by sex
#'
#' Function that generates the plot of cases distribution by sex
#' @param data_grouped The disease data grouped
#' @param col_name Column name in the disease data grouped that contains the sex
#' @return A plot of cases distribution by sex
#' @examples
#' disease_data <- import_data_disease_by_year(2020, "DENGUE")
#' data_grouped <- group_sex(disease_data, col_name = "sexo", percentage = TRUE)
#' plot_sex(data_grouped, col_name = "sexo", percentage = TRUE)
#' @export
plot_sex <- function(data_grouped, col_name = "sexo", percentage = TRUE) {
  plot_cases_by_sex <- plot_variable(data_grouped,
    var_x = col_name,
    var_y = "casos",
    var_fill = col_name,
    var_per = "porcentaje",
    label_x = "\nSexo\n",
    label_y = "Numero de casos\n",
    scale_name = "Sexo",
    scale_labels = c("Femenino", "Masculino"),
    legend_pos = "right",
    bar_wd = 0.5,
    text_sz = 3,
    show_val = percentage
  )
  return(plot_cases_by_sex)
}

#' Plot cases distribution by sex and epidemiological week
#'
#' Function that generates the plot of cases distribution by sex and epidemiological week
#' @param data_grouped The disease data grouped
#' @param col_names Column names in the disease data grouped that contains the sex and the epidemiological weeks
#' @return A plot of cases distribution by sex and epidemiological week
#' @examples
#' disease_data <- import_data_disease_by_year(2020, "DENGUE")
#' data_grouped <- group_sex_and_week(disease_data, col_names = c("sexo", "semana"), percentage = TRUE)
#' plot_sex_epiweek(data_grouped, col_names = c("sexo", "semana"), percentage = FALSE)
#' @export
plot_sex_epiweek <- function(data_grouped, col_names = c("sexo", "semana"), percentage = FALSE) {
  plot_cases_by_sex_and_week <- plot_variable(data_grouped,
    var_x = col_names[2],
    var_y = "casos",
    var_fill = col_names[1],
    var_per = "porcentaje",
    label_x = "\nSemana epidemiologica\n",
    label_y = "Numero de casos\n",
    scale_name = "Sexo",
    scale_labels = c("Femenino", "Masculino"),
    legend_pos = "right",
    bar_wd = 0.5,
    text_sz = 3,
    show_val = percentage
  ) +
    ggplot2::scale_x_continuous(breaks = seq(1, 52, 4))

  return(plot_cases_by_sex_and_week)
}

#' Plot cases distribution by age
#'
#' Function that generates the plot of cases distribution by age
#' @param data_grouped The disease data grouped
#' @param col_name Column name in the disease data grouped that contains the ages
#' @return A plot of cases distribution by age
#' @examples
#' disease_data <- import_data_disease_by_year(2020, "DENGUE")
#' data_grouped <- group_age(disease_data, col_name = "edad", percentage = FALSE)
#' plot_age(data_grouped, col_name = "edad", percentage = FALSE)
#' @export
plot_age <- function(data_grouped, col_names = "edad", percentage = FALSE) {
  plot_cases_by_age <- plot_variable(data_grouped,
    var_x = col_names[1],
    var_y = "casos",
    label_x = "\nEdad\n",
    label_y = "Numero de casos\n",
    scale_name = "Edad",
    legend_pos = "right",
    bar_wd = 0.7,
    text_sz = 2.5
  )
  
  return(plot_cases_by_age)

}

#' Plot cases distribution by age and sex
#'
#' Function that generates the plot of cases distribution by age and sex
#' @param data_grouped The disease data grouped
#' @param col_names Column names in the disease data grouped that contains the ages and the epidemiological weeks
#' @return A plot of cases distribution by age and sex
#' @examples
#' disease_data <- import_data_disease_by_year(2020, "DENGUE")
#' data_grouped <- group_by_age_and_week(disease_data, col_names = c("edad", "semana"), percentage = FALSE)
#' plot_age_sex(data_grouped, col_names = c("edad", "sexo"), percentage = FALSE)
#' @export
plot_age_sex <- function(data_grouped, col_names = c("edad", "sexo"), percentage = FALSE) {
  plot_cases_by_age_and_sex <- plot_variable(data_grouped,
    var_x = col_names[1],
    var_y = "casos",
    var_fill = col_names[2],
    label_x = "\nEdad\n",
    label_y = "Numero de casos\n",
    scale_name = "Edad",
    legend_pos = "right",
    bar_wd = 0.7,
    text_sz = 3,
    show_val = percentage
  )
  return(plot_cases_by_age_and_sex)
}

#' Plot cases distribution by special population
#'
#' Function that generates the plot of cases distribution by special population
#' @param data_grouped The disease data grouped
#' @param col_name Column names in the disease data grouped that contains the ages and sex
#' @return A plot of cases distribution by special population
#' @examples
#' disease_data <- import_data_disease_by_year(2020, "DENGUE")
#' data_grouped <- group_by_age_and_sex(disease_data, col_names = c("edad", "sexo"), percentage = TRUE)
#' plot_special_population(data_grouped, col_name = "poblacion", percentage = FALSE)
#' @export
plot_special_population <- function(data_grouped, col_name = "poblacion", percentage = FALSE) {
  plot_cases_by_special_population <- plot_variable(data_grouped,
    var_x = col_name,
    var_y = "casos",
    var_fill = col_name,
    label_x = "Poblacion",
    label_y = "casos",
    scale_name = "Poblacion",
    legend_pos = "right",
    bar_wd = 0.5,
    text_sz = 3,
    show_val = percentage
  ) +
    ggplot2::theme(legend.position = "bottom")
  return(plot_cases_by_special_population)
}