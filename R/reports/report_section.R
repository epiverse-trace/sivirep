#' get_cases_distribution_by_onset_sym_section
#'
#' Función que genera la seccion de distribucion de casos por fecha de inicio de sintomas
#' Function that generates the section of distribution of cases by onset symptoms date
#' @param disease_data Disease data
#' @param year Year
#' @param type Time unit
#' @param col_name Data set column name
#' @param col_cmp Data set column compare
#' @return A list with the cases by onset symptoms date and the section plot
#' @examples
#' disease_data <- import_data_disease_by_year(2020, "DENGUE")
#' get_cases_distribution_by_onset_sym_section(disease_data, year = 2020, type = "month", col_name = "ini_sin", col_cmp = "fec_hos")
#' @export
get_cases_distribution_by_onset_sym_section <- function(disease_data, year, type = "month", col_name = "ini_sin", col_cmp = "fec_hos", plot_title) {
  disease_data_by_onset_sym <- clean_disease_dates(disease_data, year = year, col_name = col_name, col_cmp = col_cmp)

  disease_data_by_onset_sym <- disease_data_by_onset_sym %>%
    dplyr::group_by(date = lubridate::floor_date(eval(parse(text = col_name)), type)) %>%
    dplyr::summarize(casos = sum(casos))

  colnames(disease_data_by_onset_sym)[colnames(disease_data_by_onset_sym) == "date"] <- col_name

  plot_cases_by_onset_sym <- plot_by_variable(disease_data_by_onset_sym,
    diagram_title = plot_title,
    var_x = col_name,
    var_y = "casos",
    label_x = "\nFecha de inicio de sintomas\n",
    label_y = "Numero de casos\n",
    legend_pos = "right",
    text_sz = 6,
    bar_wd = 9,
    show_val = FALSE
  ) +
    ggplot2::scale_x_date(
      date_breaks = paste0("1 ", type),
      date_labels = "%b %Y"
    )

  return(list(
    disease_cases = disease_data_by_onset_sym,
    plot = plot_cases_by_onset_sym
  ))
}

#' get_cases_distribution_by_notification_date_section
#'
#' Función que genera la seccion de distribucion de casos por fecha de notificacion
#' Function that generates the section of cases distribution by notification date
#' @param disease_data Disease data
#' @param year Year
#' @param type Time unit
#' @param col_name Data set column name
#' @return A list with the cases by notification date and the section plot
#' @examples
#' disease_data <- import_data_disease_by_year(2020, "DENGUE")
#' distribution_by_notification_date_section <- get_cases_distribution_by_notification_date_section(disease_data, year = 2020, type = "month", col_name = "fec_not")
#' @export
get_cases_distribution_by_notification_date_section <- function(disease_data, year, type = "month", col_name = "fec_not", plot_title) {
  disease_data_by_notification_date <- clean_disease_dates(disease_data, year = year, col_name = col_name)

  disease_data_by_notification_date <- disease_data_by_notification_date %>%
    dplyr::group_by(date = lubridate::floor_date(eval(parse(text = col_name)), type)) %>%
    dplyr::summarize(casos = sum(casos))

  colnames(disease_data_by_notification_date)[colnames(disease_data_by_notification_date) == "date"] <- col_name

  plot_cases_by_notification_date <- plot_by_variable(disease_data_by_notification_date,
    diagram_title = plot_title,
    var_x = col_name,
    var_y = "casos",
    label_x = "\nFecha de notificación\n",
    label_y = "Numero de casos\n",
    legend_pos = "right",
    text_sz = 6,
    bar_wd = 9,
    show_val = FALSE
  ) +
    ggplot2::scale_x_date(
      date_breaks = paste0("1 ", type),
      date_labels = "%b %Y"
    )

  return(list(
    disease_cases = disease_data_by_notification_date,
    plot = plot_cases_by_notification_date
  ))
}


get_temporal_cases_distribution_section <- function() {

}

#' get_cases_distribution_by_gender_section
#'
#' Función que genera la seccion de distribucion de casos por genero
#' Function that generates the section of cases distribution by gender
#' @param disease_data Disease data
#' @param year Year
#' @param col_name Data set column name
#' @param percentage Percentage
#' @return A list with the cases by gender, the female percentage, the male percentage, the gender major cases and the section plot
#' @examples
#' disease_data <- import_data_disease_by_year(2020, "DENGUE")
#' distribution_by_gender_section <- get_cases_distribution_by_gender_section(disease_data, year = 2020, col_name = "sexo", percentage = T)
#' @export
get_cases_distribution_by_gender_section <- function(disease_data, year, col_name = "sexo", percentage = T, plot_title) {
  disease_data_by_gender <- group_by_columns_and_cases(disease_data, col_name, percentage)

  male_percentage <- disease_data_by_gender$porcentaje[2]
  female_percentage <- disease_data_by_gender$porcentaje[1]

  gender_major_cases <- disease_data_by_gender[order(eval(parse(text = paste0("disease_data_by_gender$", "casos"))), decreasing = TRUE), ]
  gender_major_cases <- gender_major_cases[1, ]
  gender_major_cases$porcentaje <- round((gender_major_cases$casos[1] / nrow(disease_data)) * 100, 2)

  plot_cases_by_gender <- plot_by_variable(disease_data_by_gender,
    var_x = col_name,
    var_y = "casos",
    var_fill = col_name,
    var_per = "porcentaje",
    label_x = "\nSexo\n",
    label_y = "Numero de casos\n",
    scale_name = "Sexo",
    scale_labels = c("Femenino", "Masculino"),
    diagram_title = plot_title,
    legend_pos = "right",
    bar_wd = 0.5,
    text_sz = 3,
    show_val = percentage
  )

  return(list(
    disease_cases = disease_data_by_gender,
    plot = plot_cases_by_gender,
    male_percentage = male_percentage,
    female_percentage = female_percentage,
    gender_major_cases = gender_major_cases
  ))
}

#' get_cases_distribution_by_gender_and_week_section
#'
#' Función que genera la seccion de distribucion de casos por genero y semana epidemiologica
#' Function that generates the section of cases distribution by gender and week
#' @param disease_data Disease data
#' @param year Year
#' @param col_names Data set column names
#' @param percentage Percentage
#' @return A list with the cases by gender, the female percentage, the male percentage, the gender major cases and the section plot
#' @examples
#' disease_data <- import_data_disease_by_year(2020, "DENGUE")
#' distribution_by_gender_and_week_section <- get_cases_distribution_by_gender_and_week_section(disease_data, year = 2020, col_name = c("sexo", "semana"), percentage = F)
#' @export
get_cases_distribution_by_gender_and_week_section <- function(disease_data, year, col_names = c("sexo", "semana"), percentage = F, plot_title) {
  disease_data_by_gender_and_week <- group_by_columns_and_cases(disease_data, col_names, percentage)

  gender_major_cases <- disease_data_by_gender_and_week[order(eval(parse(text = paste0("disease_data_by_gender_and_week$", "casos"))), decreasing = TRUE), ]
  gender_major_cases <- gender_major_cases[1, ]
  gender_major_cases$porcentaje <- round((gender_major_cases$casos[1] / nrow(disease_data)) * 100, 2)

  plot_cases_by_gender_and_week <- plot_by_variable(disease_data_by_gender_and_week,
    var_x = col_names[2],
    var_y = "casos",
    var_fill = col_names[1],
    var_per = "porcentaje",
    label_x = "\nSexo\n",
    label_y = "Numero de casos\n",
    scale_name = "Sexo",
    scale_labels = c("Femenino", "Masculino"),
    diagram_title = plot_title,
    legend_pos = "right",
    bar_wd = 0.5,
    text_sz = 3,
    show_val = percentage
  ) +
    ggplot2::scale_x_continuous(breaks = seq(1, 52, 9))

  return(list(
    disease_cases = disease_data_by_gender_and_week,
    plot = plot_cases_by_gender_and_week,
    gender_major_cases = gender_major_cases
  ))
}

#' get_cases_distribution_by_age_and_week_section
#'
#' Función que genera la seccion de distribucion de casos por rango de edad
#' Function that generates the section of cases distribution by age range
#' @param disease_data Disease data
#' @param year Year
#' @param col_name Data set column name
#' @param percentage Percentage
#' @return A list with the cases by age range, the age range major cases, the percentage major cases and the section plot
#' @examples
#' disease_data <- import_data_disease_by_year(2020, "DENGUE")
#' distribution_by_age_and_week_section <- get_cases_distribution_by_age_and_week_section(disease_data, year = 2020, col_names = c("edad", "semana"), percentage = T)
#' @export
get_cases_distribution_by_age_and_week_section <- function(disease_data, year, col_names = c("edad", "semana"), percentage = T, plot_title) {
  disease_data_by_age_and_week <- clean_disease_ages(disease_data, col_names[1])
  disease_data_by_age_and_week <- group_by_columns_and_cases(disease_data_by_age_and_week, col_names, percentage)
  disease_data_by_age_and_week <- group_by_age_range_and_cases(disease_data_by_age_and_week, col_names[1], min_val = 0, max_val = max(eval(parse(text = paste0("disease_data_by_age_and_week$", col_names[1])))), step = 10)

  age_range_major_cases <- disease_data_by_age_and_week[order(eval(parse(text = paste0("disease_data_by_age_and_week$", "casos"))), decreasing = TRUE), ]
  age_range_major_cases <- age_range_major_cases[1, ]
  percentage_major_cases <- round((age_range_major_cases$casos[1] / nrow(disease_data)) * 100, 2)

  plot_cases_by_age_and_week <- plot_by_variable(disease_data_by_age_and_week,
    var_x = col_names[1],
    var_y = "casos",
    diagram_title = plot_title,
    label_x = "\nEdad\n",
    label_y = "Numero de casos\n",
    scale_name = "Edad",
    legend_pos = "right",
    bar_wd = 0.7,
    text_sz = 3
  )

  return(list(
    disease_cases = disease_data_by_age_and_week,
    plot = plot_cases_by_age_and_week,
    age_range_major_cases = age_range_major_cases,
    percentage_major_cases = percentage_major_cases
  ))
}

#' get_cases_distribution_by_age_and_gender_section
#'
#' Función que genera la seccion de distribucion de casos por rango de edad y genero
#' Function that generates the section of cases distribution by age range and gender
#' @param disease_data Disease data
#' @param year Year
#' @param col_names Data set column names
#' @param percentage Percentage
#' @return A list with the cases by age range, the age range major cases, the percentage major cases and the section plot
#' @examples
#' disease_data <- import_data_disease_by_year(2020, "DENGUE")
#' distribution_by_age_and_gender_section <- get_cases_distribution_by_age_and_gender_section(disease_data, year = 2020, col_names = c("edad", "sexo"), percentage = T)
#' @export
get_cases_distribution_by_age_and_gender_section <- function(disease_data, year, col_names = c("edad", "sexo"), percentage = T, plot_title) {
  disease_data_by_age_and_gender <- clean_disease_ages(disease_data, col_names[1])
  disease_data_by_age_and_gender <- group_by_columns_and_cases(disease_data_by_age_and_gender, col_names, percentage)
  disease_data_by_age_and_gender <- group_by_age_range_and_cases(disease_data_by_age_and_gender, col_names[1], col_names[2], min_val = 0, max_val = max(eval(parse(text = paste0("disease_data_by_age_and_gender$", col_names[1])))), step = 10)

  plot_cases_by_age_and_gender <- plot_by_variable(disease_data_by_age_and_gender,
    var_x = col_names[1],
    var_y = "casos",
    var_fill = col_names[2],
    diagram_title = plot_title,
    label_x = "\nEdad\n",
    label_y = "Numero de casos\n",
    scale_name = "Edad",
    legend_pos = "right",
    bar_wd = 0.7,
    text_sz = 3,
    show_val = FALSE
  )

  return(list(
    disease_cases = disease_data_by_age_and_gender,
    plot = plot_cases_by_age_and_gender
  ))
}

#' get_cases_distribution_by_special_population_section
#'
#' Función que genera la seccion de distribucion de casos por poblacion especial
#' Function that generates the section of cases distribution by special population
#' @param disease_data Disease data
#' @param year Year
#' @param col_name Data set column name
#' @param percentage Percentage
#' @return A list with the cases by special population and the section plot
#' @examples
#' disease_data <- import_data_disease_by_year(2020, "DENGUE")
#' distribution_by_special_population_section <- get_cases_distribution_by_special_population_section(disease_data, year = 2020, col_name = "Poblacion", percentage = F)
#' @export
get_cases_distribution_by_special_population_section <- function(disease_data, year, col_name = "Poblacion", percentage = F, plot_title) {
  disease_data_special <- get_special_population_and_cases(disease_data)
  disease_data_special_plot <- data.frame(Poblacion = disease_data_special$Poblacion, casos = disease_data_special$casos)
  plot_cases_by_special_population <- plot_by_variable(disease_data_special_plot,
    var_x = col_name,
    var_y = "casos",
    var_fill = col_name,
    label_x = "Poblacion",
    label_y = "casos",
    scale_name = "Poblacion",
    diagram_title = plot_title,
    legend_pos = "right",
    bar_wd = 0.5,
    text_sz = 3,
    show_val = TRUE
  ) +
    ggplot2::theme(legend.position = "bottom")
  return(list(
    disease_cases = disease_data_special,
    plot = plot_cases_by_special_population
  ))
}

#' get_cases_distribution_spatial_section
#'
#' Función que genera la seccion de distribucion espacial de casos
#' Function that generates the section of cases distribution spatial
#' @param disease_data Disease data
#' @param year Year
#' @param col_name Data set column name
#' @param percentage Percentage
#' @param plot_title Plot title
#' @return A list with the cases by deptos and the section map
#' @examples
#' disease_data <- import_data_disease_by_year(2020, "DENGUE")
#' distribution_by_special_population_section <- get_cases_distribution_spatial_section(disease_data, year = 2020, col_name = "Poblacion", percentage = F)
#' @export
get_cases_distribution_spatial_section <- function(disease_data, year, col_name = "cod_dpto_r", percentage = F, plot_title = "") {
  disease_data_by_depto_codes <- group_by_columns_and_cases(disease_data, col_names = col_name)
  colnames(disease_data_by_depto_codes)[colnames(disease_data_by_depto_codes) == col_name] <- "id"

  disease_data_by_depto_codes$id <- sapply(disease_data_by_depto_codes$id, as.character)
  map_by_deptos <- plot_dept_map(disease_data_by_depto_codes, map_title = plot_title)

  # disease_data_by_depto_codes <- get_depto_names(disease_data_by_depto_codes)
  return(list(
    disease_cases = disease_data_by_depto_codes,
    map = map_by_deptos
  ))
}
