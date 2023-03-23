#' @export
get_cases_distribution_by_onset_sym_section <- function(disease_data, year, type = "month", col_name = "INI_SIN", col_cmp = "FEC_HOS") {
  disease_data_by_onset_sym <- clean_disease_dates(disease_data, year = year, col_name = col_name, col_cmp = col_cmp)
  
  disease_data_by_onset_sym <- disease_data_by_onset_sym %>% 
    dplyr::group_by(date = lubridate::floor_date(eval(parse(text = col_name)), type)) %>%
    dplyr::summarize(Casos = sum(Casos))
  
  colnames(disease_data_by_onset_sym)[colnames(disease_data_by_onset_sym) == "date" ] <- col_name
  
  plot_cases_by_onset_sym <- plot_by_variable(disease_data_by_onset_sym,
                   diagram_title = NULL,
                   var_x = col_name, 
                   var_y = "Casos", 
                   label_x = "\nFecha de inicio de sintomas\n", 
                   label_y = "Numero de casos\n", 
                   legend_pos = "right", 
                   text_sz = 6,
                   bar_wd = 9,
                   show_val = FALSE) + 
    ggplot2::scale_x_date(date_breaks = paste0("1 ", type), 
                 date_labels = "%b %Y")
  
  return(list(disease_cases = disease_data_by_onset_sym, 
              plot = plot_cases_by_onset_sym))
}

#' @export
get_cases_distribution_by_notification_date_section <- function(disease_data, year, type = "month", col_name = "FEC_NOT") {
  
  disease_data_by_notification_date <- clean_disease_dates(disease_data, year = year, col_name = col_name)
  
  disease_data_by_notification_date <- disease_data_by_notification_date %>% 
    dplyr::group_by(date = lubridate::floor_date(eval(parse(text = col_name)), type)) %>%
    dplyr::summarize(Casos = sum(Casos))
  
  colnames(disease_data_by_notification_date)[colnames(disease_data_by_notification_date) == "date" ] <- col_name
  
  plot_cases_by_notification_date <- plot_by_variable(disease_data_by_notification_date,
                           diagram_title = NULL,
                           var_x = col_name, 
                           var_y = "Casos", 
                           label_x = "\nFecha de notificación\n", 
                           label_y = "Numero de casos\n", 
                           legend_pos = "right", 
                           text_sz = 6,
                           bar_wd = 9,
                           show_val = FALSE) + 
    ggplot2::scale_x_date(date_breaks = paste0("1 ", type), 
                          date_labels = "%b %Y")
  
   return(list(disease_cases = disease_data_by_notification_date, 
               plot = plot_cases_by_notification_date))
}


get_temporal_cases_distribution_section <- function() {
  
}

#' @export
get_cases_distribution_by_gender_section <- function(disease_data, year, col_name = "SEXO", percentage = T) {
  
  disease_data_by_gender <- group_by_columns_and_cases(disease_data, col_name, percentage)
  
  male_percentage <- disease_data_by_gender$Porcentaje[2]
  female_percentage <- disease_data_by_gender$Porcentaje[1]
  
  gender_major_cases <- disease_data_by_gender[order(eval(parse(text = paste0("disease_data_by_gender$", "Casos"))), decreasing = TRUE), ]
  gender_major_cases <- gender_major_cases[1, ]
  gender_major_cases$Porcentaje <-  round((gender_major_cases$Casos[1]/nrow(disease_data)) * 100, 2)
  
  plot_cases_by_gender <- plot_by_variable(disease_data_by_gender, 
                   var_x = "SEXO", 
                   var_y = "Casos", 
                   var_fill = "SEXO", 
                   var_per = "Porcentaje", 
                   label_x = "\nSexo\n", 
                   label_y = "Numero de casos\n", 
                   scale_name = "Sexo", 
                   scale_labels = c("Femenino", "Masculino"), 
                   diagram_title = "", 
                   legend_pos = "right", 
                   bar_wd = 0.5, 
                   text_sz = 3, 
                   show_val = percentage)
  
  return(list(disease_cases = disease_data_by_gender, 
              plot =  plot_cases_by_gender, 
              male_percentage = male_percentage, 
              female_percentage = female_percentage,
              gender_major_cases = gender_major_cases))
}

#' @export
get_cases_distribution_by_age_and_week_section <- function(disease_data, year, col_names = c("EDAD", "SEMANA"), percentage = T) {
  disease_data_by_age_and_week <- clean_disease_ages(disease_data, col_names[1])
  disease_data_by_age_and_week <- group_by_columns_and_cases(disease_data_by_age_and_week, col_names, percentage)
  disease_data_by_age_and_week <- group_by_age_range_and_cases(disease_data_by_age_and_week, col_names[1], min_val = 0, max_val = max(eval(parse(text = paste0("disease_dt_by_age$", col_names[1])))), step = 10)
  
  age_range_major_cases <- disease_data_by_age_and_week[order(eval(parse(text = paste0("disease_data_by_age_and_week$", "Casos"))), decreasing = TRUE), ]
  age_range_major_cases <- age_range_major_cases[1, ]
  percentage_major_cases <-  round((age_range_major_cases$Casos[1]/nrow(disease_data)) * 100, 2)
  
  plot_cases_by_age_and_week <- plot_by_variable(disease_data_by_age_and_week,
                                                   var_x = "EDAD", 
                                                   var_y = "Casos",
                                                   diagram_title = "", 
                                                   label_x = "\nEdad\n", 
                                                   label_y = "Numero de casos\n", 
                                                   scale_name = "Edad", 
                                                   legend_pos = "right", 
                                                   bar_wd = 0.7, 
                                                   text_sz = 3)
  
  return(list(disease_cases = disease_data_by_age_and_week, 
              plot = plot_cases_by_age_and_week, 
              age_range_major_cases = age_range_major_cases, 
              percentage_major_cases = percentage_major_cases))
}

#' @export
get_cases_distribution_by_age_and_gender_section <- function(disease_data, year, col_names = c("EDAD", "SEXO"), percentage = T) {
  disease_data_by_age_and_gender <- clean_disease_ages(disease_data, col_names[1])
  disease_data_by_age_and_gender <- group_by_columns_and_cases(disease_data_by_age_and_gender, col_names, percentage)
  disease_data_by_age_and_gender <- group_by_age_range_and_cases(disease_data_by_age_and_gender, col_names[1], col_names[2], min_val = 0, max_val = max(eval(parse(text = paste0("disease_dt_by_age$", col_names[1])))), step = 10)
  
  plot_cases_by_age_and_gender <- plot_by_variable(disease_data_by_age_and_gender, 
                   var_x = "EDAD", 
                   var_y = "Casos", 
                   var_fill = "SEXO",
                   diagram_title = "",
                   label_x = "\nEdad\n", 
                   label_y = "Numero de casos\n", 
                   scale_name = "Edad", 
                   legend_pos = "right", 
                   bar_wd = 0.7, 
                   text_sz = 3, 
                   show_val = FALSE)
  
  return(list(disease_cases = disease_data_by_age_and_gender, 
              plot = plot_cases_by_age_and_gender))
}