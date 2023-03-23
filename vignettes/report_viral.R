generate_distribution_by_age <- function(diseases_data) {
  df1 <- data.frame(enfermedad  = c("H3N2", "H3N2", "H3N2","H3N2", "H3N2", "H3N2", "Influenza B", "Influenza B", "Influenza B", "Influenza B","Influenza B", "Influenza B", "Otros Virus", "Otros Virus", "Otros Virus","Otros Virus", "Otros Virus",  "Otros Virus", "Bocavirus", "Bocavirus", "Bocavirus","Bocavirus", "Bocavirus", "Bocavirus", "Metapneumovirus", "Metapneumovirus","Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Rinovirus", "Rinovirus", "Rinovirus","Rinovirus", "Rinovirus", "Rinovirus", "Parainfluenza", "Parainfluenza", "Parainfluenza","Parainfluenza", "Parainfluenza", "Parainfluenza", "VSR", "VSR", "VSR", "VSR", "VSR", "VSR", "Adenovirus", "Adenovirus","Adenovirus","Adenovirus","Adenovirus","Adenovirus"),
                    grupo_edad = c("<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más"),
                    casos = c(51, 77, 45, 60, 30, 49, 0, 1, 0, 0, 0, 0, 25, 11, 5, 5, 4, 3, 84, 117, 13, 10, 7, 7, 51, 43, 16, 3, 4, 11, 261, 233, 95, 48, 23, 40, 121, 97, 44, 16, 16, 23, 314, 239, 23, 8, 8, 14, 224, 247, 64, 18, 11, 18)
  )
  
  plot <- ggplot2::ggplot(df1) +
    ggplot2::geom_col(ggplot2::aes(x = grupo_edad, y = casos, fill = enfermedad), alpha = 0.9) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Edad") + ggplot2::ylab("Casos") +
    ggplot2::scale_fill_discrete(name = "Enfermedad") +
    ggplot2::theme(legend.position = "bottom")
  
  return(plot)
}


generate_distribution_by_age_and_sars <- function(diseases_data) {
  df2 <- data.frame(enfermedad  = c("H3N2", "H3N2", "H3N2","H3N2", "H3N2", "H3N2", "Influenza B", "Influenza B", "Influenza B", "Influenza B","Influenza B", "Influenza B", "Otros Virus", "Otros Virus", "Otros Virus","Otros Virus", "Otros Virus",  "Otros Virus", "Bocavirus", "Bocavirus", "Bocavirus","Bocavirus", "Bocavirus", "Bocavirus", "Metapneumovirus", "Metapneumovirus","Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Rinovirus", "Rinovirus", "Rinovirus","Rinovirus", "Rinovirus", "Rinovirus", "Parainfluenza", "Parainfluenza", "Parainfluenza","Parainfluenza", "Parainfluenza", "Parainfluenza", "VSR", "VSR", "VSR", "VSR", "VSR", "VSR", "Adenovirus", "Adenovirus","Adenovirus","Adenovirus","Adenovirus","Adenovirus"),
                    grupo_edad = c("<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más"),
                    casos = c(51, 77, 45, 60, 30, 49, 0, 1, 0, 0, 0, 0, 25, 11, 5, 5, 4, 3, 84, 117, 13, 10, 7, 7, 51, 43, 16, 3, 4, 11, 261, 233, 95, 48, 23, 40, 121, 97, 44, 16, 16, 23, 314, 239, 23, 8, 8, 14, 224, 247, 64, 18, 11, 18)
  )
  
  plot <- ggplot2::ggplot(df2) +
    ggplot2::geom_col(ggplot2::aes(x = grupo_edad, y = casos, fill = enfermedad), alpha = 0.9) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Edad") + ggplot2::ylab("Casos") +
    ggplot2::scale_fill_discrete(name = "Enfermedad") +
    ggplot2::theme(legend.position = "bottom")
  
  return(plot)
}

generate_distribution_by_age_and_esi <- function(diseases_data) {
  df3 <- data.frame(enfermedad  = c("SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "H3N2", "H3N2", "H3N2","H3N2", "H3N2", "H3N2", "Influenza B", "Influenza B", "Influenza B", "Influenza B","Influenza B", "Influenza B", "Otros Virus", "Otros Virus", "Otros Virus","Otros Virus", "Otros Virus",  "Otros Virus", "Bocavirus", "Bocavirus", "Bocavirus","Bocavirus", "Bocavirus", "Bocavirus", "Metapneumovirus", "Metapneumovirus","Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Rinovirus", "Rinovirus", "Rinovirus","Rinovirus", "Rinovirus", "Rinovirus", "Parainfluenza", "Parainfluenza", "Parainfluenza","Parainfluenza", "Parainfluenza", "Parainfluenza", "VSR", "VSR", "VSR", "VSR", "VSR", "VSR", "Adenovirus", "Adenovirus","Adenovirus","Adenovirus","Adenovirus","Adenovirus"),
                    grupo_edad = c("<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más"),
                    casos = c(45, 25, 3, 7, 5, 5, 3, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 2, 0, 2, 6, 14, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 3, 14, 0, 2, 0, 0, 2, 3, 0, 0, 0, 0, 7, 14, 0, 0, 0, 0, 5, 16, 1, 0, 0, 1)
  )
  
  plot <- ggplot2::ggplot(df3) +
    ggplot2::geom_col(ggplot2::aes(x = grupo_edad, y = casos, fill = enfermedad), alpha = 0.9) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Edad") + ggplot2::ylab("Casos") +
    ggplot2::scale_fill_discrete(name = "Enfermedad") +
    ggplot2::theme(legend.position = "bottom")
  
  return(plot)
}


generate_distribution_by_age_and_ira <- function(diseases_data) {
  df4 <- data.frame(enfermedad  = c("SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "H3N2", "H3N2", "H3N2","H3N2", "H3N2", "H3N2", "Influenza B", "Influenza B", "Influenza B", "Influenza B","Influenza B", "Influenza B", "Otros Virus", "Otros Virus", "Otros Virus","Otros Virus", "Otros Virus",  "Otros Virus", "Bocavirus", "Bocavirus", "Bocavirus","Bocavirus", "Bocavirus", "Bocavirus", "Metapneumovirus", "Metapneumovirus","Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Rinovirus", "Rinovirus", "Rinovirus","Rinovirus", "Rinovirus", "Rinovirus", "Parainfluenza", "Parainfluenza", "Parainfluenza","Parainfluenza", "Parainfluenza", "Parainfluenza", "VSR", "VSR", "VSR", "VSR", "VSR", "VSR", "Adenovirus", "Adenovirus","Adenovirus","Adenovirus","Adenovirus","Adenovirus"),
                    grupo_edad = c("<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más"),
                    casos = c(106, 30, 13, 30, 19, 51, 11, 28, 13, 18, 8, 4, 0, 0, 0, 0, 0, 0, 14, 5, 2, 5, 0, 1, 21, 33, 5, 8, 3, 5, 3, 6, 1, 0, 0, 1, 32, 32, 7, 7, 3, 3, 13, 7, 2, 0, 2, 1, 34, 35, 1, 4, 1, 1, 19, 21, 2, 3, 0, 1)
  )
  
  plot <- ggplot2::ggplot(df4) +
    ggplot2::geom_col(ggplot2::aes(x = grupo_edad, y = casos, fill = enfermedad), alpha = 0.9) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Edad") + ggplot2::ylab("Casos") +
    ggplot2::scale_fill_discrete(name = "Enfermedad") +
    ggplot2::theme(legend.position = "bottom")
  
  return(plot)
}

generate_distribution_by_age_and_irag_inusitado <- function(diseases_data) {
  df5 <- data.frame(enfermedad  = c("SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "SARS CoV 2", "H3N2", "H3N2", "H3N2","H3N2", "H3N2", "H3N2", "Influenza B", "Influenza B", "Influenza B", "Influenza B","Influenza B", "Influenza B", "Otros Virus", "Otros Virus", "Otros Virus","Otros Virus", "Otros Virus",  "Otros Virus", "Bocavirus", "Bocavirus", "Bocavirus","Bocavirus", "Bocavirus", "Bocavirus", "Metapneumovirus", "Metapneumovirus","Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Metapneumovirus", "Rinovirus", "Rinovirus", "Rinovirus","Rinovirus", "Rinovirus", "Rinovirus", "Parainfluenza", "Parainfluenza", "Parainfluenza","Parainfluenza", "Parainfluenza", "Parainfluenza", "VSR", "VSR", "VSR", "VSR", "VSR", "VSR", "Adenovirus", "Adenovirus","Adenovirus","Adenovirus","Adenovirus","Adenovirus"),
                    grupo_edad = c("<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más", "<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más","<2 años", "2 a 4 años", "5 a 14 años", "15 a 39 años", "40 a 59 años", "60 y más"),
                    casos = c(106, 30, 13, 30, 19, 51, 11, 28, 13, 18, 8, 4, 0, 0, 0, 0, 0, 0, 14, 5, 2, 5, 0, 1, 21, 33, 5, 8, 3, 5, 3, 6, 1, 0, 0, 1, 32, 32, 7, 7, 3, 3, 13, 7, 2, 0, 2, 1, 34, 35, 1, 4, 1, 1, 19, 21, 2, 3, 0, 1)
  )
  
  plot <- ggplot2::ggplot(df5) +
    ggplot2::geom_col(ggplot2::aes(x = grupo_edad, y = casos, fill = enfermedad), alpha = 0.9) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Edad") + ggplot2::ylab("Casos") +
    ggplot2::scale_fill_discrete(name = "Enfermedad") +
    ggplot2::theme(legend.position = "bottom")
  
  return(plot)
}






























import_data_viral_circulation <- function(report_data = c()) {
  
  viral_circulation_data <- data.frame()
  
  for (data_path in report_data) {
    file_extension <- tools::file_ext(data_path)
    if (!is.null(file_extension)) {
      temp_data <- switch(  
        file_extension,  
        "xlsx" = readxl::read_excel(paste0("C:/Users/geral/Documents/TRACE/sivirep/data/", data_path), col_names = F, skip = 3),  
        "csv" = utils::read.csv(data_path, header = F, skip = 3)
      )
      temp_data <- row_to_header(data = temp_data)
      viral_circulation_data <- rbind(viral_circulation_data, temp_data)
    }
  }
  return(viral_circulation_data)
}

row_to_header <- function(data, row_num = 1) {
  if (!is.null(data)) { 
    names(data) <- as.character(unlist(data[row_num,]))
    data[-row_num,]
  }
}

clean_viral_circulation_data <- function(viral_circulation_data) {
  names(viral_circulation_data) <- epitrix::clean_labels(names(viral_circulation_data))
  return(viral_circulation_data)
}

generate_age_categories <- function(data) {
  data <- cbind(data, grupo_edad = NA)
  data[, ncol(data)] <- sapply(data$edad, define_age_categorie)
  return(data)
}

define_age_categorie <- function(age) {
  #config_path <- system.file("extdata", "config.yml", package = "sivirep")
  config_path <- "C:/Users/geral/Documents/TRACE/sivirep/inst/extdata/config.yml"
  categorie_conditionals <- config::get(file = config_path, "age_categorie_conditionals")
  categorie_labels <- config::get(file = config_path, "age_categorie_labels")
  
  age_values <- unlist(strsplit(age, " ", fixed = T))
  categorie <- categorie_labels[1]
  
  if ("AÑOS" %in% age_values) {
    age_num <- as.numeric(age_values[1])
    i <- 1;
    for (conditional in categorie_conditionals) {
      if (eval(parse(text = conditional)) == T) {
        categorie <- categorie_labels[i]
      } 
      i <- i + 1
    }
  }
  
  return(categorie)
}

group_by_columns_and_cases_total <- function(disease_data, event_name = "adenovirus", col_names, wt_percentage = FALSE, total_cases = 0, event_label) {
  config_path <- "C:/Users/geral/Documents/TRACE/sivirep/inst/extdata/config.yml"
  categorie_labels <- config::get(file = config_path, "age_categorie_labels")
  
  disease_data_grouped  <- disease_data %>% dplyr::group_by(dplyr::across(dplyr::all_of(col_names))) %>% dplyr::summarise(casos = dplyr::n(), .groups = "drop")
  if (wt_percentage) {
    disease_data_grouped  <-  disease_data_grouped %>% dplyr::mutate(porcentaje = round(disease_data_grouped$casos/total_cases*100, 1))
    disease_data_grouped  <-  disease_data_grouped %>% dplyr::mutate(evento = event_name, etiqueta = event_label)
  }
  
  for (label in categorie_labels) {
    if (!any(disease_data_grouped == label)) {
      new_row <- data.frame(grupo_edad = label, casos = 0, porcentaje  = 0, evento = event_name, etiqueta = event_label)
      disease_data_grouped <- rbind(disease_data_grouped, new_row)
    }
  }
  
  return(disease_data_grouped)
}

get_distribution_by_age_group <- function(report_data, positive_value = "DETECTADO") {
  config_path <- "C:/Users/geral/Documents/TRACE/sivirep/inst/extdata/config.yml"
  column_names <- config::get(file = config_path, "respiratory_virus_column_names")
  names <- config::get(file = config_path, "respiratory_virus_names")
  viruses_by_age_group <- data.frame()
  
  i <- 1
  for (column in column_names) { 
    print(column)
    positive_cases <- report_data[eval(parse(text = paste0("report_data$", column, " == ", '"', positive_value, '"'))), ]
    positive_cases_by_age_group <- group_by_columns_and_cases_total(positive_cases, "grupo_edad", event_name = column,  wt_percentage = TRUE, total_cases = nrow(report_data), event_label = names[i])
    viruses_by_age_group <- rbind(viruses_by_age_group, positive_cases_by_age_group)
    i <- i + 1
  }
  
  return(viruses_by_age_group)
}

plot_distribution_by_age_group <- function(report_data) {
  plot <- ggplot2::ggplot(report_data) +
    ggplot2::geom_col(ggplot2::aes(x = grupo_edad, y = porcentaje, fill = etiqueta), alpha = 0.9) +
    ggplot2::theme_classic() +
    ggplot2::xlab("Grupo de edad") + ggplot2::ylab("Porcentaje de casos") +
    ggplot2::scale_fill_discrete(name = "Virus respiratorios") +
    ggplot2::theme(legend.position = "bottom")
  
  return(plot)
}


