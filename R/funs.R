

import_sivigila_data <- function() {
  link <- "https://www.datos.gov.co/api/views/qvnt-2igj/rows.csv?accessType=DOWNLOAD"
  data <- read.csv(link)
  
}




#Funcion que seleccione la enfermedad por ejemplo Tuberculosis

select_disease <- function(name_disease, data) {
  lista_enfermedades <- unique(data$Nombre)
  lista_específica <- lista_enfermedades[stringr::str_detect(lista_enfermedades, name_disease) == TRUE]
  data2 <- data %>% filter(Nombre %in% lista_específica)
  return (data2)
  
}



plot_epiweek <- function(dat, var_week, var_cases, type = "week") {
  
  dat$epiweek <- dat[,var_week]
  dat$cases_count <- dat[,var_cases]
  dat_plot <- dat %>% group_by(epiweek) %>% dplyr::summarise(casos = sum(cases_count))

  plot <- ggplot(dat_plot) +
    geom_col(aes(x = epiweek, y = casos), fill = "pink", alpha = 0.9) +
    theme_classic() +
    xlab ("Semana epidemiológica") + ylab ("Número de casos")
  
  if (type == "date") {
   
    dat_plot$date_week <- as.Date(paste(2019, dat_plot$epiweek, 1, sep="-"), "%Y-%U-%u")
    plot <- ggplot(dat_plot) +
      geom_col(aes(x = date_week, y = casos), fill = "pink", alpha = 0.9) +
      theme_classic() +
      xlab ("Fecha de semana epidemiológica") + ylab ("Número de casos")
  }
  
  return(plot)
  
}






