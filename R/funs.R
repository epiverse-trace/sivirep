

import_sivigila_data <- function() {
  link <- "https://www.datos.gov.co/api/views/qvnt-2igj/rows.csv?accessType=DOWNLOAD"
  data <- read.csv(link)
  
}


get_depto_codes <- function () {
  link_codes_deptos <- "https://www.datos.gov.co/api/views/gdxc-w37w/rows.csv?accessType=DOWNLOAD"
  codes <- read.csv(link_codes_deptos)
  codes <- codes %>% group_by(cod_dep = Código.Departamento, name_dep= Nombre.Departamento) %>%  
    select(cod_dep, name_dep) %>% distinct()
  codes <- codes [1:33,]
  
  return(codes)
  
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
  dat_plot <- dat %>% group_by(epiweek, Nombre) %>% dplyr::summarise(casos = sum(cases_count))
  
  plot <- ggplot(dat_plot) +
    geom_col(aes(x = epiweek, y = casos, fill = Nombre), alpha = 0.9) +
    theme_classic() +
    xlab ("Semana epidemiológica") + ylab ("Número de casos por semana") +
    scale_fill_discrete(name = "") +
    theme(legend.position = "bottom")
  
  if (type == "date") {
    
    dat_plot$date_week <- as.Date(paste(2019, dat_plot$epiweek, 1, sep="-"), "%Y-%U-%u")
    plot <- ggplot(dat_plot) +
      geom_col(aes(x = date_week, y = casos, fill = Nombre), alpha = 0.9) +
      theme_classic() +
      xlab ("Fecha de semana epidemiológica") + ylab ("Número de casos por semana") +
      scale_fill_discrete(name = "") +
      theme(legend.position = "bottom")
    
    
  }
  
  return(plot)
  
}


get_dept_map_sivigila <- function(dat) {
  
  dat$id <- as.character(dat$COD_DPTO_O)
  dat_map_depto <- dat %>% group_by(id) %>% dplyr::summarise(casos = sum(conteo_casos))
  dat_map_depto$id[dat_map_depto$id == "8"] <- "08"
  dat_map_depto$id[dat_map_depto$id == "5"] <- "05"
  dat_map_depto$id[dat_map_depto$id == "1"] <- "11"
  
  shp <- readOGR(dsn = file.path("../data/depto_adm_shp/depto.shp"), stringsAsFactors = FALSE)
  shp.df <- fortify(shp, region = "DPTO")
  shp.df <- shp.df %>% 
    left_join(dat_map_depto, by = "id")
  
  map <- ggplot() + 
    geom_polygon(data = shp.df, aes(x = long, y = lat, group = group, fill = casos), 
                 colour = "black") +
    scale_fill_gradient(low = "white", high="darkred")+
    theme_void()
  
  return(map)
}





