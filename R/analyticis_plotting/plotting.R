#' Función que genera el reporte de la Semana epidemiológica
#' Function that generates the report of the Epidemiological Week
#' @param data Disease data
#' @param var_week  Name of variable that indicates the week in the data
#' @param var_cases Name of variable that indicates the cases number in the data
#' @param year The year of the data
#' @param type The type (week, date)
#' @param xlabel The label of x axis
#' @param ylabel The label of y axis
#' @return The chart of Epidemiological Week
#' @examples
#' plot_epiweek(data, var_week = "SEMANA", var_cases = "conteo_casos", year = 2019, type = "date", xlabel = "Fecha de semana epidemiológica")
plot_epiweek <- function(dat, var_week, var_cases, year, type = "week", xlabel = "Semana epidemiológica", ylabel = "Número de casos por semana") {
  dat$epiweek <- dat[,var_week]
  dat$cases_count <- dat[,var_cases]
  dat_plot <- dat %>% group_by(epiweek, Nombre) %>% dplyr::summarise(casos = sum(cases_count))
  
  if (type == "week") {
      plot <- ggplot(dat_plot) +
        geom_col(aes(x = epiweek, y = casos, fill = Nombre), alpha = 0.9) +
        theme_classic() +
        xlab (xlabel) + ylab (ylabel) +
        scale_fill_discrete(name = "") +
        theme(legend.position = "bottom")
  }
  
  if (type == "date") {
    
    dat_plot$date_week <- as.Date(paste(year, dat_plot$epiweek, 1, sep = "-"), "%Y-%U-%u")
    plot <- ggplot(dat_plot) +
      geom_col(aes(x = date_week, y = casos, fill = Nombre), alpha = 0.9) +
      theme_classic() +
      xlab (xlabel) + ylab (ylabel) +
      scale_fill_discrete(name = "") +
      theme(legend.position = "bottom")
  }
  
  return(plot)
}

#' Función que genera el mapa por departamento de la información de la enfermedad seleccionada
#' Function that generates the map by information department of the selected disease
#' @param data_map_depto Data for department
#' @return The map for department
#' @examples
#' generate_dept_map_sivigila(data_map_disease_deptos, var_lj = "id")
generate_dept_map_sivigila <- function(data_map_depto, var_lj = "id") {
  
  shp <- readOGR(dsn = file.path("../data/depto_adm_shp/depto.shp"), stringsAsFactors = FALSE)
  shp.df <- fortify(shp, region = "DPTO")
  shp.df <- shp.df %>% 
    left_join(data_map_depto, by = var_lj)
  
  map <- ggplot() + 
    geom_polygon(data = shp.df, aes(x = long, y = lat, group = group, fill = casos), 
                 colour = "black") +
    scale_fill_gradient(low = "white", high="darkred")+
    theme_void()
  
  return(map)
}