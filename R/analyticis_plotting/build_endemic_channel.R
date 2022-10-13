define_quartiles <- function(years) {
  q_base <- ((years + 1)/4)
  quartiles <- C(q_base * 1, q_base * 2, q_base * 3)
  return(quartiles)
}

get_disease_quartiles <- function(disease_data_by_years) {
   
}

plot_endemic_channel <- function (df_quartiles) {
  df_reshaped <- data.frame(df_quartiles,                           
                            y = df_quartiles
                            )
  
  ggplot(df_reshaped, aes(x, y, col = group)) +  geom_line()
}

get_disease_years <- function(max) {
  year <- 2018
  years_endemic_chanel <- c((year - 4):year)
  
  disease_dt_by_week <- import_data_disease_by_year((year - 5), "DENGUE", use_cache = TRUE)  %>% group_by_param("SEMANA")
  names(disease_dt_by_week)[names(disease_dt_by_week) == "Casos"] <- as.character((year - 5))
  for (yr in years_endemic_chanel) {
    disease_dt_by_week_and_year <- import_data_disease_by_year(yr, "DENGUE", use_cache = TRUE) %>% group_by_param("SEMANA")
    disease_dt_by_week <- cbind(disease_dt_by_week, Casos = disease_dt_by_week_and_year$Casos)
    names(disease_dt_by_week)[names(disease_dt_by_week) == "Casos"] <- as.character(yr)
  }
  
  quantiles <- quantile(unlist(select(disease_dt_by_week, -1)), probs = c(0.25,0.5,0.75))
  q1  <- disease_dt_by_week[2,  826]
  q1  <- select(disease_dt_by_week, -1) %>% filter_all(any_vars(. < quantiles[1]))
  q2  <- select(disease_dt_by_week, -1) %>% filter_all(any_vars(. < quantiles[2]))
  q3  <- select(disease_dt_by_week, -1) %>% filter_all(any_vars(. < quantiles[3])) 
}