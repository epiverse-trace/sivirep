calculate_quartile <- function(df, percentage) {
  
  row <- c(df[2], df[3], df[4])
  quartile1 <- quantile(row, probs = c(percentage))
  
  return(quartile1)
}

get_disease_quartiles <- function(disease_data_by_years) {
  df_quartiles <- data.frame(SEMANA = disease_data_by_years$SEMANA)
  df_quartiles <- cbind(df_quartiles, quartile1 = apply(disease_data_by_years, 1, calculate_quartile, percentage = 0.25))
  df_quartiles <- cbind(df_quartiles, quartile2 = apply(disease_data_by_years, 1, calculate_quartile, percentage = 0.50))
  df_quartiles <- cbind(df_quartiles, quartile3 = apply(disease_data_by_years, 1, calculate_quartile, percentage = 0.75))
  df_quartiles <- cbind(df_quartiles, year = disease_data_by_years[,2])
  
  return(df_quartiles)
}

plot_endemic_channel <- function (df_quartiles) {
  df_reshaped <- data.frame(df_quartiles,                           
                            y = df_quartiles
                            )
  
  ggplot(df_reshaped, aes(x, y, col = group)) +  geom_line()
}