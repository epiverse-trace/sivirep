#' @export
get_months_major_cases <- function(disease_data, col_dates, col_cases, top = 3, concat_values = T)  {
  data_major_cases <- disease_data[order(eval(parse(text = paste0("disease_data$", col_cases))), decreasing = TRUE), ]
  if (nrow(data_major_cases) < top) {
    top <- nrow(data_major_cases)
  }
  
  data_major_cases <- data_major_cases[1:top, ]
  
  data_major_cases$Meses <- sapply(eval(parse(text = paste0("data_major_cases$", col_dates))), months)
  if (concat_values) {
    months_concat <- concatenate_values_with_token(as.character(data_major_cases$Meses)[1:top])
    return(months_concat)
  }
  return(data_major_cases)
}

#' @export
concatenate_values_with_token <- function(values, length = 3, main_token = ", ", final_token = "y ") {
  final_value <- ""
  i <- 1
  for (value in values) {
     if (i != length) final_value <- paste0(final_value, value, main_token)
     else final_value <- paste0(final_value, final_token, value)
     i <- i + 1
  }
  return(final_value)
}