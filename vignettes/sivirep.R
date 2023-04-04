## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  include = TRUE,
  error = FALSE, 
  warning = FALSE, 
  message = FALSE
)

## ----setup--------------------------------------------------------------------
library(sivirep)

## -----------------------------------------------------------------------------
# install.packages("remotes")
# remotes::install_github("epiverse-trace/sivirep")   
library(sivirep)

## -----------------------------------------------------------------------------
list_of_diseases <- list_available_diseases_years()
knitr::kable(list_of_diseases)

## -----------------------------------------------------------------------------
library(sivirep)

## ----results = 'hide'---------------------------------------------------------
list_available_diseases_years()

## ----results = 'hide'---------------------------------------------------------
list_of_diseases <- list_available_diseases_years()

## -----------------------------------------------------------------------------
disease_data <-  import_linelist_disease_year(year = 2020, 
                                              disease_name = "dengue")

## -----------------------------------------------------------------------------
clean_disease_data <- cleansing_sivigila_data(disease_data, year = 2020)

## -----------------------------------------------------------------------------
cases_onset_symptoms_by_day <- group_onset_symptoms(disease_data = clean_disease_data, 
                                                    type = "day") 
cases_onset_symptoms_by_month <- group_onset_symptoms(disease_data = clean_disease_data, 
                                                      type = "month") 

## ----fig.height = 4, fig.width = 7--------------------------------------------
plot_onset_symptoms(data_grouped = cases_onset_symptoms_by_day,
                    break_tick_date = "months")

## -----------------------------------------------------------------------------
cases_notification_date_by_day <- group_notification_date(disease_data = clean_disease_data, 
                                                          type = "day") 
cases_notification_date_by_month <- group_notification_date(disease_data = clean_disease_data, 
                                                            type = "month") 

## ----fig.height = 4, fig.width = 7--------------------------------------------
plot_notification_date(data_grouped = cases_notification_date_by_day,
                       break_tick_date = "months")

## -----------------------------------------------------------------------------
cases_sex <- group_sex(disease_data = clean_disease_data, 
                       percentage = TRUE)

## ----fig.height = 4, fig.width = 7--------------------------------------------
plot_sex(data_grouped = cases_sex)

## -----------------------------------------------------------------------------
cases_sex_epiweek <- group_sex_epiweek(disease_data = clean_disease_data)

## ----fig.height = 4, fig.width = 7--------------------------------------------
plot_sex_epiweek(data_grouped = cases_sex_epiweek)

## -----------------------------------------------------------------------------
#cases_age <- group_age(disease_data = clean_disease_data, age_interval = 10)

## ----fig.height = 4, fig.width = 8--------------------------------------------
#plot_age(data_grouped = cases_age)

## -----------------------------------------------------------------------------
#cases_age_sex <- group_age_sex(disease_data = clean_disease_data, 
#                               age_interval = 10)


## ----fig.height = 3, fig.width = 7--------------------------------------------
#plot_age_sex(data_grouped = cases_age_sex)

## -----------------------------------------------------------------------------
spatial_dept_dist <- group_dept(disease_data = clean_disease_data)

