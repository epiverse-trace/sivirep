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

