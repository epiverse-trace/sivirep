#' Filter disease
#'
#' Function that filters by disease name in a dataset
#' @param name_disease Name of the disease
#' @param sivigila_summary_data The data set
#' @return Data filtered with the disease selected
#' @examples
#' sivigila_summary_data <- import_sivigila_summary_data()
#' filter_disease("MALAR", sivigila_summary_data)
#' @export
filter_disease <- function(name_disease, sivigila_summary_data) {
  if ("conteo_casos" %in% names(sivigila_summary_data)) {
    names(sivigila_summary_data)[names(sivigila_summary_data) == "conteo_casos"] <- "casos"
  }
  list_diseases <- unique(sivigila_summary_data$nombre)
  list_specific <- list_diseases[stringr::str_detect(list_diseases, name_disease) == TRUE]
  filtered_data <- sivigila_summary_data %>% dplyr::filter(.data$nombre %in% list_specific)
  return(filtered_data)
}

#' Get department codes
#'
#' Function that gets the list of departments of Colombia with their name and code
#' @param geo_codes Geographical codes (Colombia departments and municipalities)
#' @return The Departments data with code and name
#' @examples
#' geo_codes <- import_geo_codes()
#' get_depto_codes(geo_codes)
#' @export
get_depto_codes <- function(geo_codes) {
  deptos_data <- geo_codes %>%
    dplyr::group_by(cod_dep = .data$codigo_departamento, name_dep = .data$nombre_departamento) %>%
    dplyr::select(.data$cod_dep, .data$name_dep) %>%
    dplyr::distinct()
  deptos_data <- deptos_data[1:33, ]
  return(deptos_data)
}

#' Get special population and cases
#'
#' Function that gets the cases by special population type of a disease
#' @param disease_data The disease data
#' @return The cases by special population type of a disease
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' get_special_population_cases(disease_data)
#' @export
get_special_population_cases <- function(disease_data) {
  special_populations <- config::get(file = system.file("extdata", "config.yml", package = "sivirep"), "special_populations_cols")
  special_populations_names <- config::get(file = system.file("extdata", "config.yml", package = "sivirep"), "special_populations_names")
  special_cases <- c()
  for (sp in special_populations) {
    special_cases <- append(special_cases, sum(eval(parse(text = paste0("disease_data$", sp)))))
  }

  disease_data_special_population <- data.frame(poblacion = special_populations, casos = special_cases, nombre = special_populations_names)
  return(disease_data_special_population)
}

#' Group By week and cases
#'
#' Function that groups the disease data by epidemiological week and cases number
#' @param disease_data The disease data
#' @return The disease data grouped by epidemiological week and cases number
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' group_epiweek_cases(disease_data)
#' @export
group_epiweek_cases <- function(disease_data) {
  disease_data_grouped <- disease_data %>%
    dplyr::group_by(.data$semana) %>%
    dplyr::summarise(casos = sum(.data$uni_med))
  disease_data_grouped <- disease_data_grouped[1:52, ]
  return(disease_data_grouped)
}

#' Group by columns and cases
#'
#' Function that groups the disease data by a specific column name(s) and cases number
#' @param disease_data The disease data
#' @param col_names The column name(s)
#' @param wt_percentage Indicates if it is required to add a percentage of cases as a column
#' @return The disease data grouped by a specific column name(s) and cases number
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' group_columns_cases(disease_data, col_names = "sexo", wt_percentage = TRUE)
#' group_columns_cases(disease_data, col_names = c("sexo", "semana"))
#' @export
group_columns_cases <- function(disease_data, col_names, wt_percentage = FALSE) {
  disease_data_grouped <- disease_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(col_names))) %>%
    dplyr::summarise(casos = dplyr::n(), .groups = "drop")
  if (wt_percentage) {
    disease_data_grouped <- disease_data_grouped %>% dplyr::mutate(porcentaje = round(disease_data_grouped$casos / sum(disease_data_grouped$casos) * 100, 1))
  }
  return(disease_data_grouped)
}

#' Group by age range and cases
#'
#' Function that groups the disease data by age range and cases number
#' @param disease_data The disease data
#' @param col_name Column name in the disease data that contains the ages
#' @param min_val  Minimum value of ages
#' @param max_val  Maximum value of ages
#' @param step Step to generate the age range
#' @return The disease data grouped by the age range and cases number
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' disease_dt_by_age <- group_columns_cases(disease_data, c("edad", "semana"),
#'   wt_percentage = TRUE
#' )
#' group_age_range_cases(disease_dt_by_age, "edad",
#'   min_val = 0,
#'   max_val = max(disease_dt_by_age$edad), step = 10
#' )
#' @export
group_age_range_cases <- function(disease_data, col_name, var_a = NULL, min_val, max_val, step) {
  data_values_range <- data.frame()
  if (!is.null(var_a) & length(var_a) > 0) {
    data_values_range <- disease_data %>%
      dplyr::mutate(ranges = cut(
        .data$edad,
        seq(min_val, max_val, step)
      )) %>%
      dplyr::group_by_("ranges", var_a) %>%
      dplyr::summarize(casos = sum(.data$casos), .groups = "drop") %>%
      as.data.frame()
    names(data_values_range)[names(data_values_range) == "ranges"] <- col_name
  } else {
    data_values_range <- disease_data %>%
      dplyr::mutate(ranges = cut(
        .data$edad,
        seq(min_val, max_val, step)
      )) %>%
      dplyr::group_by_("ranges") %>%
      dplyr::summarize(casos = sum(.data$casos), .groups = "drop") %>%
      as.data.frame()
    names(data_values_range)[names(data_values_range) == "ranges"] <- col_name
  }
  return(data_values_range)
}

#' Group by columns and cases
#'
#' Function that groups the disease data by a specific column name(s) and cases
#' @param disease_data The disease data
#' @param col_names The column name(s)
#' @param wt_percentage Indicates if it is required to add a percentage of cases as a column
#' @return The disease data grouped by a specific column name(s) and cases number
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' group_columns_cases(disease_data, col_names = "sexo", wt_percentage = TRUE)
#' group_columns_cases(disease_data, col_names = c("sexo", "semana"))
#' @export
group_columns_cases <- function(disease_data, col_names, wt_percentage = FALSE) {
  disease_data_grouped <- disease_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(col_names))) %>%
    dplyr::summarise(casos = dplyr::n(), .groups = "drop")
  if (wt_percentage) {
    disease_data_grouped <- disease_data_grouped %>% dplyr::mutate(porcentaje = round(disease_data_grouped$casos / sum(disease_data_grouped$casos) * 100, 1))
  }
  return(disease_data_grouped)
}

#' Group by symptom onset date and cases
#'
#' Function that groups the disease data by symptom onset date and number of cases
#' @param disease_data The disease data
#' @param col_name Column name in the disease data that contains the symptom onset dates
#' @param type Time unit (day, month and year)
#' @return The disease data grouped by symptom onset date and number of cases
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' group_onset_symptoms(disease_data, col_name = "ini_sin", type = "month")
#' @export
group_onset_symptoms <- function(disease_data, col_name = "ini_sin", type = "month") {
  dates_column_names <- config::get(
    file =
      system.file("extdata", "config.yml",
        package = "sivirep"
      ), "dates_column_names"
  )
  if (is.null(col_name)) {
    col_name <- dates_column_names[3]
  }
  group_by_onset_symp <- group_columns_cases(disease_data, col_names = col_name)
  # disease_data_by_onset_sym <- disease_data %>%
  # dplyr::group_by(date = lubridate::floor_date(eval(parse(text = col_name)), type)) %>%
  # dplyr::summarize(casos = sum(casos))
  # colnames(disease_data_by_onset_sym)[colnames(disease_data_by_onset_sym) == "date" ] <- col_name

  return(group_by_onset_symp)
}

#' Group by notification date and cases
#'
#' Function that groups the disease data by notification date and cases number
#' @param disease_data The disease data
#' @param col_name Column name in the disease data that contains the notification dates
#' @param type Time unit (day, month and year)
#' @return The disease data grouped by notification date and cases
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' group_notification_date(disease_data, col_name = "fec_not", type = "month")
#' @export
group_notification_date <- function(disease_data, col_name = "fec_not", type = "month") {
  dates_column_names <- config::get(
    file =
      system.file("extdata", "config.yml",
        package = "sivirep"
      ), "dates_column_names"
  )
  if (is.null(col_name)) {
    col_name <- dates_column_names[2]
  }
  group_by_onset_symp <- group_columns_cases(disease_data, col_names = col_name)

  return(group_by_onset_symp)
}

#' Group by sex and cases
#'
#' Function that groups the disease data by sex and cases number
#' @param disease_data The disease data
#' @param col_name Column name in the disease data that contains the sex
#' @param percentage Indicates if it is required to add a percentage of cases as a column
#' @return The disease data grouped by sex and cases number
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' group_sex(disease_data, col_name = "sexo", percentage = TRUE)
#' @export
group_sex <- function(disease_data, col_name = "sexo", percentage = TRUE) {
  disease_data_by_sex <- group_columns_cases(disease_data, col_name, percentage)
  return(disease_data_by_sex)
}

#' Group by sex, epidemiological week and cases
#'
#' Function that groups the disease data by sex, epidemiological week and cases number
#' @param disease_data The disease data
#' @param col_names Column names in the disease data that contains the sex and the epidemiological weeks
#' @param percentage Indicates if it is required to add a percentage of cases as a column
#' @return The disease data grouped by sex, epidemiological week and cases number
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' group_sex_epiweek(disease_data, col_names = c("sexo", "semana"), percentage = TRUE)
#' @export
group_sex_epiweek <- function(disease_data, col_names = c("sexo", "semana"), percentage = TRUE) {
  disease_data_by_sex_and_week <- group_columns_cases(disease_data, col_names, percentage)
  return(disease_data_by_sex_and_week)
}

#' Group by age and cases
#'
#' Function that groups the disease data by age and cases
#' @param disease_data The disease data
#' @param col_name Column name in the disease data that contains the ages
#' @param percentage Indicates if it is required to add a percentage of cases as a column
#' @param age_interval The interval of age range
#' @return The disease data grouped by age and cases
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' group_age(disease_data, col_name = "edad", percentage = FALSE)
#' @export
group_age <- function(disease_data, col_name = "edad", percentage = FALSE, age_interval = 10) {
  disease_data_by_age <- group_columns_cases(disease_data, col_name, percentage)
  disease_data_by_age <- group_age_range_cases(disease_data_by_age,
    col_name,
    min_val = 0,
    max_val = max(eval(parse(
      text = paste0(
        "disease_data_by_age$",
        col_name
      )
    ))), step = age_interval
  )
  return(disease_data_by_age)
}

#' Group by ages, sex and cases
#'
#' Function that groups the data by age, sex and cases number
#' @param disease_data The disease data
#' @param col_names Column names in the disease data that contains the ages and sex
#' @param percentage Indicates if it is required to add a percentage of cases as a column
#' @param age_interval The interval of age range
#' @return The disease data grouped by ages, sex and number cases
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' group_age_sex(disease_data, col_names = c("edad", "sexo"), percentage = TRUE)
#' @export
group_age_sex <- function(disease_data, col_names = c("edad", "sexo"), percentage = TRUE, age_interval = 10) {
  disease_data_by_age_and_sex <- group_columns_cases(disease_data, col_names, percentage)
  disease_data_by_age_and_sex <- group_age_range_cases(disease_data_by_age_and_sex,
    col_names[1],
    col_names[2],
    min_val = 0,
    max_val =
      max(eval(parse(
        text = paste0(
          "disease_data_by_age_and_sex$",
          col_names[1]
        )
      ))),
    age_interval = 10
  )
  return(disease_data_by_age_and_sex)
}

#' Group by special population and cases
#'
#' Function that groups the disease data by special population and cases
#' @param disease_data The disease data
#' @param col_name Column name in the disease data that contains the special populations
#' @param percentage Indicates if it is required to add a percentage of cases as a column
#' @return The disease data grouped by special populations and cases
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' group_special_population(disease_data, col_name = "poblacion", percentage = TRUE)
#' @export
group_special_population <- function(disease_data, col_name = "poblacion", percentage = TRUE) {
  disease_data_special <- get_special_population_cases(disease_data)
  disease_data_special_grouped <- data.frame(poblacion = disease_data_special$poblacion, casos = disease_data_special$casos)
  return(disease_data_special_grouped)
}

#' Group by department and cases
#'
#' Function that groups the data by code departments and cases number
#' @param disease_data The disease data
#' @param col_name Column name in the disease data that contains the department codes
#' @param percentage Indicates if it is required to add a percentage of cases as a column
#' @return The disease data grouped by department codes and cases number
#' @examples
#' disease_data <- import_data_disease_by_year(2019, "DENGUE")
#' group_dept(disease_data, col_name = "cod_dpto_o", percentage = FALSE)
#' @export
group_dept <- function(disease_data, col_name = "cod_dpto_o", percentage = FALSE) {
  disease_data_by_depto_codes <- group_columns_cases(disease_data, col_names = col_name)
  colnames(disease_data_by_depto_codes)[colnames(disease_data_by_depto_codes) == col_name] <- "id"

  disease_data_by_depto_codes$id <- sapply(disease_data_by_depto_codes$id, as.character)
  return(disease_data_by_depto_codes)
}