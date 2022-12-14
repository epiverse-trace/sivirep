% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleaning_data.R
\name{clean_depto_disease_codes}
\alias{clean_depto_disease_codes}
\title{Clean Depto Disease Codes}
\usage{
clean_depto_disease_codes(depto_codes, disease_data, make_group = TRUE)
}
\arguments{
\item{depto_codes}{The department codes}

\item{disease_data}{The disease data}

\item{make_group}{Indicates if it is necessary to group by department IDs and case numbers}
}
\value{
Clean codes of disease data departments
}
\description{
Función que limpia la información de la enfermedad respecto a los codigos de los departamentos
Function that cleans the disease data with respect to the codes of the departments
}
\examples{
geo_codes <- import_geo_codes()
depto_codes <- get_depto_codes(geo_codes)
disease_data <-  import_data_disease_by_year(2019, "DENGUE")
disease_data <- group_by_columns_and_cases(disease_data, "COD_DPTO_O", wt_percentage = TRUE)
clean_depto_disease_codes(depto_codes, disease_data)
}
