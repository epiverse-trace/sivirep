% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleaning_data.R
\name{parse_age_to_years}
\alias{parse_age_to_years}
\title{Parse Age to Years}
\usage{
parse_age_to_years(disease_data, col_age = "EDAD", col_uni_med = "UNI_MED")
}
\arguments{
\item{disease_data}{Disease data}

\item{col_age}{Age column name}

\item{col_uni_med}{Unit of measure column name}
}
\value{
The ages in years
}
\description{
Función que convierte las edades en años según las unidades de medida de SIVIGILA
Function that converts ages into years according to SIVIGILA measurement units
}
\examples{
disease_data <- import_data_disease_by_year(2019, "DENGUE")
parse_age_to_years(disease_data, col_age = "EDAD", col_uni_med = "UNI_MED")
}
