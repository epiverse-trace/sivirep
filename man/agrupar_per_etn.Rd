% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checking_data.R
\name{agrupar_per_etn}
\alias{agrupar_per_etn}
\title{Agrupar por la pertenencia etnica}
\usage{
agrupar_per_etn(data_event, nomb_cols = "per_etn")
}
\arguments{
\item{data_event}{Un `data.frame` que contiene los datos de la
enfermedad o evento}

\item{nomb_cols}{Un `character` (cadena de caracteres) con el nombre de
las columna(s) en los datos de la enfermedad o evento que contiene la
pertenencia etnica; su valor por defecto es `"per_etn"`}
}
\value{
Un `data.frame` con los datos de la enfermedad o evento agrupados
por la pertenencia etnica
}
\description{
Función que agrupa los casos por la pertenencia etnica
}
\examples{
data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
agrupar_per_etn(data_event = data_limpia,
                nomb_cols = "per_etn")
}
