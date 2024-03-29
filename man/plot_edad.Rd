% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot_edad}
\alias{plot_edad}
\title{Generar gráfico de distribución de casos por edad}
\usage{
plot_edad(data_agrupada, nomb_col = "edad", fuente_data = NULL)
}
\arguments{
\item{data_agrupada}{Un `data.frame` que contiene los datos de la enfermedad
o evento agrupados}

\item{nomb_col}{Un `character` (cadena de carácteres) con el nombre de
la columna de los datos agrupados de la enfermedad o evento que contiene
las edades; su valor por defecto es `"edad"`}

\item{fuente_data}{Un `character` (cadena de caracteres) que contiene la
leyenda o fuente de información de los datos; su valor por defecto es `NULL`}
}
\value{
Un `plot` o gráfico de distribución de casos por edad
}
\description{
Función que genera el gráfico de distribución de casos por edad
}
\examples{
data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)
data_agrupada <- agrupar_edad(data_event = data_limpia,
                              nomb_col = "edad",
                              porcentaje = FALSE)
plot_edad(data_agrupada = data_agrupada,
          nomb_col = "edad")
}
