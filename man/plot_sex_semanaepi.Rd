% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot_sex_semanaepi}
\alias{plot_sex_semanaepi}
\title{Generar gráfico de distribución de casos por sexo y semana epidemiológica}
\usage{
plot_sex_semanaepi(
  data_agrupada,
  nomb_cols = c("sexo", "semana"),
  fuente_data = NULL
)
}
\arguments{
\item{data_agrupada}{Un `data.frame` que contiene los datos de la enfermedad
o evento agrupados}

\item{nomb_cols}{Un `array` (arreglo) de `character` (cadena de caracteres)
con los nombres de columna de los datos agrupados de la enfermedad o evento
que contienen el sexo y las semanas epidemiológicas; su valor por defecto es
`c("sexo", "semana")`}

\item{fuente_data}{Un `character` (cadena de caracteres) que contiene la
leyenda o fuente de información de los datos; su valor por defecto es `NULL`}
}
\value{
Un `plot` o gráfico de distribución de casos por sexo y semana
epidemiológica
}
\description{
Función que genera el gráfico de distribución de casos por sexo
y semana epidemiológica
}
\examples{
data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)
data_agrupada <- agrupar_sex_semanaepi(data_event = data_limpia,
                                       nomb_cols = c("sexo", "semana"),
                                       porcentaje = TRUE)
plot_sex_semanaepi(data_agrupada = data_agrupada,
                   nomb_cols = c("sexo", "semana"))
}
