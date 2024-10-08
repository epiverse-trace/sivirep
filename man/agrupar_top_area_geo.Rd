% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checking_data.R
\name{agrupar_top_area_geo}
\alias{agrupar_top_area_geo}
\title{Agrupar por área geográfica a nivel departamental o municipal}
\usage{
agrupar_top_area_geo(
  data_event,
  dpto = NULL,
  col_area = "area",
  porcentaje = FALSE,
  top = 10
)
}
\arguments{
\item{data_event}{Un `data.frame` que contiene los datos de la
enfermedad o evento.}

\item{dpto}{Un `character` (cadena de caracteres) que contiene
el nombre del departamento; su valor por defecto es `NULL`. Si se ingresa
un valor en este parámetro se procederá agrupar los datos por los
municipios del departamento y sus áreas geográficas. Si no se
ingresa un valor en este parámetro validará si los datos ya están
filtrados por algún departamento; si no lo están generará la agrupación
por departamento.}

\item{col_area}{Un `character` (cadena de caracteres) con el nombre de
la columna que contiene las áreas geográficas en los datos de la enfermedad
o evento; su valor por defecto es `"cod_mun_o"`.}

\item{porcentaje}{Un `logical` (TRUE o FALSE) que indica si se debe
agregar una columna con el porcentaje de casos; su valor por
defecto es `FALSE`.}

\item{top}{Un `numeric` (numérico) que indica la cantidad de departamentos o
municipios con mayor número de casos que se deben retornar; su valor
por defecto es `10`.}
}
\value{
Un `data.frame` con el top 10 de los datos de la enfermedad o evento
agrupados por áreas geográficas y número de casos.
}
\description{
Función que agrupa los datos de una enfermedad o evento por
área geográfica a nivel departamental o municipal.
}
\examples{
data(dengue2020)
data_limpia <- limpiar_data_sivigila(data_event = dengue2020)
agrupar_top_area_geo(
  data_event = data_limpia,
  dpto = "Antioquia",
  col_area = "area",
  porcentaje = FALSE,
  top = 10
)
}
