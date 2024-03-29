% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot_map}
\alias{plot_map}
\title{Función que genera el mapa por departamentos o municipios con el número de
casos de una enfermedad o evento}
\usage{
plot_map(
  data_agrupada,
  col_codigos = NULL,
  fuente_data = NULL,
  dpto = NULL,
  mpio = NULL
)
}
\arguments{
\item{data_agrupada}{Un `data.frame` que contiene los datos de la enfermedad
agrupados por departamento y número de casos}

\item{col_codigos}{Un `character` (cadena de caracteres) que contiene el
nombre de la columna para unir con el archivo de forma (shape file);
su valor por defecto `NULL`}

\item{fuente_data}{Un `character` (cadena de caracteres) que contiene la
leyenda o fuente de información de los datos de la enfermedad o evento;
su valor por defecto `NULL`}

\item{dpto}{Un `character` (cadena de caracteres) que contiene el
nombre del departamento; su valor por defecto `NULL`}

\item{mpio}{Un `character` (cadena de caracteres) que contiene el
nombre del municipio; su valor por defecto `NULL`}
}
\value{
Un `plot` o mapa por departamentos o municipios con el número de
casos de una enfermedad específica
}
\description{
Función que genera el mapa por departamentos o municipios con el número de
casos de una enfermedad o evento
}
\examples{
data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)
data_espacial_dpto <- estandarizar_geo_cods(data_limpia)
data_filtrada <- geo_filtro(data_event = data_espacial_dpto,
                            dpto = "Antioquia",
                            mpio = "Envigado")
data_espacial_dpto <- agrupar_mpio(data_event = data_filtrada,
                                   dpto = "Antioquia")
plot_map(data_agrupada = data_espacial_dpto,
   col_codigos = "cod_mun_o",
   fuente_data = "Fuente: SIVIGILA, Instituto Nacional de Salud, Colombia",
   dpto = "Antioquia",
   mpio = "Envigado")
}
