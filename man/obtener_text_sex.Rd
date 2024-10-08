% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{obtener_text_sex}
\alias{obtener_text_sex}
\title{Obtener el párrafo de la distribución de casos por sexo}
\usage{
obtener_text_sex(data_agrupada, year, figura)
}
\arguments{
\item{data_agrupada}{Un `data.frame` que contiene los datos
de la enfermedad o evento agrupados por sexo.}

\item{year}{Un `numeric` (numérico) con el año de los datos
agrupados por sexo.}

\item{figura}{Un `numeric` (numérico) con el número de la
figura de la distribución de casos por sexo.}
}
\value{
Un `character` (cadena de caracteres) con el párrafo descriptivo
de la distribución de casos por sexo.
}
\description{
Función que obtiene el párrafo descriptivo de la sección
de distribución de casos por sexo de la plantilla del reporte.
}
\examples{
data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)
data_agrupada <- agrupar_sex(
  data_event = data_limpia,
  porcentaje = TRUE
)
obtener_text_sex(data_agrupada, year = 2020, figura = 3)
}
