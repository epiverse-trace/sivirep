% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{obtener_nombre_mpio}
\alias{obtener_nombre_mpio}
\title{Obtener el nombre de un municipio de Colombia}
\usage{
obtener_nombre_mpio(data_geo, cod_dpto, cod_mpio)
}
\arguments{
\item{data_geo}{Un `data.frame` que contiene los códigos geográficos
(departamentos y municipios de Colombia).}

\item{cod_dpto}{Un `numeric` (numérico) o `character` (cadena de caracteres)
que contiene el código del departamento.}

\item{cod_mpio}{Un `numeric` (numérico) o `character`
(cadena de caracteres) que contiene el código del municipio.}
}
\value{
Un `character` (cadena de caracteres) con el nombre del municipio.
}
\description{
Función que obtiene el nombre de un municipio de Colombia a
partir de su código geográfico.
}
\examples{
data_geo <- import_geo_cods()
obtener_nombre_mpio(data_geo,
  cod_dpto = "05",
  cod_mpio = "001"
)
obtener_nombre_mpio(data_geo,
  cod_dpto = 05,
  cod_mpio = 001
)
obtener_nombre_mpio(data_geo,
  cod_dpto = 5,
  cod_mpio = 1
)
}
