% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cleaning_data.R
\name{remove_error_fecha}
\alias{remove_error_fecha}
\title{Eliminar fechas mayores que el valor de comparación}
\usage{
remove_error_fecha(data_event, col_ini = "ini_sin", col_comp = "fec_hos")
}
\arguments{
\item{data_event}{Un `data.frame` que contiene los datos de
una enfermedad o evento.}

\item{col_ini}{Un `character` (cadena de caracteres) que contiene
el nombre de la columna de la fecha inicial en los datos de una
enfermedad o evento; su valor por defecto es `"ini_sin"`.}

\item{col_comp}{Un `character` (cadena de caracteres) que contiene el
nombre de la columna de la fecha de comparación en los datos de una
enfermedad o evento; su valor por defecto es `"fec_hos`".}
}
\value{
Un `data.frame` con los datos sin las fechas mayores que el
valor de comparación.
}
\description{
Función que elimina fechas mayores que el valor de
comparación.
}
\keyword{internal}
