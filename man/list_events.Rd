% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_data.R
\name{list_events}
\alias{list_events}
\title{Importar enfermedades y años disponibles para
su descarga desde los microdatos del SIVIGILA}
\usage{
list_events()
}
\value{
Una `list` con las enfermedades y los años disponibles
para su descarga desde los microdatos del SIVIGILA.
}
\description{
Función que obtiene las enfermedades y los años
disponibles para su descarga desde los microdatos del SIVIGILA.

Esta función requiere acceso a Internet.
}
\examples{
\donttest{
if (interactive()) {
  list_events()
 }
}
}
