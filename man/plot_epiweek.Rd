% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot_epiweek}
\alias{plot_epiweek}
\title{Plot Epiweek}
\usage{
plot_epiweek(
  dat,
  col_week,
  col_cases,
  year,
  type = "week",
  xlabel = "Semana epidemiológica",
  ylabel = "Número de casos por semana"
)
}
\arguments{
\item{dat}{Disease data}

\item{col_week}{Column Name that indicates the week in the data}

\item{col_cases}{Column Name hat indicates the cases number in the data}

\item{year}{Year of the data}

\item{type}{Type for axis x (week, date)}

\item{xlabel}{Label of axis x}

\item{ylabel}{Label of axis y}
}
\value{
The graphic of Epidemiological Week
}
\description{
Función que genera la gráfica de la Semana epidemiológica
Function that generates the graphic of the Epidemiological Week
}
\examples{
sivigila_summary_data <- import_sivigila_summary_data()
data <- filter_disease("MALAR", sivigila_summary_data)
}
