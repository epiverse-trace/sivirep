# Generar gráfico de distribución de casos por año

Función que genera un gráfico de distribución de casos por año.

## Usage

``` r
plot_years(data_agrupada, col_year = "ano", fuente_data = NULL)
```

## Arguments

- data_agrupada:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento
  agrupados por año.

- col_year:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene los años en los datos agrupados de la enfermedad o
  evento; su valor por defecto es \`"ano"\`.

- fuente_data:

  Un \`character\` (cadena de caracteres) que contiene la leyenda o
  fuente de información de los datos; su valor por defecto es \`NULL\`.

## Value

Un \`plot\` o gráfico de distribución de casos por año.

## Examples

``` r
# \donttest{
data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)
data_agrupada <- agrupar_years(data_event = data_limpia)
plot_years(data_agrupada,
  col_year = "ano"
)

if (interactive()) {
data_years <- import_data_event(
  nombre_event = "CHAGAS",
  years = c(2019, 2020))
data_limpia <- limpiar_data_sivigila(data_years)
data_agrupada <- agrupar_years(data_event = data_limpia)
plot_years(data_agrupada, col_year = "ano")
 }
# }
```
