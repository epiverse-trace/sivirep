# Generar gráfico de distribución de casos por área geográfica

Función que genera el gráfico de casos por área geográfica.

## Usage

``` r
plot_area_geo(data_agrupada, col_area = "area", fuente_data = NULL)
```

## Arguments

- data_agrupada:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento
  agrupados.

- col_area:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  con el área geográfica en los datos agrupados de la enfermedad o
  evento; su valor por defecto es \`"area"\`.

- fuente_data:

  Un \`character\` (cadena de caracteres) que contiene la leyenda o
  fuente de información de los datos; su valor por defecto es \`NULL\`.

## Value

Un \`plot\` o gráfico de distribución de casos por área geográfica.

## Examples

``` r
# \donttest{
data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)
data_agrupada <- agrupar_area_geo(data_event = data_limpia)
plot_area_geo(data_agrupada,
  col_area = "area"
)

# }
```
