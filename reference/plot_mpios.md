# Generar gráfico de distribución de casos por municipios

Función que genera un gráfico de distribución de casos por municipios.

## Usage

``` r
plot_mpios(data_agrupada, col_mpios = NULL, fuente_data = NULL)
```

## Arguments

- data_agrupada:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento
  agrupados por municipios.

- col_mpios:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene los municipios en los datos agrupados de la enfermedad o
  evento; su valor por defecto es \`NULL\`.

- fuente_data:

  Un \`character\` (cadena de caracteres) que contiene la leyenda o
  fuente de información de los datos; su valor por defecto es \`NULL\`.

## Value

Un \`plot\` o gráfico de distribución de casos por municipios.

## Examples

``` r
# \donttest{
data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)
data_limpia <- estandarizar_geo_cods(data_limpia)
data_agrupada <- agrupar_mpio(
  data_event = data_limpia,
  dpto = "Antioquia"
)
plot_mpios(data_agrupada,
  col_mpios = "municipio_ocurrencia"
)

# }
```
