# Generar gráfico de distribución de casos por pertenencia étnica

Función que genera el gráfico de la distribución de casos por
pertenencia étnica.

## Usage

``` r
plot_per_etn(
  data_agrupada,
  col_etn = "per_etn",
  porcentaje = TRUE,
  fuente_data = NULL
)
```

## Arguments

- data_agrupada:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento
  agrupados por pertenencia étnica.

- col_etn:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene la pertenencia étnica en los datos agrupados de la
  enfermedad o evento; su valor por defecto es \`"per_etn"\`.

- porcentaje:

  Un \`logical\` (\`TRUE\` o \`FALSE\`) que indica si los datos tienen
  porcentajes; su valor por defecto es \`TRUE\`.

- fuente_data:

  Un \`character\` (cadena de caracteres) que contiene la leyenda o
  fuente de información de los datos; su valor por defecto es \`NULL\`.

## Value

Un \`plot\` o gráfico de la distribución de casos por pertenencia
étnica.

## Examples

``` r
# \donttest{
data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)
data_agrupada <- agrupar_per_etn(data_event = data_limpia)
plot_per_etn(data_agrupada,
  col_etn = "per_etn"
)

# }
```
