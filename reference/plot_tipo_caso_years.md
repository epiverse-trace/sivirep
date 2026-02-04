# Generar gráfico de distribución de casos por la clasificación inicial del caso y los años seleccionados

Función que genera un gráfico de distribución de casos según su
clasificación inicial y los años seleccionados.

## Usage

``` r
plot_tipo_caso_years(
  data_agrupada,
  col_tipo = "tip_cas",
  col_year = "ano",
  fuente_data = NULL
)
```

## Arguments

- data_agrupada:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento,
  agrupados por la clasificación inicial y los años seleccionados.

- col_tipo:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene la clasificación inicial del caso en los datos agrupados
  de la enfermedad o evento; su valor por defecto es \`"tip_cas"\`.

- col_year:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene el año en los datos agrupados de la enfermedad o evento;
  su valor por defecto es \`"ano"\`.

- fuente_data:

  Un \`character\` (cadena de caracteres) que contiene la leyenda o
  fuente de información de los datos; su valor por defecto es \`NULL\`.

## Value

Un \`plot\` o gráfico de distribución de casos según su clasificación
inicial y los años seleccionados.

## Examples

``` r
data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)
data_agrupada <- agrupar_tipo_caso(
  data_event = data_limpia,
  cols_tipo = c(
    "tip_cas",
    "ano"
  )
)
plot_tipo_caso_years(data_agrupada,
  col_tipo = "tip_cas",
  col_year = "ano"
)
```
