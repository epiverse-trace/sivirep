# Generar gráfico de distribución de casos por la clasificación inicial del caso

Función que genera un gráfico de distribución de casos según su
clasificación inicial.

## Usage

``` r
plot_tipo_caso(data_agrupada, col_tipo = "tip_cas", fuente_data = NULL)
```

## Arguments

- data_agrupada:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento
  agrupados según la clasificación inicial de los casos.

- col_tipo:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene la clasificación inicial de los casos en los datos
  agrupados de la enfermedad o evento; su valor por defecto es
  \`"tip_cas"\`.

- fuente_data:

  Un \`character\` (cadena de caracteres) que contiene la leyenda o
  fuente de información de los datos; su valor por defecto es \`NULL\`.

## Value

Un \`plot\` o gráfico de distribución de casos según su clasificación
inicial.

## Examples

``` r
# \donttest{
data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)
data_agrupada <- agrupar_tipo_caso(data_event = data_limpia)
plot_tipo_caso(data_agrupada,
  col_tipo = "tip_cas"
)

# }
```
