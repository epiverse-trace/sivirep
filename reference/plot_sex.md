# Generar gráfico de distribución de casos por sexo

Función que genera un gráfico de distribución de casos por sexo.

## Usage

``` r
plot_sex(
  data_agrupada,
  col_sex = "sexo",
  col_distribucion = "casos",
  porcentaje = TRUE,
  fuente_data = NULL
)
```

## Arguments

- data_agrupada:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento
  agrupados.

- col_sex:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene el sexo en los datos agrupados de la enfermedad o evento;
  su valor por defecto es \`"sexo"\`.

- col_distribucion:

  Un \`character\` (cadena de caracteres) que contiene el nombre de la
  columna que tiene los valores de la distribución, por número de casos
  o incidencia; su valor por defecto es \`"incidencia"\`.

- porcentaje:

  Un \`logical\` (\`TRUE\` o \`FALSE\`) que indica si los datos tienen
  porcentajes; su valor por defecto es \`TRUE\`.

- fuente_data:

  Un \`character\` (cadena de caracteres) que contiene la leyenda o
  fuente de información de los datos; su valor por defecto es \`NULL\`.

## Value

Un \`plot\` o gráfico de distribución de casos por sexo.

## Examples

``` r
# \donttest{
data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)
data_agrupada <- agrupar_sex(
  data_event = data_limpia,
  porcentaje = TRUE
)
plot_sex(
  data_agrupada = data_agrupada,
  col_sex = "sexo",
  porcentaje = TRUE
)

# }
```
