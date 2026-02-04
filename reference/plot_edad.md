# Generar gráfico de distribución de casos por edad

Función que genera un gráfico de distribución de casos por edad.

## Usage

``` r
plot_edad(data_agrupada, col_edad = "edad", fuente_data = NULL)
```

## Arguments

- data_agrupada:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento
  agrupados.

- col_edad:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene las edades en los datos agrupados de la enfermedad o
  evento; su valor por defecto es \`"edad"\`.

- fuente_data:

  Un \`character\` (cadena de caracteres) que contiene la leyenda o
  fuente de información de los datos; su valor por defecto es \`NULL\`.

## Value

Un \`plot\` o gráfico de distribución de casos por edad.

## Examples

``` r
# \donttest{
data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)
data_agrupada <- agrupar_edad(data_event = data_limpia)
plot_edad(
  data_agrupada = data_agrupada,
  col_edad = "edad"
)

# }
```
