# Generar gráfico de distribución de casos por edad y sexo

Función que genera un gráfico de distribución de casos por edad y sexo.

## Usage

``` r
plot_edad_sex(
  data_agrupada,
  col_edad = "edad",
  col_sex = "sexo",
  fuente_data = NULL
)
```

## Arguments

- data_agrupada:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento
  agrupados.

- col_edad:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene las edades en los datos agrupados de la enfermedad o
  evento; su valor por defecto es \`"edad\`.

- col_sex:

  Un \`character\` (cadena de caracteres) con el nombre de la columna
  que contiene el sexo en los datos agrupados de la enfermedad o evento;
  su valor por defecto es \`"sexo\`.

- fuente_data:

  Un \`character\` (cadena de caracteres) que contiene la leyenda o
  fuente de información de los datos; su valor por defecto es \`NULL\`.

## Value

Un \`plot\` o gráfico de distribución de casos por edad y sexo.

## Examples

``` r
# \donttest{
data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)
data_agrupada <- agrupar_edad_sex(data_event = data_limpia)
plot_edad_sex(
  data_agrupada = data_agrupada,
  col_edad = "edad",
  col_sex = "sexo"
)

# }
```
