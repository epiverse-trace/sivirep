# Generar gráfico de distribución de casos por fecha de inicio de síntomas

Función que genera un gráfico de distribución de casos por fecha de
inicio de síntomas.

## Usage

``` r
plot_fecha_inisintomas(
  data_agrupada,
  col_fecha = "ini_sin",
  uni_marca = "semanaepi",
  tipo = "barras",
  fuente_data = NULL
)
```

## Arguments

- data_agrupada:

  Un \`data.frame\` que contiene los datos de la enfermedad o evento
  agrupados.

- col_fecha:

  Un \`character\` (cadena de caracteres) que contiene el nombre de la
  columna con las fechas de notificación en los datos de la enfermedad o
  evento agrupados; su valor por defecto es \`"ini_sin"\`.

- uni_marca:

  Un \`character\` (cadena de caracteres) que contiene la unidad de las
  marcas del gráfico (\`"dia"\`, \`"semanaepi"\` y \`"mes"\`); su valor
  por defecto es \`"semanaepi"\`.

- tipo:

  Un \`character\` (cadena de caracteres) que contiene el tipo de
  gráfico (\`"barras"\` o \`"tendencia"\`); su valor por defecto es
  \`"barras"\`.

- fuente_data:

  Un \`character\` (cadena de caracteres) que contiene la leyenda o
  fuente de información de los datos; su valor por defecto es \`NULL\`.

## Value

Un \`plot\` o gráfico de la distribución de casos por fecha de inicio de
síntomas.

## Examples

``` r
# \donttest{
data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)
data_agrupada <- agrupar_fecha_inisintomas(
  data_event = data_limpia
)
plot_fecha_inisintomas(
  data_agrupada = data_agrupada,
  col_fecha = "ini_sin",
  uni_marca = "semanaepi"
)

# }
```
