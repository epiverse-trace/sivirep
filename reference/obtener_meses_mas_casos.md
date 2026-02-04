# Obtener los meses con mayor número de casos

Función que obtiene los meses con el mayor número de casos

## Usage

``` r
obtener_meses_mas_casos(
  data_event,
  col_fechas,
  col_casos = "casos",
  top = 1,
  concat_vals = TRUE
)
```

## Arguments

- data_event:

  Un \`data.frame\` con los datos de la enfermedad o evento.

- col_fechas:

  Un \`array\` (arreglo) de \`character\` (cadena de caracteres) con los
  nombres de las columnas que contienen las fechas en los datos de la
  enfermedad o evento.

- col_casos:

  Un \`character\` (cadena de caracteres) con el nombre de la columna de
  los datos de la enfermedad o evento que contiene el número de casos;
  su valor por defecto es \`"casos"\`.

- top:

  Un \`numeric\` (numérico) que contiene la cantidad máxima de meses a
  retornar; su valor por defecto es \`3\`.

- concat_vals:

  Un \`logical\` (\`TRUE\` o \`FALSE\`) que indica si se requiere
  concatenar los meses como una cadena; su valor por defecto es
  \`TRUE\`.

## Value

Un \`data.frame\` que contiene los meses con mayor número de casos.

## Examples

``` r
data(dengue2020)
data_limpia <- limpiar_data_sivigila(dengue2020)
casos_inisintomas <- agrupar_fecha_inisintomas(data_limpia)
obtener_meses_mas_casos(
  data_event = casos_inisintomas,
  col_fechas = "ini_sin",
  col_casos = "casos",
  top = 3,
  concat_vals = TRUE
)
#> [1] "enero"
```
